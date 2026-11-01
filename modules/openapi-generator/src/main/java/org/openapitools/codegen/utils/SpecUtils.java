package org.openapitools.codegen.utils;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.cfg.JsonNodeFeature;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import com.fasterxml.jackson.dataformat.yaml.YAMLMapper;
import io.swagger.v3.parser.core.models.AuthorizationValue;
import io.swagger.v3.parser.util.RemoteUrl;

import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.Locale;

/**
 * Reads an OpenAPI document from an input spec location (path or URL) into
 * raw text or a Jackson tree, before swagger-parser interprets it.
 */
public final class SpecUtils {

    private SpecUtils() {
    }

    /** Reads the input document with swagger-parser's configured YAML limits. */
    public static JsonNode readRawDocument(String inputSpec) throws Exception {
        return readRawDocument(inputSpec, null);
    }

    /**
     * Reads the input document with swagger-parser's configured YAML limits,
     * using {@code authorizationValues} for remote locations exactly like the
     * parser does.
     */
    public static JsonNode readRawDocument(String inputSpec,
                                           List<AuthorizationValue> authorizationValues)
            throws Exception {
        String text = readInputSpec(inputSpec, authorizationValues);
        YAMLFactory yamlFactory = YAMLFactory.builder()
                .loaderOptions(io.swagger.v3.parser.util.DeserializationUtils
                        .buildLoaderOptions())
                .build();
        ObjectMapper mapper = YAMLMapper.builder(yamlFactory)
                .enable(DeserializationFeature.USE_BIG_DECIMAL_FOR_FLOATS)
                .enable(DeserializationFeature.USE_BIG_INTEGER_FOR_INTS)
                .disable(JsonNodeFeature.STRIP_TRAILING_BIGDECIMAL_ZEROES)
                .build();
        return mapper.readTree(text);
    }

    public static String readInputSpec(String inputSpec) throws Exception {
        return readInputSpec(inputSpec, null);
    }

    /**
     * Reads the raw text of {@code inputSpec}. HTTP(S) locations are fetched
     * through {@link RemoteUrl#urlToString(String, List)}, so authorization
     * headers, the connect/read timeouts and redirect handling are identical
     * to the parser's own reads. Any other URI scheme ({@code file:},
     * {@code jar:}, a custom protocol handler) is read through
     * {@code URL.openStream()} — RemoteUrl casts the connection to
     * {@code HttpURLConnection}, which only works for HTTP.
     */
    public static String readInputSpec(String inputSpec,
                                       List<AuthorizationValue> authorizationValues)
            throws Exception {
        // http(s) is dispatched by prefix, before URI parsing: RemoteUrl is
        // the parser's own reader and accepts inputs (e.g. containing spaces)
        // that URI.create would reject — they must not fall through to the
        // local-file read.
        String lowerCased = inputSpec.toLowerCase(Locale.ROOT);
        if (lowerCased.startsWith("http://") || lowerCased.startsWith("https://")) {
            return RemoteUrl.urlToString(inputSpec, authorizationValues);
        }
        boolean windowsDrivePath = inputSpec.matches("^[A-Za-z]:[\\\\/].*");
        if (!windowsDrivePath) {
            try {
                URI uri = URI.create(inputSpec);
                if (uri.getScheme() != null) {
                    // A "scheme" the JVM has no protocol handler for (a local
                    // file named "a:b.yaml" looks like one) fails in toURL()
                    // with MalformedURLException — that input is a file name,
                    // so it falls through to the local read.
                    try {
                        URL url = uri.toURL();
                        try (InputStream input = url.openStream()) {
                            return new String(input.readAllBytes(), StandardCharsets.UTF_8);
                        }
                    } catch (MalformedURLException ignored) {
                    }
                }
            } catch (IllegalArgumentException ignored) {
                // A local path may contain URI-reserved characters such as spaces.
            }
        }
        return Files.readString(Paths.get(inputSpec), StandardCharsets.UTF_8);
    }
}
