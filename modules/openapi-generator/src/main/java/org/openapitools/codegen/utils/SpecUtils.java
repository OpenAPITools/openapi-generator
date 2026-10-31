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
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;

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
        boolean windowsDrivePath = inputSpec.matches("^[A-Za-z]:[\\\\/].*");
        if (!windowsDrivePath) {
            try {
                URI uri = URI.create(inputSpec);
                if (uri.getScheme() != null) {
                    if ("http".equalsIgnoreCase(uri.getScheme())
                            || "https".equalsIgnoreCase(uri.getScheme())) {
                        return RemoteUrl.urlToString(inputSpec, authorizationValues);
                    }
                    try (InputStream input = uri.toURL().openStream()) {
                        return new String(input.readAllBytes(), StandardCharsets.UTF_8);
                    }
                }
            } catch (IllegalArgumentException ignored) {
                // A local path may contain URI-reserved characters such as spaces.
            }
        }
        return Files.readString(Paths.get(inputSpec), StandardCharsets.UTF_8);
    }
}
