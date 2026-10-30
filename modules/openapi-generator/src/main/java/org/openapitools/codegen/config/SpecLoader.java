package org.openapitools.codegen.config;

import io.swagger.v3.parser.core.models.AuthorizationValue;
import io.swagger.v3.parser.core.models.ParseOptions;
import io.swagger.v3.parser.core.models.SwaggerParseResult;

import java.util.List;

/**
 * Loads an OpenAPI document from an input spec location into a
 * {@link SwaggerParseResult}. See
 * <a href="https://github.com/OpenAPITools/openapi-generator/issues/844">issue #844</a>
 * ("configurable spec loading/parsing": putting spec loading behind a
 * generic interface users can extend).
 */
public interface SpecLoader {

    /**
     * Parses the document at {@code inputSpec}.
     *
     * @param inputSpec            path or URL of the OpenAPI document
     * @param authorizationValues  authorization for remote locations, may be null
     * @param options              parser options, may be null
     * @return the parse result
     */
    SwaggerParseResult load(String inputSpec, List<AuthorizationValue> authorizationValues,
                            ParseOptions options);

    /**
     * Returns the loader for documents at {@code inputSpec}-like locations.
     * The returned loader behaves exactly like calling
     * {@link io.swagger.parser.OpenAPIParser#readLocation(String, List, ParseOptions)}
     * for OpenAPI 3.0/3.1 documents (a single parser read); only when that read
     * fails with the parser's "unsupported {@code openapi} version" signature
     * does it transparently retry through the 3.2 compatibility loader
     * ({@link Oas32CompatSpecLoader}), so deciding up front whether a location
     * holds a 3.2 document is not needed.
     */
    static SpecLoader forInputSpec(String inputSpec) {
        return new DefaultSpecLoader();
    }
}
