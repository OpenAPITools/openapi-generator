package org.openapitools.codegen.config;

import com.fasterxml.jackson.databind.JsonNode;
import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.parser.core.models.AuthorizationValue;
import io.swagger.v3.parser.core.models.ParseOptions;
import io.swagger.v3.parser.core.models.SwaggerParseResult;
import org.openapitools.codegen.utils.SpecUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

/**
 * The {@link SpecLoader} used for every input: delegates to
 * {@link OpenAPIParser#readLocation(String, List, ParseOptions)} exactly as
 * the generator always has.
 *
 * <p>There is one narrow retry: when the parser returns no document and its
 * messages carry the released parser's signature for an unsupported
 * {@code openapi} version ({@code attribute openapi is unexpected}, emitted
 * for {@code openapi: 3.2.x} input by swagger-parser 2.1.48), the raw document
 * is re-read with the caller's authorization values; if it really declares
 * 3.2, parsing is retried through {@link Oas32CompatSpecLoader}. 3.0/3.1
 * documents never reach that branch, so their behaviour (a single parser
 * read) is unchanged.
 */
class DefaultSpecLoader implements SpecLoader {

    private static final Logger LOGGER = LoggerFactory.getLogger(DefaultSpecLoader.class);

    /**
     * Message swagger-parser emits when the document is readable but its
     * {@code openapi} version is not one the released schema accepts.
     */
    private static final String UNEXPECTED_OPENAPI_VERSION_MESSAGE =
            "attribute openapi is unexpected";

    @Override
    public SwaggerParseResult load(String inputSpec, List<AuthorizationValue> authorizationValues,
                                   ParseOptions options) {
        SwaggerParseResult result =
                new OpenAPIParser().readLocation(inputSpec, authorizationValues, options);
        if (result.getOpenAPI() != null
                || !hasUnexpectedVersionMessage(result.getMessages())) {
            return result;
        }
        JsonNode rawDocument;
        try {
            rawDocument = SpecUtils.readRawDocument(inputSpec, authorizationValues);
        } catch (Exception e) {
            // The raw document cannot be read either; the parser's own result
            // remains the correct answer.
            return result;
        }
        if (!Oas32CompatSpecLoader.isOas32Document(rawDocument)) {
            return result;
        }
        SwaggerParseResult compatResult = new Oas32CompatSpecLoader(rawDocument)
                .load(inputSpec, authorizationValues, options);
        if (compatResult.getOpenAPI() != null) {
            // The first readLocation above already ran the parser's Swagger 2
            // converter fallback, which logs ERROR stacks for YAML it cannot
            // map onto Swagger 2 — expected noise for a 3.2 input.
            LOGGER.info("the ERROR messages from the Swagger 2 converter above "
                    + "are expected for an OpenAPI 3.2 input document: the "
                    + "released parser cannot read 3.2, so it was re-read "
                    + "through the 3.2 compatibility loader");
        }
        return compatResult;
    }

    private static boolean hasUnexpectedVersionMessage(List<String> messages) {
        if (messages == null) {
            return false;
        }
        for (String message : messages) {
            if (message != null && message.contains(UNEXPECTED_OPENAPI_VERSION_MESSAGE)) {
                return true;
            }
        }
        return false;
    }
}
