package org.openapitools.codegen.utils;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.examples.Example;
import io.swagger.v3.oas.models.media.Content;
import io.swagger.v3.oas.models.media.MediaType;
import io.swagger.v3.oas.models.responses.ApiResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;

import static org.openapitools.codegen.utils.OnceLogger.once;

public class ExamplesUtils {
    private static final Logger LOGGER = LoggerFactory.getLogger(ExamplesUtils.class);

    /**
     * Return examples of API response.
     *
     * @param openAPI OpenAPI spec.
     * @param response ApiResponse of the operation
     * @return examples of API response
     */
    public static Map<String, Example> getExamplesFromResponse(OpenAPI openAPI, ApiResponse response) {
        ApiResponse result = ModelUtils.getReferencedApiResponse(openAPI, response);
        if (result == null) {
            return Collections.emptyMap();
        } else {
            return getExamplesFromContent(result.getContent());
        }
    }

    private static Map<String, Example> getExamplesFromContent(Content content) {
        if (content == null || content.isEmpty()) {
            return Collections.emptyMap();
        }
        Map.Entry<String, MediaType> entry = content.entrySet().iterator().next();
        if (content.size() > 1) {
            once(LOGGER).debug("Multiple API response examples found in the OAS 'content' section, returning only the first one ({})",
                    entry.getKey());
        }
        return entry.getValue().getExamples();
    }


    /**
     * Return actual examples objects of API response with values and processed from references (unaliased)
     *
     * @param openapi OpenAPI spec.
     * @param apiRespExamples examples of API response
     * @return unaliased examples of API response
     */
    public static List<Map<String, Object>> unaliasExamples(OpenAPI openapi, Map<String, Example> apiRespExamples) {
        Map<String, Example> actualComponentsExamples = getAllExamples(openapi);

        List<Map<String, Object>> result = new ArrayList<>();
        for (Map.Entry<String, Example> example : apiRespExamples.entrySet()) {
            try {
                Map<String, Object> exampleRepr = new LinkedHashMap<>();
                String exampleName = ModelUtils.getSimpleRef(example.getValue().get$ref());

                // api response example can both be a reference and specified directly in the code
                // if the reference is null, we get the value directly from the example -- no unaliasing is needed
                // if it isn't, we get the value from the components examples
                Object exampleValue;
                if(example.getValue().get$ref() != null){
                    exampleValue = actualComponentsExamples.get(exampleName).getValue();
                    LOGGER.trace("Unaliased example value from components examples: {}", exampleValue);
                } else {
                    exampleValue = example.getValue().getValue();
                    LOGGER.trace("Retrieved example value directly from the api response example definition: {}", exampleValue);
                }

                exampleRepr.put("exampleName", exampleName);
                exampleRepr.put("exampleValue", new ObjectMapper().writeValueAsString(exampleValue)
                        .replace("\"", "\\\""));

                result.add(exampleRepr);
            } catch (JsonProcessingException e) {
                LOGGER.error("Failed to serialize example value", e);
                throw new RuntimeException(e);
            }
        }

        return result;
    }

    private static Map<String, Example> getAllExamples(OpenAPI openapi) {
        return openapi.getComponents().getExamples();
    }
}
