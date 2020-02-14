package org.openapitools.codegen.serializer;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.MapperFeature;
import com.fasterxml.jackson.databind.module.SimpleModule;
import io.swagger.v3.core.util.Yaml;
import io.swagger.v3.core.util.Json;
import io.swagger.v3.oas.models.OpenAPI;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class SerializerUtils {
    private static final Logger LOGGER = LoggerFactory.getLogger(SerializerUtils.class);

    public static String toYamlString(OpenAPI openAPI) {
        if(openAPI == null) {
            return null;
        }
        SimpleModule module = createModule();
        try {
            return Yaml.mapper()
                    .copy()
                    .registerModule(module)
                    .configure(MapperFeature.SORT_PROPERTIES_ALPHABETICALLY, true)
                    .writeValueAsString(openAPI)
                    .replace("\r\n", "\n");
        } catch (JsonProcessingException e) {
            LOGGER.warn("Can not create yaml content", e);
        }
        return null;
    }

    public static String toJsonString(OpenAPI openAPI) {
        if (openAPI == null) {
            return null;
        }
        
        SimpleModule module = createModule();
        try {
            return Json.mapper()
                    .copy()
                    .registerModule(module)
                    .configure(MapperFeature.SORT_PROPERTIES_ALPHABETICALLY, true)
                    .writerWithDefaultPrettyPrinter()
                    .writeValueAsString(openAPI)
                    .replace("\r\n", "\n");
        } catch (JsonProcessingException e) {
            LOGGER.warn("Can not create json content", e);
        }
        return null;
    }

    private static SimpleModule createModule() {
        SimpleModule module = new SimpleModule("OpenAPIModule");
        module.addSerializer(OpenAPI.class, new OpenAPISerializer());
        return module;
    }
}
