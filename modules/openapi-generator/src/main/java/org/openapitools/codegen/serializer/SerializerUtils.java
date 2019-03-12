package org.openapitools.codegen.serializer;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.MapperFeature;
import com.fasterxml.jackson.databind.module.SimpleModule;
import io.swagger.v3.core.util.Yaml;
import io.swagger.v3.oas.models.OpenAPI;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class SerializerUtils {
    private static final Logger LOGGER = LoggerFactory.getLogger(SerializerUtils.class);

    public static String toYamlString(OpenAPI openAPI) {
        if(openAPI == null) {
            return null;
        }

        SimpleModule module = new SimpleModule("OpenAPIModule");
        module.addSerializer(OpenAPI.class, new OpenAPISerializer());
        try {
            return Yaml.mapper()
                    .registerModule(module)
                    .configure(MapperFeature.SORT_PROPERTIES_ALPHABETICALLY, true)
                    .writeValueAsString(openAPI)
                    .replace("\r\n", "\n");
        } catch (JsonProcessingException e) {
            LOGGER.warn("Can not create yaml content", e);
        }
        return null;
    }
}
