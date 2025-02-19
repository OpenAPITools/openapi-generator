package org.openapitools.codegen.serializer;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.MapperFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.module.SimpleModule;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import com.fasterxml.jackson.dataformat.yaml.YAMLGenerator;
import io.swagger.v3.core.util.Json;
import io.swagger.v3.core.util.Yaml;
import io.swagger.v3.oas.models.OpenAPI;
import org.openapitools.codegen.config.GlobalSettings;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class SerializerUtils {
    private static final Logger LOGGER = LoggerFactory.getLogger(SerializerUtils.class);
    private static final String YAML_MINIMIZE_QUOTES_PROPERTY = "org.openapitools.codegen.utils.yaml.minimize.quotes";
    private static final boolean minimizeYamlQuotes = Boolean.parseBoolean(GlobalSettings.getProperty(YAML_MINIMIZE_QUOTES_PROPERTY, "true"));

    public static String toYamlString(OpenAPI openAPI) {
        if (openAPI == null) {
            return null;
        }
        SimpleModule module = createModule();
        try {
            ObjectMapper yamlMapper = Yaml.mapper().copy();
            // there is an unfortunate YAML condition where user inputs should be treated as strings (e.g. "1234_1234"), but in yaml this is a valid number and
            // removing quotes forcibly by default means we are potentially doing a data conversion resulting in an unexpected change to the user's YAML outputs.
            // We may allow for property-based enable/disable, retaining the default of enabled for backward compatibility.
            if (minimizeYamlQuotes) {
                ((YAMLFactory) yamlMapper.getFactory()).enable(YAMLGenerator.Feature.MINIMIZE_QUOTES);
            } else {
                ((YAMLFactory) yamlMapper.getFactory()).disable(YAMLGenerator.Feature.MINIMIZE_QUOTES);
            }
            return yamlMapper.registerModule(module)
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
