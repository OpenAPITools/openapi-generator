package org.openapitools.codegen.serializer;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.MapperFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.module.SimpleModule;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import com.fasterxml.jackson.dataformat.yaml.YAMLGenerator;
import io.swagger.v3.core.util.Json;
import io.swagger.v3.core.util.Yaml;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.PathItem;
import org.openapitools.codegen.config.GlobalSettings;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;

public class SerializerUtils {
    private static final Logger LOGGER = LoggerFactory.getLogger(SerializerUtils.class);
    private static final String YAML_MINIMIZE_QUOTES_PROPERTY = "org.openapitools.codegen.utils.yaml.minimize.quotes";
    private static final boolean minimizeYamlQuotes = Boolean.parseBoolean(GlobalSettings.getProperty(YAML_MINIMIZE_QUOTES_PROPERTY, "true"));

    public static String toYamlString(OpenAPI openAPI) {
        return toYamlString(openAPI, false);
    }

    public static String toYamlString(OpenAPI openAPI, boolean sortOutput) {
        if (openAPI == null) {
            return null;
        }
        SimpleModule module = createModule(sortOutput);
        try {
            StringWriter writer = new StringWriter();
            writeYaml(openAPI, writer, module);
            return writer.toString().replace("\r\n", "\n");
        } catch (IOException e) {
            LOGGER.warn("Can not create yaml content", e);
        }
        return null;
    }

    public static void writeYaml(OpenAPI openAPI, Writer writer) throws IOException {
        writeYaml(openAPI, writer, createModule(false));
    }

    public static void writeYaml(OpenAPI openAPI, Writer writer, boolean sortOutput) throws IOException {
        writeYaml(openAPI, writer, createModule(sortOutput));
    }

    public static String toJsonString(OpenAPI openAPI) {
        return toJsonString(openAPI, false);
    }

    public static String toJsonString(OpenAPI openAPI, boolean sortOutput) {
        if (openAPI == null) {
            return null;
        }

        SimpleModule module = createModule(sortOutput);
        try {
            StringWriter writer = new StringWriter();
            writeJson(openAPI, writer, module);
            return writer.toString().replace("\r\n", "\n");
        } catch (IOException e) {
            LOGGER.warn("Can not create json content", e);
        }
        return null;
    }

    public static void writeJson(OpenAPI openAPI, Writer writer) throws IOException {
        writeJson(openAPI, writer, createModule(false));
    }

    public static void writeJson(OpenAPI openAPI, Writer writer, boolean sortOutput) throws IOException {
        writeJson(openAPI, writer, createModule(sortOutput));
    }

    private static void writeYaml(OpenAPI openAPI, Writer writer, SimpleModule module) throws IOException {
        if (openAPI == null) {
            return;
        }
        ObjectMapper yamlMapper = Yaml.mapper().copy();
        yamlMapper.getFactory().disable(JsonGenerator.Feature.AUTO_CLOSE_TARGET);
        // there is an unfortunate YAML condition where user inputs should be treated as strings (e.g. "1234_1234"), but in yaml this is a valid number and
        // removing quotes forcibly by default means we are potentially doing a data conversion resulting in an unexpected change to the user's YAML outputs.
        // We may allow for property-based enable/disable, retaining the default of enabled for backward compatibility.
        if (minimizeYamlQuotes) {
            ((YAMLFactory) yamlMapper.getFactory()).enable(YAMLGenerator.Feature.MINIMIZE_QUOTES);
        } else {
            ((YAMLFactory) yamlMapper.getFactory()).disable(YAMLGenerator.Feature.MINIMIZE_QUOTES);
        }
        yamlMapper.registerModule(module)
                .configure(MapperFeature.SORT_PROPERTIES_ALPHABETICALLY, true)
                .writeValue(writer, openAPI);
    }

    private static void writeJson(OpenAPI openAPI, Writer writer, SimpleModule module) throws IOException {
        if (openAPI == null) {
            return;
        }
        ObjectMapper jsonMapper = Json.mapper().copy();
        jsonMapper.getFactory().disable(JsonGenerator.Feature.AUTO_CLOSE_TARGET);
        jsonMapper.registerModule(module)
                .configure(MapperFeature.SORT_PROPERTIES_ALPHABETICALLY, true)
                .writerWithDefaultPrettyPrinter()
                .writeValue(writer, openAPI);
    }

    private static SimpleModule createModule(boolean sortOutput) {
        SimpleModule module = new SimpleModule("OpenAPIModule");
        module.addSerializer(OpenAPI.class, new OpenAPISerializer());
        module.addSerializer(byte[].class, new ByteArraySerializer());
        if (sortOutput) {
            module.addSerializer(PathItem.class, new PathItemSerializer());
        }
        return module;
    }
}
