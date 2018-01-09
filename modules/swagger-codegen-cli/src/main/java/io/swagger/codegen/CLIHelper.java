package io.swagger.codegen;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLMapper;
import com.google.common.io.Resources;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.BooleanSchema;
import io.swagger.v3.oas.models.media.IntegerSchema;
import io.swagger.v3.oas.models.media.NumberSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.parser.util.SchemaTypeUtil;
import org.apache.commons.io.Charsets;
import org.apache.commons.lang3.StringUtils;

import java.io.IOException;
import java.net.URI;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

public class CLIHelper {

    static String loadResourceOAS3File() {
        URL url = Resources.getResource("oas3.yaml");
        try {
            return Resources.toString(url, Charsets.UTF_8);
        } catch (IOException e) {
            e.printStackTrace();
        }
        return null;
    }

    static boolean containsOptionExtensions(Map<String, Object> extensions) {
        if(extensions == null) {
            return false;
        }
        final Object option = extensions.get("x-option");
        if(option != null && StringUtils.isNotBlank(option.toString())) {
            return true;
        }
        return false;
    }

    static String getCommand(String schemaName, Schema schema) {
        if(schema.getExtensions() != null && !schema.getExtensions().isEmpty() && schema.getExtensions().get("x-command") != null) {
            return schema.getExtensions().get("x-command").toString();
        } else {
            return schemaName.toLowerCase();
        }
    }

    static String[] getArguments(Map<String, Object> extensions) {
        if(extensions.get("x-short-version") != null && StringUtils.isNotBlank(extensions.get("x-short-version").toString())) {
            return new String[] {extensions.get("x-short-version").toString(), extensions.get("x-option").toString()};
        }
        return new String[] {extensions.get("x-option").toString()};
    }

    static String detectCommand(String[] args) {
        if(args == null || args.length == 0) {
            return null;
        }
        String command = args[0];
        if(StringUtils.isBlank(command) || command.startsWith("-")) {
            return null;
        }
        return command;
    }

    static Class getClass(Schema property) {
        if(property instanceof BooleanSchema) {
            return Boolean.class;
        }
        return String.class;
    }

    static Object getDefault(Schema property) {
        if(property instanceof BooleanSchema) {
            return Boolean.TRUE;
        }
        return null;
    }

    public static Map<String, Object> createOptionValueMap(Schema schema, Map<String, Object> inputArgs) {
        if(inputArgs == null || inputArgs.isEmpty()) {
            return null;
        }
        final Map<String, Schema> properties = schema.getProperties();
        if(properties == null || properties.isEmpty()) {
            return null;
        }
        final Map<String, Object> optionValueMap = new HashMap<>();
        for(String propertyName : properties.keySet()) {
            final Schema property = properties.get(propertyName);
            final Map<String, Object> extensions = property.getExtensions();
            if(extensions == null || extensions.isEmpty()) {
                continue;
            }
            Object value = null;
            if(extensions.get("x-option") != null) {
                String option = fixOptionName(extensions.get("x-option").toString());
                value = inputArgs.get(option);
            } else {
                continue;
            }
            if(value == null) {
                continue;
            }
            if(property instanceof BooleanSchema) {
                optionValueMap.put(propertyName, Boolean.valueOf(value.toString()));
            }
            else if(property instanceof IntegerSchema) {
                if(SchemaTypeUtil.INTEGER64_FORMAT.equals(property.getFormat())) {
                    optionValueMap.put(propertyName, Long.valueOf(value.toString()));
                } else {
                    optionValueMap.put(propertyName, Integer.valueOf(value.toString()));
                }
            }
            else if(property instanceof NumberSchema) {
                if(SchemaTypeUtil.FLOAT_FORMAT.equals(property.getFormat())) {
                    optionValueMap.put(propertyName, Float.valueOf(value.toString()));
                } else {
                    optionValueMap.put(propertyName, Double.valueOf(value.toString()));
                }
            }
            else if(property instanceof ArraySchema) {
                String inputElements = value.toString()
                        .replace("[", StringUtils.EMPTY)
                        .replace("]", StringUtils.EMPTY)
                        .replace(" ", StringUtils.EMPTY);
                final List<String> values = new ArrayList<>(Arrays.asList(inputElements.split(",")));
                optionValueMap.put(propertyName, values);
            }
            else {
                optionValueMap.put(propertyName, value);
            }
        }
        return optionValueMap;
    }

    public static Map<String, Object> createOptionValueMap(JsonNode node) {
        final Map<String, Object> optionValueMap = new HashMap<>();
        Iterator<String> fieldNames = node.fieldNames();
        while (fieldNames.hasNext()) {
            String argument = fieldNames.next();
            JsonNode valueNode = node.findValue(argument);
            if (valueNode.isBoolean()) {
                optionValueMap.put(argument, valueNode.booleanValue());
            }
            else if (valueNode.isShort() || valueNode.isInt()) {
                optionValueMap.put(argument, valueNode.intValue());
            }
            else if (valueNode.isLong()) {
                optionValueMap.put(argument, valueNode.longValue());
            }
            else if (valueNode.isFloat()) {
                optionValueMap.put(argument, valueNode.floatValue());
            }
            else if (valueNode.isDouble()) {
                optionValueMap.put(argument, valueNode.doubleValue());
            }
            else if (valueNode.isArray()) {
                String inputElements = valueNode.toString()
                        .replace("[", StringUtils.EMPTY)
                        .replace("]", StringUtils.EMPTY)
                        .replace("\"", StringUtils.EMPTY)
                        .replace(" ", StringUtils.EMPTY);
                final List<String> values = new ArrayList<>(Arrays.asList(inputElements.split(",")));
                optionValueMap.put(argument, values);
            } else {
                optionValueMap.put(argument, valueNode.toString()
                        .replace("\"", StringUtils.EMPTY));
            }
        }
        return optionValueMap;
    }

    private static String fixOptionName(String option) {
        option = option.substring(countDashes(option));
        return option.replace("-", "_");
    }

    private static int countDashes(String option) {
        for(int i = 0; i < option.length(); i++) {
            if(option.charAt(i) != '-') {
                return i;
            }
        }
        return 0;
    }

    public static boolean isValidJson(String content) {

        if (StringUtils.isBlank(content)) {
            return false;
        }
        try {
            new ObjectMapper().readTree(content);
            return true;
        } catch (IOException ex) {
            return false;
        }
    }

    public static boolean isValidYaml(String content) {
        if (StringUtils.isBlank(content)) {
            return false;
        }
        try {
            new YAMLMapper().readTree(content);
            return true;
        } catch (IOException ex) {
            return false;
        }
    }

    public static boolean isValidURL(String urlStr) {
        if (StringUtils.isBlank(urlStr)) {
            return false;
        }
        try {
            URI uri = new URI(urlStr);
            return uri.getScheme().toLowerCase().startsWith("http");
        }
        catch (Exception e) {
            return false;
        }
    }
}
