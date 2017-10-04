package io.swagger.codegen.examples;

import io.swagger.oas.models.media.ArraySchema;
import io.swagger.oas.models.media.BooleanSchema;
import io.swagger.oas.models.media.DateSchema;
import io.swagger.oas.models.media.DateTimeSchema;
import io.swagger.oas.models.media.FileSchema;
import io.swagger.oas.models.media.IntegerSchema;
import io.swagger.oas.models.media.MapSchema;
import io.swagger.oas.models.media.NumberSchema;
import io.swagger.oas.models.media.ObjectSchema;
import io.swagger.oas.models.media.Schema;
import io.swagger.oas.models.media.StringSchema;
import io.swagger.oas.models.media.UUIDSchema;
import io.swagger.parser.v3.util.SchemaTypeUtil;
import io.swagger.util.Json;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;

//import static io.swagger.models.properties.StringProperty.Format.URI;
//import static io.swagger.models.properties.StringProperty.Format.URL;

public class ExampleGenerator {

    private static final Logger logger = LoggerFactory.getLogger(ExampleGenerator.class);

    // TODO: move constants to more appropriate location
    private static final String MIME_TYPE_JSON = "application/json";
    private static final String MIME_TYPE_XML = "application/xml";

    private static final String EXAMPLE = "example";
    private static final String CONTENT_TYPE = "contentType";
    private static final String OUTPUT = "output";
    private static final String NONE = "none";
    private static final String URL = "url";
    private static final String URI = "uri";

    protected Map<String, Schema> examples;
    private Random random;

    public ExampleGenerator(Map<String, Schema> examples) {
        this.examples = examples;
        // use a fixed seed to make the "random" numbers reproducible.
        this.random = new Random("ExampleGenerator".hashCode());
    }

    public List<Map<String, String>> generate(Map<String, Object> examples, List<String> mediaTypes, Schema property) {
        List<Map<String, String>> output = new ArrayList<>();
        Set<String> processedModels = new HashSet<>();
        if (examples == null) {
            if (mediaTypes == null) {
                // assume application/json for this
                mediaTypes = Collections.singletonList(MIME_TYPE_JSON); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.
            }
            for (String mediaType : mediaTypes) {
                Map<String, String> kv = new HashMap<>();
                kv.put(CONTENT_TYPE, mediaType);
                if (property != null && mediaType.startsWith(MIME_TYPE_JSON)) {
                    String example = Json.pretty(resolvePropertyToExample("", mediaType, property, processedModels));

                    if (example != null) {
                        kv.put(EXAMPLE, example);
                        output.add(kv);
                    }
                } else if (property != null && mediaType.startsWith(MIME_TYPE_XML)) {
                    System.out.println("XML.................????");
                    String example = new XmlExampleGenerator(this.examples).toXml(property);
                    if (example != null) {
                        System.out.println("putting the example.....................");
                        System.out.println(example);
                        System.out.println("done..");
                        kv.put(EXAMPLE, example);
                        output.add(kv);
                    }
                }
            }
        } else {
            System.out.println("........................................... 00");
            for (Map.Entry<String, Object> entry : examples.entrySet()) {
                final Map<String, String> kv = new HashMap<>();
                kv.put(CONTENT_TYPE, entry.getKey());
                kv.put(EXAMPLE, Json.pretty(entry.getValue()));
                output.add(kv);
            }
        }
        System.out.println("the size is: " + output.size());
        if (output.size() == 0) {
            Map<String, String> kv = new HashMap<>();
            kv.put(OUTPUT, NONE);
            output.add(kv);
        }
        return output;
    }

    public List<Map<String, String>> generate(Map<String, Object> examples, List<String> mediaTypes, String modelName) {
        List<Map<String, String>> output = new ArrayList<>();
        Set<String> processedModels = new HashSet<>();
        if (examples == null) {
            if (mediaTypes == null) {
                // assume application/json for this
                mediaTypes = Collections.singletonList(MIME_TYPE_JSON); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.
            }
            for (String mediaType : mediaTypes) {
                Map<String, String> kv = new HashMap<>();
                kv.put(CONTENT_TYPE, mediaType);
                if (modelName != null && mediaType.startsWith(MIME_TYPE_JSON)) {
                    final Schema schema = this.examples.get(modelName);
                    if (schema != null) {

                        String example = Json.pretty(resolveModelToExample(modelName, mediaType, schema, processedModels));

                        if (example != null) {
                            kv.put(EXAMPLE, example);
                            output.add(kv);
                        }
                    }
                } else if (modelName != null && mediaType.startsWith(MIME_TYPE_XML)) {
                    final Schema schema = this.examples.get(modelName);
                    String example = new XmlExampleGenerator(this.examples).toXml(schema, 0, Collections.<String>emptySet());
                    if (example != null) {
                        kv.put(EXAMPLE, example);
                        output.add(kv);
                    }
                }
            }
        } else {
            for (Map.Entry<String, Object> entry : examples.entrySet()) {
                final Map<String, String> kv = new HashMap<>();
                kv.put(CONTENT_TYPE, entry.getKey());
                kv.put(EXAMPLE, Json.pretty(entry.getValue()));
                output.add(kv);
            }
        }
        if (output.size() == 0) {
            Map<String, String> kv = new HashMap<>();
            kv.put(OUTPUT, NONE);
            output.add(kv);
        }
        return output;
    }

    private Object resolvePropertyToExample(String propertyName, String mediaType, Schema property, Set<String> processedModels) {
        logger.debug("Resolving example for property {}...", property);
        if (property.getExample() != null) {
            logger.debug("Example set in swagger spec, returning example: '{}'", property.getExample().toString());
            return property.getExample();
        } else if (property instanceof StringSchema) {
            logger.debug("String property");
            String defaultValue = ((StringSchema) property).getDefault();
            if (defaultValue != null && !defaultValue.isEmpty()) {
                logger.debug("Default value found: '{}'", defaultValue);
                return defaultValue;
            }
            List<String> enumValues = ((StringSchema) property).getEnum();
            if (enumValues != null && !enumValues.isEmpty()) {
                logger.debug("Enum value found: '{}'", enumValues.get(0));
                return enumValues.get(0);
            }
            String format = property.getFormat();
            if (format != null && (URI.equals(format) || URL.equals(format))) {
                logger.debug("URI or URL format, without default or enum, generating random one.");
                return "http://example.com/aeiou";
            }
            logger.debug("No values found, using property name " + propertyName + " as example");
            return propertyName;
        } else if (property instanceof BooleanSchema) {
            Object defaultValue = property.getDefault();
            if (defaultValue != null) {
                return defaultValue;
            }
            return Boolean.TRUE;
        } else if (property instanceof ArraySchema) {
            Schema innerType = ((ArraySchema) property).getItems();
            if (innerType != null) {
                int arrayLength = null == ((ArraySchema) property).getMaxItems() ? 2 : ((ArraySchema) property).getMaxItems();
                Object[] objectProperties = new Object[arrayLength];
                Object objProperty = resolvePropertyToExample(propertyName, mediaType, innerType, processedModels);
                for(int i=0; i < arrayLength; i++) {
                    objectProperties[i] = objProperty;
                }
                return objectProperties;
            }
        } else if (property instanceof DateSchema) {
            return "2000-01-23";
        } else if (property instanceof DateTimeSchema) {
            return "2000-01-23T04:56:07.000+00:00";
        } else if (property instanceof NumberSchema) {
            Double min = property.getMinimum() == null ? null : property.getMinimum().doubleValue();
            Double max = property.getMaximum() == null ? null : property.getMaximum().doubleValue();
            if(SchemaTypeUtil.FLOAT_FORMAT.equals(property.getFormat())) {
                return (float) randomNumber(min, max);
            }
            return randomNumber(min, max);
        } else if (property instanceof FileSchema) {
            return "";  // TODO
        } else if (property instanceof IntegerSchema) {
            Double min = property.getMinimum() == null ? null : property.getMinimum().doubleValue();
            Double max = property.getMaximum() == null ? null : property.getMaximum().doubleValue();
            if(SchemaTypeUtil.INTEGER32_FORMAT.equals(property.getFormat())) {
                return (long) randomNumber(min, max);
            }
            return (int) randomNumber(min, max);
        } else if (property instanceof MapSchema) {
            Map<String, Object> mp = new HashMap<String, Object>();
            if (property.getName() != null) {
                mp.put(property.getName(),
                        resolvePropertyToExample(propertyName, mediaType, property.getAdditionalProperties(), processedModels));
            } else {
                mp.put("key",
                        resolvePropertyToExample(propertyName, mediaType, property.getAdditionalProperties(), processedModels));
            }
            return mp;
        } else if (property instanceof ObjectSchema) {
            return "{}";
        }
        /** this concep no longer exists
        else if (property instanceof RefProperty) {
            String simpleName = ((RefProperty) property).getSimpleRef();
            logger.debug("Ref property, simple name: {}", simpleName);
            Model model = examples.get(simpleName);
            if (model != null) {
                return resolveModelToExample(simpleName, mediaType, model, processedModels);
            }
            logger.warn("Ref property with empty model.");

        }*/ else if (property instanceof UUIDSchema) {
            return "046b6c7f-0b8a-43b9-b35d-6489e6daee91"; 
        }

        return "";
    }

    private double randomNumber(Double min, Double max) {
        if (min != null && max != null) {
            double range = max - min;
            return random.nextDouble() * range + min;
        } else if (min != null) {
            return random.nextDouble() + min;
        } else if (max != null) {
            return random.nextDouble() * max;
        } else {
            return random.nextDouble() * 10;
        }
    }

    private Object resolveModelToExample(String name, String mediaType, Schema schema, Set<String> processedModels) {
        if (processedModels.contains(name)) {
            return schema.getExample();
        }
        processedModels.add(name);
        Map<String, Object> values = new HashMap<>();

        logger.debug("Resolving model '{}' to example", name);

        if (schema.getExample() != null) {
            logger.debug("Using example from spec: {}", schema.getExample());
            return schema.getExample();
        } else if (schema.getProperties() != null) {
            logger.debug("Creating example from model values");
            for (Object propertyName : schema.getProperties().keySet()) {
                schema.getProperties().get(propertyName.toString());
                values.put(propertyName.toString(), resolvePropertyToExample(propertyName.toString(), mediaType, schema, processedModels));
            }
            schema.setExample(values);
        }
        return values;
    }
}
