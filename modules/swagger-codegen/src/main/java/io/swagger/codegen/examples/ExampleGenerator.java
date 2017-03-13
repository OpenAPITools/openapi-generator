package io.swagger.codegen.examples;

import static io.swagger.models.properties.StringProperty.Format.URI;
import static io.swagger.models.properties.StringProperty.Format.URL;

import io.swagger.models.Model;
import io.swagger.models.ModelImpl;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.BaseIntegerProperty;
import io.swagger.models.properties.BooleanProperty;
import io.swagger.models.properties.DateProperty;
import io.swagger.models.properties.DateTimeProperty;
import io.swagger.models.properties.DecimalProperty;
import io.swagger.models.properties.DoubleProperty;
import io.swagger.models.properties.FileProperty;
import io.swagger.models.properties.FloatProperty;
import io.swagger.models.properties.LongProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.ObjectProperty;
import io.swagger.models.properties.Property;
import io.swagger.models.properties.RefProperty;
import io.swagger.models.properties.StringProperty;
import io.swagger.models.properties.UUIDProperty;
import io.swagger.util.Json;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class ExampleGenerator {
    private static final Logger logger = LoggerFactory.getLogger(ExampleGenerator.class);

    // TODO: move constants to more appropriate location
    private static final String MIME_TYPE_JSON = "application/json";
    private static final String MIME_TYPE_XML = "application/xml";

    private static final String EXAMPLE = "example";
    private static final String CONTENT_TYPE = "contentType";
    private static final String OUTPUT = "output";
    private static final String NONE = "none";

    protected Map<String, Model> examples;

    public ExampleGenerator(Map<String, Model> examples) {
        this.examples = examples;
    }

    public List<Map<String, String>> generate(Map<String, Object> examples, List<String> mediaTypes, Property property) {
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
                    String example = Json.pretty(resolvePropertyToExample(mediaType, property, processedModels));

                    if (example != null) {
                        kv.put(EXAMPLE, example);
                        output.add(kv);
                    }
                } else if (property != null && mediaType.startsWith(MIME_TYPE_XML)) {
                    String example = new XmlExampleGenerator(this.examples).toXml(property);
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

    private Object resolvePropertyToExample(String mediaType, Property property, Set<String> processedModels) {
        logger.debug("Resolving example for property {}...", property);
        if (property.getExample() != null) {
            logger.debug("Example set in swagger spec, returning example: '{}'", property.getExample().toString());
            return property.getExample();
        } else if (property instanceof StringProperty) {
            logger.debug("String property");
            String defaultValue = ((StringProperty) property).getDefault();
            if (defaultValue != null && !defaultValue.isEmpty()) {
                logger.debug("Default value found: '{}'", defaultValue);
                return defaultValue;
            }
            List<String> enumValues = ((StringProperty) property).getEnum();
            if (enumValues != null && !enumValues.isEmpty()) {
                logger.debug("Enum value found: '{}'", enumValues.get(0));
                return enumValues.get(0);
            }
            String format = property.getFormat();
            if (format != null && (URI.getName().equals(format) || URL.getName().equals(format))) {
                logger.debug("URI or URL format, without default or enum, generating random one.");
                return "http://example.com/aeiou";
            }
            logger.debug("No values found, using default string 'aeiou' as example");
            return "aeiou";
        } else if (property instanceof BooleanProperty) {
            Boolean defaultValue = ((BooleanProperty) property).getDefault();
            if (defaultValue != null) {
                return defaultValue;
            }
            return Boolean.TRUE;
        } else if (property instanceof ArrayProperty) {
            Property innerType = ((ArrayProperty) property).getItems();
            if (innerType != null) {
                return new Object[]{
                        resolvePropertyToExample(mediaType, innerType, processedModels)
                };
            }
        } else if (property instanceof DateProperty) {
            return "2000-01-23";
        } else if (property instanceof DateTimeProperty) {
            return "2000-01-23T04:56:07.000+00:00";
        } else if (property instanceof DoubleProperty) {
            Double min = ((DecimalProperty) property).getMinimum() == null ? null : ((DecimalProperty) property).getMinimum().doubleValue();
            Double max = ((DecimalProperty) property).getMaximum() == null ? null : ((DecimalProperty) property).getMaximum().doubleValue();
            return randomNumber(min, max);
        } else if (property instanceof FloatProperty) {
            Double min = ((DecimalProperty) property).getMinimum() == null ? null : ((DecimalProperty) property).getMinimum().doubleValue();
            Double max = ((DecimalProperty) property).getMaximum() == null ? null : ((DecimalProperty) property).getMaximum().doubleValue();
            return (float) randomNumber(min, max);
        }  else if (property instanceof DecimalProperty) {
            Double min = ((DecimalProperty) property).getMinimum() == null ? null : ((DecimalProperty) property).getMinimum().doubleValue();
            Double max = ((DecimalProperty) property).getMaximum() == null ? null : ((DecimalProperty) property).getMaximum().doubleValue();
            return new BigDecimal(randomNumber(min, max));
        } else if (property instanceof FileProperty) {
            return "";  // TODO
        } else if (property instanceof LongProperty) {
            Double min = ((BaseIntegerProperty) property).getMinimum() == null ? null : ((BaseIntegerProperty) property).getMinimum().doubleValue();
            Double max = ((BaseIntegerProperty) property).getMaximum() == null ? null : ((BaseIntegerProperty) property).getMaximum().doubleValue();
            return (long) randomNumber(min, max);
        } else if (property instanceof BaseIntegerProperty) { // Includes IntegerProperty
            Double min = ((BaseIntegerProperty) property).getMinimum() == null ? null : ((BaseIntegerProperty) property).getMinimum().doubleValue();
            Double max = ((BaseIntegerProperty) property).getMaximum() == null ? null : ((BaseIntegerProperty) property).getMaximum().doubleValue();
            return (int) randomNumber(min, max);
        } else if (property instanceof MapProperty) {
            Map<String, Object> mp = new HashMap<String, Object>();
            if (property.getName() != null) {
                mp.put(property.getName(),
                        resolvePropertyToExample(mediaType, ((MapProperty) property).getAdditionalProperties(), processedModels));
            } else {
                mp.put("key",
                        resolvePropertyToExample(mediaType, ((MapProperty) property).getAdditionalProperties(), processedModels));
            }
            return mp;
        } else if (property instanceof ObjectProperty) {
            return "{}";
        } else if (property instanceof RefProperty) {
            String simpleName = ((RefProperty) property).getSimpleRef();
            logger.debug("Ref property, simple name: {}", simpleName);
            Model model = examples.get(simpleName);
            if (model != null) {
                return resolveModelToExample(simpleName, mediaType, model, processedModels);
            }
            logger.warn("Ref property with empty model.");
        } else if (property instanceof UUIDProperty) {
            return "046b6c7f-0b8a-43b9-b35d-6489e6daee91"; 
        }

        return "";
    }

    private double randomNumber(Double min, Double max) {
        if (min != null && max != null) {
            double range = max - min;
            return Math.random() * range + min;
        } else if (min != null) {
            return Math.random() + min;
        } else if (max != null) {
            return Math.random() * max;
        } else {
            return Math.random() * 10;
        }
    }

    private Object resolveModelToExample(String name, String mediaType, Model model, Set<String> processedModels) {
        if (processedModels.contains(name)) {
            return "";
        }
        if (model instanceof ModelImpl) {
            processedModels.add(name);
            ModelImpl impl = (ModelImpl) model;
            Map<String, Object> values = new HashMap<>();

            logger.debug("Resolving model '{}' to example", name);

            if (impl.getExample() != null) {
                logger.debug("Using example from spec: {}", impl.getExample());
                return impl.getExample();
            } else if (impl.getProperties() != null) {
                logger.debug("Creating example from model values");
                for (String propertyName : impl.getProperties().keySet()) {
                    Property property = impl.getProperties().get(propertyName);
                    values.put(propertyName, resolvePropertyToExample(mediaType, property, processedModels));
                }
            }
            return values;
        }
        return "";
    }
}