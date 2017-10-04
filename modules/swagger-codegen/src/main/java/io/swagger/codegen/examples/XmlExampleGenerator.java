package io.swagger.codegen.examples;

/**
import io.swagger.models.Model;
import io.swagger.models.ModelImpl;
import io.swagger.models.RefModel;
import io.swagger.models.Xml;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.BooleanProperty;
import io.swagger.models.properties.DateProperty;
import io.swagger.models.properties.DateTimeProperty;
import io.swagger.models.properties.LongProperty;
import io.swagger.models.properties.DecimalProperty;
import io.swagger.models.properties.DoubleProperty;
import io.swagger.models.properties.BaseIntegerProperty;
import io.swagger.models.properties.AbstractNumericProperty;
import io.swagger.models.properties.PasswordProperty;
import io.swagger.models.properties.UUIDProperty;
import io.swagger.models.properties.Property;
import io.swagger.models.properties.RefProperty;
import io.swagger.models.properties.StringProperty;
*/
import io.swagger.oas.models.media.ArraySchema;
import io.swagger.oas.models.media.BooleanSchema;
import io.swagger.oas.models.media.DateSchema;
import io.swagger.oas.models.media.DateTimeSchema;
import io.swagger.oas.models.media.IntegerSchema;
import io.swagger.oas.models.media.NumberSchema;
import io.swagger.oas.models.media.PasswordSchema;
import io.swagger.oas.models.media.Schema;
import io.swagger.oas.models.media.StringSchema;
import io.swagger.oas.models.media.UUIDSchema;
import io.swagger.oas.models.media.XML;
import io.swagger.parser.v3.util.SchemaTypeUtil;
import org.apache.commons.lang3.StringUtils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

public class XmlExampleGenerator {

    protected final Logger LOGGER = LoggerFactory.getLogger(XmlExampleGenerator.class);
    public static String NEWLINE = "\n";
    public static String TAG_START = "<";
    public static String CLOSE_TAG = ">";
    public static String TAG_END = "</";
    private static String EMPTY = "";
    protected Map<String, Schema> examples;

    public XmlExampleGenerator(Map<String, Schema> examples) {
        this.examples = examples;
        if (examples == null) {
            this.examples = new HashMap<String, Schema>();
        }
    }

    public String toXml(Schema property) {
        return toXml(null, property, 0, Collections.<String>emptySet());
    }

    protected String toXml(Schema schema, int indent, Collection<String> path) {
        if (StringUtils.isNotEmpty(schema.get$ref())) {
            Schema actualSchema = examples.get(schema.get$ref());
            if (actualSchema != null) {
                return modelImplToXml(actualSchema, indent, path);
            }
        }
        return modelImplToXml(schema, indent, path);
    }

    protected String modelImplToXml(Schema schema, int indent, Collection<String> path) {
        final String modelName = schema.getName();
        if (path.contains(modelName)) {
            return EMPTY;
        }
        final Set<String> selfPath = new HashSet<String>(path);
        selfPath.add(modelName);

        StringBuilder sb = new StringBuilder();
        // attributes
        Map<String, Schema> attributes = new LinkedHashMap<>();
        Map<String, Schema> elements = new LinkedHashMap<>();

        String name = modelName;
        XML xml = schema.getXml();
        if (xml != null) {
            if (xml.getName() != null) {
                name = xml.getName();
            }
        }
        // TODO: map objects will not enter this block
        Map<String, Schema> properties = schema.getProperties();
        if(properties != null && !properties.isEmpty()) {
            for (String pName : properties.keySet()) {
                Schema property = properties.get(pName);
                if (property != null && property.getXml() != null && property.getXml().getAttribute() != null && property.getXml().getAttribute()) {
                    attributes.put(pName, property);
                } else {
                    elements.put(pName, property);
                }
            }
        }
        sb.append(indent(indent)).append(TAG_START);
        sb.append(name);
        for (String pName : attributes.keySet()) {
            Schema property = attributes.get(pName);
            sb.append(" ").append(pName).append("=").append(quote(toXml(null, property, 0, selfPath)));
        }
        sb.append(CLOSE_TAG);
        sb.append(NEWLINE);
        for (String pName : elements.keySet()) {
            Schema p = elements.get(pName);
            final String asXml = toXml(pName, p, indent + 1, selfPath);
            if (StringUtils.isEmpty(asXml)) {
                continue;
            }
            sb.append(asXml);
            sb.append(NEWLINE);
        }
        sb.append(indent(indent)).append(TAG_END).append(name).append(CLOSE_TAG);

        return sb.toString();
    }

    @SuppressWarnings("static-method")
    protected String quote(String string) {
        return "\"" + string + "\"";
    }

    protected String toXml(String name, Schema property, int indent, Collection<String> path) {
        if (property == null) {
            return "";
        }
        StringBuilder sb = new StringBuilder();

        if (property instanceof ArraySchema) {
            ArraySchema arraySchema = (ArraySchema) property;
            Schema inner = arraySchema.getItems();
            boolean wrapped = false;
            if (property.getXml() != null && property.getXml().getWrapped() != null && property.getXml().getWrapped()) {
                wrapped = true;
            }
            if (wrapped) {
                String prefix = EMPTY;
                if (name != null) {
                    sb.append(indent(indent));
                    sb.append(openTag(name));
                    prefix = NEWLINE;
                }
                final String asXml = toXml(name, inner, indent + 1, path);
                if (StringUtils.isNotEmpty(asXml)) {
                    sb.append(prefix).append(asXml);
                }
                if (name != null) {
                    sb.append(NEWLINE);
                    sb.append(indent(indent));
                    sb.append(closeTag(name));
                }
            } else {
                sb.append(toXml(name, inner, indent, path));
            }
        } else if (StringUtils.isNotEmpty(property.get$ref())) {
            Schema actualSchema = examples.get(property.get$ref());
            sb.append(toXml(actualSchema, indent, path));
        } else {
            if (name != null) {
                sb.append(indent(indent));
                sb.append(openTag(name));
            }
            sb.append(getExample(property));
            if (name != null) {
                sb.append(closeTag(name));
            }
        }
        return sb.toString();
    }

    /**
    * Get the example string value for the given Property.
    *
    * If an example value was not provided in the specification, a default will be generated.
    *
    * @param property Property to get example string for
    *
    * @return Example String
    */
    protected String getExample(Schema property) {
        if (property.getExample() != null) {
            return property.getExample().toString();
        } else if (property instanceof DateTimeSchema) {
            return "2000-01-23T04:56:07.000Z";
        } else if (property instanceof DateSchema) {
            return "2000-01-23";
        } else if (property instanceof BooleanSchema) {
            return "true";
        } else if (property instanceof IntegerSchema) {
            if(SchemaTypeUtil.INTEGER32_FORMAT.equals(property.getFormat())) {
                return "123";
            }
            return "123456789";
        } else if (property instanceof NumberSchema) {
            return "1.3579";
        } else if (property instanceof PasswordSchema) {
            return "********";
        } else if (property instanceof UUIDSchema) {
            return "046b6c7f-0b8a-43b9-b35d-6489e6daee91";
        // do these last in case the specific types above are derived from these classes
        } else if (property instanceof StringSchema) {
            return "aeiou";
        }
        LOGGER.warn("default example value not implemented for " + property);
        return "";
    }

    @SuppressWarnings("static-method")
    protected String openTag(String name) {
        return "<" + name + ">";
    }

    @SuppressWarnings("static-method")
    protected String closeTag(String name) {
        return "</" + name + ">";
    }

    @SuppressWarnings("static-method")
    protected String indent(int indent) {
        StringBuffer sb = new StringBuffer();
        for (int i = 0; i < indent; i++) {
            sb.append("  ");
        }
        return sb.toString();
    }
}