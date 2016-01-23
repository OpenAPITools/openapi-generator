package io.swagger.codegen.examples;


import io.swagger.models.Model;
import io.swagger.models.ModelImpl;
import io.swagger.models.RefModel;
import io.swagger.models.Xml;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.BooleanProperty;
import io.swagger.models.properties.DateProperty;
import io.swagger.models.properties.DateTimeProperty;
import io.swagger.models.properties.IntegerProperty;
import io.swagger.models.properties.LongProperty;
import io.swagger.models.properties.Property;
import io.swagger.models.properties.RefProperty;
import io.swagger.models.properties.StringProperty;
import org.codehaus.plexus.util.StringUtils;

import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

public class XmlExampleGenerator {
    public static String NEWLINE = "\n";
    public static String TAG_START = "<";
    public static String CLOSE_TAG = ">";
    public static String TAG_END = "</";
    private static String EMPTY = "";
    protected Map<String, Model> examples;
    protected SimpleDateFormat dtFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'");
    protected SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'");

    public XmlExampleGenerator(Map<String, Model> examples) {
        this.examples = examples;
        if (examples == null) {
            this.examples = new HashMap<String, Model>(); 
        }
    }

    public String toXml(Property property) {
        return toXml(null, property, 0, Collections.<String>emptySet());
    }

    protected String toXml(Model model, int indent, Collection<String> path) {
        if (model instanceof RefModel) {
            RefModel ref = (RefModel) model;
            Model actualModel = examples.get(ref.getSimpleRef());
            if (actualModel instanceof ModelImpl) {
                return modelImplToXml((ModelImpl) actualModel, indent, path);
            }
        } else if (model instanceof ModelImpl) {
            return modelImplToXml((ModelImpl) model, indent, path);
        }
        return null;
    }

    protected String modelImplToXml(ModelImpl model, int indent, Collection<String> path) {
        final String modelName = model.getName();
        if (path.contains(modelName)) {
            return EMPTY;
        }
        final Set<String> selfPath = new HashSet<String>(path);
        selfPath.add(modelName);

        StringBuilder sb = new StringBuilder();
        // attributes
        Map<String, Property> attributes = new LinkedHashMap<String, Property>();
        Map<String, Property> elements = new LinkedHashMap<String, Property>();

        String name = modelName;
        Xml xml = model.getXml();
        if (xml != null) {
            if (xml.getName() != null) {
                name = xml.getName();
            }
        }
        // TODO: map objects will not enter this block
        if(model.getProperties() != null) {
            for (String pName : model.getProperties().keySet()) {
                Property p = model.getProperties().get(pName);
                if (p != null && p.getXml() != null && p.getXml().getAttribute() != null && p.getXml().getAttribute()) {
                    attributes.put(pName, p);
                } else {
                    elements.put(pName, p);
                }
            }
        }
        sb.append(indent(indent)).append(TAG_START);
        sb.append(name);
        for (String pName : attributes.keySet()) {
            Property p = attributes.get(pName);
            sb.append(" ").append(pName).append("=").append(quote(toXml(null, p, 0, selfPath)));
        }
        sb.append(CLOSE_TAG);
        sb.append(NEWLINE);
        for (String pName : elements.keySet()) {
            Property p = elements.get(pName);
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

    protected String toXml(String name, Property property, int indent, Collection<String> path) {
        if (property == null) {
            return "";
        }
        StringBuilder sb = new StringBuilder();

        if (property instanceof ArrayProperty) {
            ArrayProperty p = (ArrayProperty) property;
            Property inner = p.getItems();
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
        } else if (property instanceof RefProperty) {
            RefProperty ref = (RefProperty) property;
            Model actualModel = examples.get(ref.getSimpleRef());
            sb.append(toXml(actualModel, indent, path));
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

    protected String getExample(Property property) {
        if (property instanceof DateTimeProperty) {
            if (property.getExample() != null) {
                return property.getExample();
            } else {
                return dtFormat.format(new Date());
            }
        } else if (property instanceof StringProperty) {
            if (property.getExample() != null) {
                return property.getExample();
            } else {
                return "string";
            }
        } else if (property instanceof DateProperty) {
            if (property.getExample() != null) {
                return property.getExample();
            } else {
                return dateFormat.format(new Date());
            }
        } else if (property instanceof IntegerProperty) {
            if (property.getExample() != null) {
                return property.getExample();
            } else {
                return "0";
            }
        } else if (property instanceof BooleanProperty) {
            if (property.getExample() != null) {
                return property.getExample();
            } else {
                return "true";
            }
        } else if (property instanceof LongProperty) {
            if (property.getExample() != null) {
                return property.getExample();
            } else {
                return "123456";
            }
        }
        return "not implemented " + property;
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