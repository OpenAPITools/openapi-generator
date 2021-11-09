/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.examples;

import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.XML;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;

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

    public String toXml(Schema schema) {
        return toXml(null, schema, 0, Collections.emptySet());
    }

    protected String toXml(Schema schema, int indent, Collection<String> path) {
        if (schema == null) return "";
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
        Map<String, Schema> attributes = new LinkedHashMap<String, Schema>();
        Map<String, Schema> elements = new LinkedHashMap<String, Schema>();

        String name = modelName;
        XML xml = schema.getXml();
        if (xml != null) {
            if (xml.getName() != null) {
                name = xml.getName();
            }
        }
        // TODO: map objects will not enter this block
        Map<String, Schema> properties = schema.getProperties();
        if (properties != null && !properties.isEmpty()) {
            for (Map.Entry<String, Schema> propertiesEntry : properties.entrySet()) {
                String pName = propertiesEntry.getKey();
                Schema property = propertiesEntry.getValue();
                if (property != null && property.getXml() != null && property.getXml().getAttribute() != null && property.getXml().getAttribute()) {
                    attributes.put(pName, property);
                } else {
                    elements.put(pName, property);
                }
            }
        }

        sb.append(indent(indent)).append(TAG_START);
        sb.append(name);
        for (Map.Entry<String, Schema> attributesEntry : attributes.entrySet()) {
            String pName = attributesEntry.getKey();
            Schema s = attributesEntry.getValue();
            sb.append(" ").append(pName).append("=").append(quote(toXml(null, s, 0, selfPath)));
        }
        sb.append(CLOSE_TAG);
        sb.append(NEWLINE);
        for (Map.Entry<String, Schema> elementsEntry : elements.entrySet()) {
            String pName = elementsEntry.getKey();
            Schema s = elementsEntry.getValue();
            final String asXml = toXml(pName, s, indent + 1, selfPath);
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

    protected String toXml(String name, Schema schema, int indent, Collection<String> path) {
        if (schema == null) {
            return "";
        }
        StringBuilder sb = new StringBuilder();

        if (ModelUtils.isArraySchema(schema)) {
            ArraySchema as = (ArraySchema) schema;
            Schema inner = as.getItems();
            boolean wrapped = false;
            if (schema.getXml() != null && schema.getXml().getWrapped() != null && schema.getXml().getWrapped()) {
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
        } else if (StringUtils.isNotEmpty(schema.get$ref())) {
            Schema actualSchema = examples.get(schema.get$ref());
            sb.append(toXml(actualSchema, indent, path));
        } else {
            if (name != null) {
                sb.append(indent(indent));
                sb.append(openTag(name));
            }
            sb.append(getExample(schema));
            if (name != null) {
                sb.append(closeTag(name));
            }
        }
        return sb.toString();
    }

    /**
     * Get the example string value for the given schema.
     *
     * If an example value was not provided in the specification, a default will be generated.
     *
     * @param schema Schema to get example string for
     * @return Example String
     */
    protected String getExample(Schema schema) {
        if (schema.getExample() != null) {
            return schema.getExample().toString();
        } else if (ModelUtils.isDateTimeSchema(schema)) {
            return "2000-01-23T04:56:07.000Z";
        } else if (ModelUtils.isDateSchema(schema)) {
            return "2000-01-23";
        } else if (ModelUtils.isBooleanSchema(schema)) {
            return "true";
        } else if (ModelUtils.isNumberSchema(schema)) {
            if (ModelUtils.isFloatSchema(schema)) { // float
                return "1.3579";
            } else { // double
                return "3.149";
            }
        } else if (ModelUtils.isPasswordSchema(schema)) {
            return "********";
        } else if (ModelUtils.isUUIDSchema(schema)) {
            return "046b6c7f-0b8a-43b9-b35d-6489e6daee91";
        } else if (ModelUtils.isURISchema(schema)) {
            return "https://openapi-generator.tech";
            // do these last in case the specific types above are derived from these classes
        } else if (ModelUtils.isStringSchema(schema)) {
            return "aeiou";
        } else if (ModelUtils.isIntegerSchema(schema)) {
            if (ModelUtils.isLongSchema(schema)) { // long
                return "123456789";
            } else { //integer
                return "123";
            }
        } else {
            LOGGER.debug("default example value not implemented for {}. Default to UNDEFINED_EXAMPLE_VALUE", schema);
            return "UNDEFINED_EXAMPLE_VALUE";
        }
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
