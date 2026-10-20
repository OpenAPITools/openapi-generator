package org.openapitools.codegen.languages;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.utils.ModelUtils;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.function.BiFunction;
import java.util.function.Function;

/**
 * Renders Java expressions for complex OpenAPI property defaults.
 */
public final class JavaDefaultValueRenderer {
    private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

    private final Context context;

    public JavaDefaultValueRenderer(Context context) {
        this.context = context;
    }

    /**
     * Renders a complex default value, or {@code null} when this renderer does not
     * support the value.
     *
     * @param property
     *            codegen property
     * @param schema
     *            original property schema
     * @param resolvedSchema
     *            referenced property schema
     * @return Java expression or {@code null}
     */
    public String render(CodegenProperty property, Schema schema, Schema resolvedSchema) {
        final Object defaultValue = schema != null && schema.getDefault() != null
                ? schema.getDefault()
                : resolvedSchema == null ? null : resolvedSchema.getDefault();
        if (defaultValue == null) {
            return null;
        }

        final JsonNode defaultNode = this.toJsonNode(defaultValue);
        if (defaultNode == null) {
            return null;
        }
        if (defaultNode.isNull()) {
            return "null";
        }

        if (ModelUtils.isArraySchema(resolvedSchema) && defaultNode.isArray() && property != null
                && property.items != null) {
            return this.renderArrayDefault(property, resolvedSchema, defaultNode);
        }
        if (defaultNode.isValueNode()
                && (ModelUtils.isByteArraySchema(resolvedSchema) || ModelUtils.isBinarySchema(resolvedSchema))) {
            return this.renderDefaultScalar(property, resolvedSchema, defaultNode, null);
        }
        if (this.isObjectLike(resolvedSchema) && defaultNode.isObject()) {
            return this.renderObjectDefault(property, resolvedSchema, defaultNode);
        }

        return null;
    }

    private String renderArrayDefault(CodegenProperty property, Schema schema, JsonNode defaultNode) {
        if (defaultNode.isEmpty()) {
            return this.context.defaultCollectionType().apply(schema, "");
        }

        final List<String> values = new ArrayList<>();
        for (final JsonNode item : defaultNode) {
            final String value = this.renderDefaultValue(property.items, schema.getItems(), item, null);
            if (value == null) {
                return null;
            }
            values.add(value);
        }
        return this.context.defaultCollectionType().apply(schema, this.formatDefaultValues(values));
    }

    private String formatDefaultValues(List<String> values) {
        final boolean nested = values.stream()
                .anyMatch(value -> value.startsWith("new ") && value.contains("Arrays.asList("));
        if (!nested) {
            return String.join(", ", values);
        }
        return "\n        " + String.join(",\n        ", values) + "\n      ";
    }

    private String renderDefaultValue(CodegenProperty property, Schema schema, JsonNode value, String ownerType) {
        if (value == null || value.isNull()) {
            return "null";
        }

        final Schema resolvedSchema = ModelUtils.getReferencedSchema(this.context.openAPI(), schema);
        if (resolvedSchema == null) {
            return null;
        }
        if (ModelUtils.isArraySchema(resolvedSchema) && value.isArray() && property != null && property.items != null) {
            return this.renderArrayDefault(property, resolvedSchema, value);
        }
        if (this.isObjectLike(resolvedSchema) && value.isObject()) {
            return this.renderObjectDefault(property, resolvedSchema, value);
        }
        if (ModelUtils.isMapSchema(resolvedSchema)) {
            return null;
        }
        return this.renderDefaultScalar(property, resolvedSchema, value, ownerType);
    }

    private String renderObjectDefault(CodegenProperty property, Schema schema, JsonNode defaultNode) {
        if (defaultNode == null || defaultNode.isNull()) {
            return "null";
        }

        Schema targetSchema = schema;
        CodegenProperty targetProperty = property;
        boolean needsOneOfWrapper = false;
        if (ModelUtils.hasOneOf(schema)) {
            final Schema selectedMember = this.firstObjectMember(schema, defaultNode);
            if (selectedMember == null) {
                return null;
            }
            targetSchema = ModelUtils.getReferencedSchema(this.context.openAPI(), selectedMember);
            targetProperty = this.context.fromProperty().apply(property.baseName, selectedMember);
            needsOneOfWrapper = !this.context.useOneOfInterfaces();
        }
        if (targetProperty == null || targetProperty.datatypeWithEnum == null) {
            return null;
        }

        final Map<String, Schema> propertySchemas = this.context.composedSchemaProperties().apply(targetSchema);
        final StringBuilder expression = new StringBuilder("new ").append(targetProperty.datatypeWithEnum).append("()");
        for (final Map.Entry<String, JsonNode> defaultProperty : iterable(defaultNode.fields())) {
            final Schema propertySchema = propertySchemas.get(defaultProperty.getKey());
            if (propertySchema == null) {
                continue;
            }
            final CodegenProperty nestedProperty = this.context.fromProperty().apply(defaultProperty.getKey(),
                    propertySchema);
            final String propertyExpression = this.renderDefaultValue(nestedProperty, propertySchema,
                    defaultProperty.getValue(), targetProperty.datatypeWithEnum);
            if (propertyExpression != null) {
                expression.append(".").append(this.context.toVarName().apply(defaultProperty.getKey())).append("(")
                        .append(propertyExpression).append(")");
            }
        }

        if (needsOneOfWrapper) {
            return "new " + property.datatypeWithEnum + "(" + expression + ")";
        }
        return expression.toString();
    }

    private String renderDefaultScalar(CodegenProperty property, Schema schema, JsonNode value, String ownerType) {
        if (ModelUtils.isByteArraySchema(schema)) {
            return "java.util.Base64.getDecoder().decode(\"" + this.context.escapeText().apply(value.asText()) + "\")";
        }
        if (ModelUtils.isBinarySchema(schema)) {
            if (property != null && ("Resource".equals(property.datatypeWithEnum)
                    || "org.springframework.core.io.Resource".equals(property.datatypeWithEnum))) {
                return "new org.springframework.core.io.ByteArrayResource(java.util.Base64.getDecoder().decode(\""
                        + this.context.escapeText().apply(value.asText()) + "\"))";
            }
            if (property != null
                    && ("File".equals(property.datatypeWithEnum) || "java.io.File".equals(property.datatypeWithEnum))) {
                return "null";
            }
            return null;
        }
        if (ModelUtils.isEnumSchema(schema)) {
            final CodegenProperty enumProperty = property;
            final String enumType = enumProperty != null && enumProperty.isEnum && ownerType != null
                    ? ownerType + "." + enumProperty.datatypeWithEnum
                    : enumProperty == null ? null : enumProperty.datatypeWithEnum;
            return enumType == null
                    ? null
                    : enumType + "." + this.context.toEnumVarName().apply(value.asText(), enumProperty.dataType);
        }
        if (ModelUtils.isLongSchema(schema)) {
            return value.asText() + "l";
        }
        if (ModelUtils.isIntegerSchema(schema)) {
            return value.asText();
        }
        if (ModelUtils.isDoubleSchema(schema)) {
            return value.asText() + "d";
        }
        if (ModelUtils.isFloatSchema(schema)) {
            return value.asText() + "f";
        }
        if (ModelUtils.isNumberSchema(schema)) {
            return "new java.math.BigDecimal(\"" + this.context.escapeText().apply(value.asText()) + "\")";
        }
        if (ModelUtils.isURISchema(schema)) {
            return "java.net.URI.create(\"" + this.context.escapeText().apply(value.asText()) + "\")";
        }
        if (ModelUtils.isDateSchema(schema)) {
            if (this.context.dateLibrary().startsWith("java8")) {
                return "java.time.LocalDate.parse(\"" + this.context.escapeText().apply(value.asText()) + "\")";
            }
            if ("joda".equals(this.context.dateLibrary())) {
                return "org.joda.time.LocalDate.parse(\"" + this.context.escapeText().apply(value.asText()) + "\")";
            }
            return null;
        }
        if (ModelUtils.isDateTimeSchema(schema)) {
            if ("java8".equals(this.context.dateLibrary())) {
                return "java.time.OffsetDateTime.parse(\"" + this.context.escapeText().apply(value.asText()) + "\", "
                        + "java.time.format.DateTimeFormatter.ISO_ZONED_DATE_TIME.withZone(java.time.ZoneId.systemDefault()))";
            }
            if ("java8-localdatetime".equals(this.context.dateLibrary())) {
                return "java.time.OffsetDateTime.parse(\"" + this.context.escapeText().apply(value.asText())
                        + "\").toLocalDateTime()";
            }
            if ("joda".equals(this.context.dateLibrary())) {
                return "org.joda.time.DateTime.parse(\"" + this.context.escapeText().apply(value.asText()) + "\")";
            }
            return null;
        }
        if (ModelUtils.isTimeLocalSchema(schema) && this.context.dateLibrary().startsWith("java8")) {
            return "java.time.LocalTime.parse(\"" + this.context.escapeText().apply(value.asText()) + "\")";
        }
        if (ModelUtils.isDateTimeLocalSchema(schema) && this.context.dateLibrary().startsWith("java8")) {
            return "java.time.LocalDateTime.parse(\"" + this.context.escapeText().apply(value.asText()) + "\")";
        }
        if (ModelUtils.isUUIDSchema(schema)) {
            return "java.util.UUID.fromString(\"" + this.context.escapeText().apply(value.asText()) + "\")";
        }
        if (ModelUtils.isBooleanSchema(schema)) {
            return value.asText();
        }
        if (ModelUtils.isStringSchema(schema)) {
            return "\"" + this.context.escapeText().apply(value.asText()) + "\"";
        }
        return null;
    }

    private Schema firstObjectMember(Schema schema, JsonNode defaultNode) {
        final List<Schema> members = schema.getOneOf() != null ? schema.getOneOf() : schema.getAnyOf();
        if (members == null) {
            return null;
        }
        Schema firstMember = null;
        for (final Schema member : members) {
            final Schema resolved = ModelUtils.getReferencedSchema(this.context.openAPI(), member);
            if (this.isObjectLike(resolved)) {
                if (firstMember == null) {
                    firstMember = member;
                }
                if (defaultNode != null && defaultNode.isObject()
                        && this.matchesDefaultProperties(resolved, defaultNode)) {
                    return member;
                }
            }
        }
        return firstMember;
    }

    private boolean matchesDefaultProperties(Schema schema, JsonNode defaultNode) {
        final Map<String, Schema> propertySchemas = this.context.composedSchemaProperties().apply(schema);
        final Iterator<String> propertyNames = defaultNode.fieldNames();
        while (propertyNames.hasNext()) {
            if (!propertySchemas.containsKey(propertyNames.next())) {
                return false;
            }
        }
        return true;
    }

    private boolean isObjectLike(Schema schema) {
        return ModelUtils.isObjectSchema(schema) || ModelUtils.isComposedSchema(schema);
    }

    private JsonNode toJsonNode(Object value) {
        if (value instanceof JsonNode) {
            return (JsonNode) value;
        }
        if (value instanceof java.time.temporal.TemporalAccessor) {
            return OBJECT_MAPPER.getNodeFactory().textNode(value.toString());
        }
        try {
            return OBJECT_MAPPER.valueToTree(value);
        } catch (final IllegalArgumentException e) {
            return null;
        }
    }

    private static <T> Iterable<T> iterable(Iterator<T> iterator) {
        return () -> iterator;
    }

    public static final class Context {
        private final OpenAPI openAPI;
        private final String dateLibrary;
        private final boolean useOneOfInterfaces;
        private final Function<String, String> escapeText;
        private final Function<String, String> toVarName;
        private final BiFunction<String, String, String> toEnumVarName;
        private final BiFunction<String, Schema, CodegenProperty> fromProperty;
        private final BiFunction<Schema, String, String> defaultCollectionType;
        private final Function<Schema, Map<String, Schema>> composedSchemaProperties;

        public Context(OpenAPI openAPI, String dateLibrary, boolean useOneOfInterfaces,
                Function<String, String> escapeText, Function<String, String> toVarName,
                BiFunction<String, String, String> toEnumVarName,
                BiFunction<String, Schema, CodegenProperty> fromProperty,
                BiFunction<Schema, String, String> defaultCollectionType,
                Function<Schema, Map<String, Schema>> composedSchemaProperties) {
            this.openAPI = openAPI;
            this.dateLibrary = dateLibrary;
            this.useOneOfInterfaces = useOneOfInterfaces;
            this.escapeText = escapeText;
            this.toVarName = toVarName;
            this.toEnumVarName = toEnumVarName;
            this.fromProperty = fromProperty;
            this.defaultCollectionType = defaultCollectionType;
            this.composedSchemaProperties = composedSchemaProperties;
        }

        private OpenAPI openAPI() {
            return this.openAPI;
        }

        private String dateLibrary() {
            return this.dateLibrary;
        }

        private boolean useOneOfInterfaces() {
            return this.useOneOfInterfaces;
        }

        private Function<String, String> escapeText() {
            return this.escapeText;
        }

        private Function<String, String> toVarName() {
            return this.toVarName;
        }

        private BiFunction<String, String, String> toEnumVarName() {
            return this.toEnumVarName;
        }

        private BiFunction<String, Schema, CodegenProperty> fromProperty() {
            return this.fromProperty;
        }

        private BiFunction<Schema, String, String> defaultCollectionType() {
            return this.defaultCollectionType;
        }

        private Function<Schema, Map<String, Schema>> composedSchemaProperties() {
            return this.composedSchemaProperties;
        }
    }
}
