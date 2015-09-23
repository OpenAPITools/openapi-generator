package io.swagger.codegen;

import java.util.List;
import java.util.Map;

public class CodegenProperty {
    public String baseName, complexType, getter, setter, description, datatype, datatypeWithEnum,
            name, min, max, defaultValue, baseType, containerType;

    public String unescapedDescription;

    /**
     * maxLength validation for strings, see http://json-schema.org/latest/json-schema-validation.html#rfc.section.5.2.1
     */
    public Integer maxLength;
    /**
     * minLength validation for strings, see http://json-schema.org/latest/json-schema-validation.html#rfc.section.5.2.2
     */
    public Integer minLength;
    /**
     * pattern validation for strings, see http://json-schema.org/latest/json-schema-validation.html#rfc.section.5.2.3
     */
    public String pattern;
    /**
     * A free-form property to include an example of an instance for this schema.
     */
    public String example;

    public String jsonSchema;
    public Double minimum;
    public Double maximum;
    public Boolean exclusiveMinimum;
    public Boolean exclusiveMaximum;
    public Boolean hasMore = null, required = null, secondaryParam = null;
    public Boolean isPrimitiveType, isContainer, isNotContainer;
    public boolean isEnum;
    public Boolean isReadOnly = false;
    public List<String> _enum;
    public Map<String, Object> allowableValues;
    public CodegenProperty items;

    @Override
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        final CodegenProperty other = (CodegenProperty) obj;
        if ((this.baseName == null) ? (other.baseName != null) : !this.baseName.equals(other.baseName)) {
            return false;
        }
        if ((this.complexType == null) ? (other.complexType != null) : !this.complexType.equals(other.complexType)) {
            return false;
        }
        if ((this.getter == null) ? (other.getter != null) : !this.getter.equals(other.getter)) {
            return false;
        }
        if ((this.setter == null) ? (other.setter != null) : !this.setter.equals(other.setter)) {
            return false;
        }
        if ((this.description == null) ? (other.description != null) : !this.description.equals(other.description)) {
            return false;
        }
        if ((this.datatype == null) ? (other.datatype != null) : !this.datatype.equals(other.datatype)) {
            return false;
        }
        if ((this.datatypeWithEnum == null) ? (other.datatypeWithEnum != null) : !this.datatypeWithEnum.equals(other.datatypeWithEnum)) {
            return false;
        }
        if ((this.name == null) ? (other.name != null) : !this.name.equals(other.name)) {
            return false;
        }
        if ((this.min == null) ? (other.min != null) : !this.min.equals(other.min)) {
            return false;
        }
        if ((this.max == null) ? (other.max != null) : !this.max.equals(other.max)) {
            return false;
        }
        if ((this.defaultValue == null) ? (other.defaultValue != null) : !this.defaultValue.equals(other.defaultValue)) {
            return false;
        }
        if ((this.baseType == null) ? (other.baseType != null) : !this.baseType.equals(other.baseType)) {
            return false;
        }
        if ((this.containerType == null) ? (other.containerType != null) : !this.containerType.equals(other.containerType)) {
            return false;
        }
        if (this.maxLength != other.maxLength && (this.maxLength == null || !this.maxLength.equals(other.maxLength))) {
            return false;
        }
        if (this.minLength != other.minLength && (this.minLength == null || !this.minLength.equals(other.minLength))) {
            return false;
        }
        if ((this.pattern == null) ? (other.pattern != null) : !this.pattern.equals(other.pattern)) {
            return false;
        }
        if ((this.example == null) ? (other.example != null) : !this.example.equals(other.example)) {
            return false;
        }
        if ((this.jsonSchema == null) ? (other.jsonSchema != null) : !this.jsonSchema.equals(other.jsonSchema)) {
            return false;
        }
        if (this.minimum != other.minimum && (this.minimum == null || !this.minimum.equals(other.minimum))) {
            return false;
        }
        if (this.maximum != other.maximum && (this.maximum == null || !this.maximum.equals(other.maximum))) {
            return false;
        }
        if (this.exclusiveMinimum != other.exclusiveMinimum && (this.exclusiveMinimum == null || !this.exclusiveMinimum.equals(other.exclusiveMinimum))) {
            return false;
        }
        if (this.exclusiveMaximum != other.exclusiveMaximum && (this.exclusiveMaximum == null || !this.exclusiveMaximum.equals(other.exclusiveMaximum))) {
            return false;
        }
        if (this.required != other.required && (this.required == null || !this.required.equals(other.required))) {
            return false;
        }
        if (this.secondaryParam != other.secondaryParam && (this.secondaryParam == null || !this.secondaryParam.equals(other.secondaryParam))) {
            return false;
        }
        if (this.isPrimitiveType != other.isPrimitiveType && (this.isPrimitiveType == null || !this.isPrimitiveType.equals(other.isPrimitiveType))) {
            return false;
        }
        if (this.isContainer != other.isContainer && (this.isContainer == null || !this.isContainer.equals(other.isContainer))) {
            return false;
        }
        if (this.isNotContainer != other.isNotContainer && (this.isNotContainer == null || !this.isNotContainer.equals(other.isNotContainer))) {
            return false;
        }
        if (this.isEnum != other.isEnum) {
            return false;
        }
        if (this._enum != other._enum && (this._enum == null || !this._enum.equals(other._enum))) {
            return false;
        }
        if (this.allowableValues != other.allowableValues && (this.allowableValues == null || !this.allowableValues.equals(other.allowableValues))) {
            return false;
        }
        return true;
    }
}
