package io.swagger.codegen;

import java.util.List;
import java.util.Map;

public class CodegenProperty {
    public String baseName, complexType, getter, setter, description, datatype, datatypeWithEnum,
            name, min, max, defaultValue, defaultValueWithParam, baseType, containerType;

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
    public Boolean hasMore, required, secondaryParam;
    public Boolean isPrimitiveType, isContainer, isNotContainer;
    public boolean isEnum;
    public Boolean isReadOnly = false;
    public List<String> _enum;
    public Map<String, Object> allowableValues;
    public CodegenProperty items;
    public Map<String, Object> vendorExtensions;

    @Override
    public int hashCode()
    {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((_enum == null) ? 0 : _enum.hashCode());
        result = prime * result + ((allowableValues == null) ? 0 : allowableValues.hashCode());
        result = prime * result + ((baseName == null) ? 0 : baseName.hashCode());
        result = prime * result + ((baseType == null) ? 0 : baseType.hashCode());
        result = prime * result + ((complexType == null) ? 0 : complexType.hashCode());
        result = prime * result + ((containerType == null) ? 0 : containerType.hashCode());
        result = prime * result + ((datatype == null) ? 0 : datatype.hashCode());
        result = prime * result + ((datatypeWithEnum == null) ? 0 : datatypeWithEnum.hashCode());
        result = prime * result + ((defaultValue == null) ? 0 : defaultValue.hashCode());
        result = prime * result + ((defaultValueWithParam == null) ? 0 : defaultValueWithParam.hashCode());
        result = prime * result + ((description == null) ? 0 : description.hashCode());
        result = prime * result + ((example == null) ? 0 : example.hashCode());
        result = prime * result + ((exclusiveMaximum == null) ? 0 : exclusiveMaximum.hashCode());
        result = prime * result + ((exclusiveMinimum == null) ? 0 : exclusiveMinimum.hashCode());
        result = prime * result + ((getter == null) ? 0 : getter.hashCode());
        result = prime * result + ((hasMore == null) ? 0 : hasMore.hashCode());
        result = prime * result + ((isContainer == null) ? 0 : isContainer.hashCode());
        result = prime * result + (isEnum ? 1231 : 1237);
        result = prime * result + ((isNotContainer == null) ? 0 : isNotContainer.hashCode());
        result = prime * result + ((isPrimitiveType == null) ? 0 : isPrimitiveType.hashCode());
        result = prime * result + ((isReadOnly == null) ? 0 : isReadOnly.hashCode());
        result = prime * result + ((items == null) ? 0 : items.hashCode());
        result = prime * result + ((jsonSchema == null) ? 0 : jsonSchema.hashCode());
        result = prime * result + ((max == null) ? 0 : max.hashCode());
        result = prime * result + ((maxLength == null) ? 0 : maxLength.hashCode());
        result = prime * result + ((maximum == null) ? 0 : maximum.hashCode());
        result = prime * result + ((min == null) ? 0 : min.hashCode());
        result = prime * result + ((minLength == null) ? 0 : minLength.hashCode());
        result = prime * result + ((minimum == null) ? 0 : minimum.hashCode());
        result = prime * result + ((name == null) ? 0 : name.hashCode());
        result = prime * result + ((pattern == null) ? 0 : pattern.hashCode());
        result = prime * result + ((required == null) ? 0 : required.hashCode());
        result = prime * result + ((secondaryParam == null) ? 0 : secondaryParam.hashCode());
        result = prime * result + ((setter == null) ? 0 : setter.hashCode());
        result = prime * result + ((unescapedDescription == null) ? 0 : unescapedDescription.hashCode());
        result = prime * result + ((vendorExtensions == null) ? 0 : vendorExtensions.hashCode());
        return result;
    }

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
        if (this.vendorExtensions != other.vendorExtensions && (this.vendorExtensions == null || !this.vendorExtensions.equals(other.vendorExtensions))) {
            return false;
        }
        return true;
    }
}
