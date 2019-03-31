/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen;

import java.util.*;

public class CodegenProperty implements Cloneable {
    public String openApiType, baseName, complexType, getter, setter, description, dataType,
            datatypeWithEnum, dataFormat, name, min, max, defaultValue, defaultValueWithParam,
            baseType, containerType, title;

    /**
     * The 'description' string without escape charcters needed by some programming languages/targets
     */
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
    public String minimum;
    public String maximum;
    public boolean exclusiveMinimum;
    public boolean exclusiveMaximum;
    public boolean hasMore, required, secondaryParam;
    public boolean hasMoreNonReadOnly; // for model constructor, true if next property is not readonly
    public boolean isPrimitiveType, isModel, isContainer;
    public boolean isString, isNumeric, isInteger, isLong, isNumber, isFloat, isDouble, isByteArray, isBinary, isFile,
            isBoolean, isDate, isDateTime, isUuid, isEmail, isFreeFormObject;
    public boolean isListContainer, isMapContainer;
    public boolean isEnum;
    public boolean isReadOnly;
    public boolean isWriteOnly;
    public boolean isNullable;
    public boolean isSelfReference;
    public List<String> _enum;
    public Map<String, Object> allowableValues;
    public CodegenProperty items;
    public CodegenProperty mostInnerItems;
    public Map<String, Object> vendorExtensions = new HashMap<String, Object>();
    public boolean hasValidation; // true if pattern, maximum, etc are set (only used in the mustache template)
    public boolean isInherited;
    public String discriminatorValue;
    public String nameInCamelCase; // property name in camel case
    public String nameInSnakeCase; // property name in upper snake case
    // enum name based on the property name, usually use as a prefix (e.g. VAR_NAME) for enum name (e.g. VAR_NAME_VALUE1)
    public String enumName;
    public Integer maxItems;
    public Integer minItems;

    // XML
    public boolean isXmlAttribute = false;
    public String xmlPrefix;
    public String xmlName;
    public String xmlNamespace;
    public boolean isXmlWrapped = false;

    public String getBaseName() {
        return baseName;
    }

    public void setBaseName(String baseName) {
        this.baseName = baseName;
    }

    public String getComplexType() {
        return complexType;
    }

    public void setComplexType(String complexType) {
        this.complexType = complexType;
    }

    public String getGetter() {
        return getter;
    }

    public void setGetter(String getter) {
        this.getter = getter;
    }

    public String getSetter() {
        return setter;
    }

    public void setSetter(String setter) {
        this.setter = setter;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    /**
     * @return dataType
     * @deprecated since version 3.0.0, use {@link #getDataType()} instead.<br>
     * May be removed with the next major release (4.0)
     */
    @Deprecated
    public String getDatatype() {
        return getDataType();
    }

    public String getDataType() {
        return dataType;
    }

    public void setDatatype(String datatype) {
        this.dataType = datatype;
    }

    public String getDatatypeWithEnum() {
        return datatypeWithEnum;
    }

    public void setDatatypeWithEnum(String datatypeWithEnum) {
        this.datatypeWithEnum = datatypeWithEnum;
    }

    public String getDataFormat() {
        return dataFormat;
    }

    public void setDataFormat(String dataFormat) {
        this.dataFormat = dataFormat;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getMin() {
        return min;
    }

    public void setMin(String min) {
        this.min = min;
    }

    public String getMax() {
        return max;
    }

    public void setMax(String max) {
        this.max = max;
    }

    public String getDefaultValue() {
        return defaultValue;
    }

    public void setDefaultValue(String defaultValue) {
        this.defaultValue = defaultValue;
    }

    public String getDefaultValueWithParam() {
        return defaultValueWithParam;
    }

    public void setDefaultValueWithParam(String defaultValueWithParam) {
        this.defaultValueWithParam = defaultValueWithParam;
    }

    public String getBaseType() {
        return baseType;
    }

    public void setBaseType(String baseType) {
        this.baseType = baseType;
    }

    public String getContainerType() {
        return containerType;
    }

    public void setContainerType(String containerType) {
        this.containerType = containerType;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getUnescapedDescription() {
        return unescapedDescription;
    }

    public void setUnescapedDescription(String unescapedDescription) {
        this.unescapedDescription = unescapedDescription;
    }

    public Integer getMaxLength() {
        return maxLength;
    }

    public void setMaxLength(Integer maxLength) {
        this.maxLength = maxLength;
    }

    public Integer getMinLength() {
        return minLength;
    }

    public void setMinLength(Integer minLength) {
        this.minLength = minLength;
    }

    public String getPattern() {
        return pattern;
    }

    public void setPattern(String pattern) {
        this.pattern = pattern;
    }

    public String getExample() {
        return example;
    }

    public void setExample(String example) {
        this.example = example;
    }

    public String getJsonSchema() {
        return jsonSchema;
    }

    public void setJsonSchema(String jsonSchema) {
        this.jsonSchema = jsonSchema;
    }

    public String getMinimum() {
        return minimum;
    }

    public void setMinimum(String minimum) {
        this.minimum = minimum;
    }

    public String getMaximum() {
        return maximum;
    }

    public void setMaximum(String maximum) {
        this.maximum = maximum;
    }

    public boolean getExclusiveMinimum() {
        return exclusiveMinimum;
    }

    public void setExclusiveMinimum(boolean exclusiveMinimum) {
        this.exclusiveMinimum = exclusiveMinimum;
    }

    public boolean getIExclusiveMaximum() {
        return exclusiveMaximum;
    }

    public void setExclusiveMaximum(boolean exclusiveMaximum) {
        this.exclusiveMaximum = exclusiveMaximum;
    }

    public boolean getRequired() {
        return required;
    }

    public void setRequired(boolean required) {
        this.required = required;
    }

    public boolean getSecondaryParam() {
        return secondaryParam;
    }

    public void setSecondaryParam(boolean secondaryParam) {
        this.secondaryParam = secondaryParam;
    }

    public List<String> get_enum() {
        return _enum;
    }

    public void set_enum(List<String> _enum) {
        this._enum = _enum;
    }

    public Map<String, Object> getAllowableValues() {
        return allowableValues;
    }

    public void setAllowableValues(Map<String, Object> allowableValues) {
        this.allowableValues = allowableValues;
    }

    public CodegenProperty getItems() {
        return items;
    }

    public void setItems(CodegenProperty items) {
        this.items = items;
    }

    public Map<String, Object> getVendorExtensions() {
        return vendorExtensions;
    }

    public void setVendorExtensions(Map<String, Object> vendorExtensions) {
        this.vendorExtensions = vendorExtensions;
    }

    public String getNameInCamelCase() {
        return nameInCamelCase;
    }

    public void setNameInCamelCase(String nameInCamelCase) {
        this.nameInCamelCase = nameInCamelCase;
    }

    public String getNameInSnakeCase() {
        return nameInSnakeCase;
    }

    public String getEnumName() {
        return enumName;
    }

    public void setEnumName(String enumName) {
        this.enumName = enumName;
    }

    public Integer getMaxItems() {
        return maxItems;
    }

    public void setMaxItems(Integer maxItems) {
        this.maxItems = maxItems;
    }

    public Integer getMinItems() {
        return minItems;
    }

    public void setMinItems(Integer minItems) {
        this.minItems = minItems;
    }

    public String getXmlPrefix() {
        return xmlPrefix;
    }

    public void setXmlPrefix(String xmlPrefix) {
        this.xmlPrefix = xmlPrefix;
    }

    public String getXmlName() {
        return xmlName;
    }

    public void setXmlName(String xmlName) {
        this.xmlName = xmlName;
    }

    public String getXmlNamespace() {
        return xmlNamespace;
    }

    public void setXmlNamespace(String xmlNamespace) {
        this.xmlNamespace = xmlNamespace;
    }

    @Override
    public int hashCode() {
        return Objects.hash(
            _enum,
            allowableValues,
            openApiType,
            baseName,
            baseType,
            complexType,
            containerType,
            dataType,
            datatypeWithEnum,
            dataFormat,
            defaultValue,
            defaultValueWithParam,
            description,
            title,
            example,
            exclusiveMaximum,
            exclusiveMinimum,
            getter,
            hasMore,
            hasMoreNonReadOnly,
            isContainer,
            isEnum,
            isPrimitiveType,
            isModel,
            isReadOnly,
            isWriteOnly,
            isNullable,
            isSelfReference,
            items,
            mostInnerItems,
            jsonSchema,
            max,
            maxLength,
            maximum,
            min,
            minLength,
            minimum,
            name,
            pattern,
            required,
            secondaryParam,
            setter,
            unescapedDescription,
            vendorExtensions,
            hasValidation,
            isString,
            isNumeric,
            isInteger,
            isLong,
            isNumber,
            isFloat,
            isDouble,
            isByteArray,
            isBinary,
            isFile,
            isBoolean,
            isDate,
            isDateTime,
            isUuid,
            isEmail,
            isFreeFormObject,
            isMapContainer,
            isListContainer,
            isInherited,
            discriminatorValue,
            nameInCamelCase,
            nameInSnakeCase,
            enumName,
            maxItems,
            minItems,
            isXmlAttribute,
            xmlPrefix,
            xmlName,
            xmlNamespace,
            isXmlWrapped);
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

        return Objects.equals(baseName, other.baseName) &&
            Objects.equals(openApiType, other.openApiType) &&
            Objects.equals(complexType, other.complexType) &&
            Objects.equals(getter, other.getter) &&
            Objects.equals(setter, other.setter) &&
            Objects.equals(description, other.description) &&
            Objects.equals(title, other.title) &&
            Objects.equals(dataType, other.dataType) &&
            Objects.equals(datatypeWithEnum, other.datatypeWithEnum) &&
            Objects.equals(dataFormat, other.dataFormat) &&
            Objects.equals(name, other.name) &&
            Objects.equals(min, other.min) &&
            Objects.equals(max, other.max) &&
            Objects.equals(defaultValue, other.defaultValue) &&
            Objects.equals(baseType, other.baseType) &&
            Objects.equals(containerType, other.containerType) &&
            Objects.equals(maxLength, other.maxLength) &&
            Objects.equals(minLength, other.minLength) &&
            Objects.equals(pattern, other.pattern) &&
            Objects.equals(example, other.example) &&
            Objects.equals(jsonSchema, other.jsonSchema) &&
            Objects.equals(minimum, other.minimum) &&
            Objects.equals(maximum, other.maximum) &&
            Objects.equals(exclusiveMinimum, other.exclusiveMinimum) &&
            Objects.equals(exclusiveMaximum, other.exclusiveMaximum) &&
            Objects.equals(required, other.required) &&
            Objects.equals(secondaryParam, other.secondaryParam) &&
            Objects.equals(isPrimitiveType, other.isPrimitiveType) &&
            Objects.equals(isModel, other.isModel) &&
            Objects.equals(isContainer, other.isContainer) &&
            Objects.equals(isEnum, other.isEnum) &&
            Objects.equals(isReadOnly, other.isReadOnly) &&
            Objects.equals(isWriteOnly, other.isWriteOnly) &&
            Objects.equals(isNullable, other.isNullable) &&
            Objects.equals(isSelfReference, other.isSelfReference) &&
            Objects.equals(_enum, other._enum) &&
            Objects.equals(allowableValues, other.allowableValues) &&
            Objects.equals(vendorExtensions, other.vendorExtensions) &&
            Objects.equals(hasValidation, other.hasValidation) &&
            Objects.equals(isString, other.isString) &&
            Objects.equals(isNumeric, other.isNumeric) &&
            Objects.equals(isInteger, other.isInteger) &&
            Objects.equals(isLong, other.isLong) &&
            Objects.equals(isNumber, other.isNumber) &&
            Objects.equals(isFloat, other.isFloat) &&
            Objects.equals(isDouble, other.isDouble) &&
            Objects.equals(isByteArray, other.isByteArray) &&
            Objects.equals(isBoolean, other.isBoolean) &&
            Objects.equals(isDate, other.isDate) &&
            Objects.equals(isDateTime, other.isDateTime) &&
            Objects.equals(isUuid, other.isUuid) &&
            Objects.equals(isEmail, other.isEmail) &&
            Objects.equals(isFreeFormObject, other.isFreeFormObject) &&
            Objects.equals(isBinary, other.isBinary) &&
            Objects.equals(isFile, other.isFile) &&
            Objects.equals(isListContainer, other.isListContainer) &&
            Objects.equals(isMapContainer, other.isMapContainer) &&
            Objects.equals(isInherited, other.isInherited) &&
            Objects.equals(discriminatorValue, other.discriminatorValue) &&
            Objects.equals(nameInCamelCase, other.nameInCamelCase) &&
            Objects.equals(nameInSnakeCase, other.nameInSnakeCase) &&
            Objects.equals(enumName, other.enumName) &&
            Objects.equals(maxItems, other.maxItems) &&
            Objects.equals(minItems, other.minItems) &&
            Objects.equals(isXmlAttribute, other.isXmlAttribute) &&
            Objects.equals(xmlPrefix, other.xmlPrefix) &&
            Objects.equals(xmlName, other.xmlName) &&
            Objects.equals(xmlNamespace, other.xmlNamespace) &&
            Objects.equals(isXmlWrapped, other.isXmlWrapped);
    }

    @Override
    public CodegenProperty clone() {
        try {
            CodegenProperty cp = (CodegenProperty) super.clone();
            if (this._enum != null) {
                cp._enum = new ArrayList<String>(this._enum);
            }
            if (this.allowableValues != null) {
                cp.allowableValues = new HashMap<String, Object>(this.allowableValues);
            }
            if (this.items != null) {
                cp.items = this.items;
            }
            if (this.mostInnerItems != null) {
                cp.mostInnerItems = this.mostInnerItems;
            }
            if (this.vendorExtensions != null) {
                cp.vendorExtensions = new HashMap<String, Object>(this.vendorExtensions);
            }

            return cp;
        } catch (CloneNotSupportedException e) {
            throw new IllegalStateException(e);
        }
    }

    @java.lang.Override
    public java.lang.String toString() {
        return "CodegenProperty{" +
                "baseName='" + baseName + '\'' +
                ", openApiType='" + openApiType + '\'' +
                ", complexType='" + complexType + '\'' +
                ", getter='" + getter + '\'' +
                ", setter='" + setter + '\'' +
                ", description='" + description + '\'' +
                ", datatype='" + dataType + '\'' +
                ", datatypeWithEnum='" + datatypeWithEnum + '\'' +
                ", dataFormat='" + dataFormat + '\'' +
                ", name='" + name + '\'' +
                ", min='" + min + '\'' +
                ", max='" + max + '\'' +
                ", defaultValue='" + defaultValue + '\'' +
                ", defaultValueWithParam='" + defaultValueWithParam + '\'' +
                ", baseType='" + baseType + '\'' +
                ", containerType='" + containerType + '\'' +
                ", title='" + title + '\'' +
                ", unescapedDescription='" + unescapedDescription + '\'' +
                ", maxLength=" + maxLength +
                ", minLength=" + minLength +
                ", pattern='" + pattern + '\'' +
                ", example='" + example + '\'' +
                ", jsonSchema='" + jsonSchema + '\'' +
                ", minimum='" + minimum + '\'' +
                ", maximum='" + maximum + '\'' +
                ", exclusiveMinimum=" + exclusiveMinimum +
                ", exclusiveMaximum=" + exclusiveMaximum +
                ", hasMore=" + hasMore +
                ", required=" + required +
                ", secondaryParam=" + secondaryParam +
                ", hasMoreNonReadOnly=" + hasMoreNonReadOnly +
                ", isPrimitiveType=" + isPrimitiveType +
                ", isModel=" + isModel +
                ", isContainer=" + isContainer +
                ", isString=" + isString +
                ", isNumeric=" + isNumeric +
                ", isInteger=" + isInteger +
                ", isLong=" + isLong +
                ", isNumber=" + isNumber +
                ", isFloat=" + isFloat +
                ", isDouble=" + isDouble +
                ", isByteArray=" + isByteArray +
                ", isBinary=" + isBinary +
                ", isFile=" + isFile +
                ", isBoolean=" + isBoolean +
                ", isDate=" + isDate +
                ", isDateTime=" + isDateTime +
                ", isUuid=" + isUuid +
                ", isEmail=" + isEmail +
                ", isFreeFormObject=" + isFreeFormObject +
                ", isListContainer=" + isListContainer +
                ", isMapContainer=" + isMapContainer +
                ", isEnum=" + isEnum +
                ", isReadOnly=" + isReadOnly +
                ", isWriteOnly=" + isWriteOnly +
                ", isNullable=" + isNullable +
                ", isSelfReference=" + isSelfReference +
                ", _enum=" + _enum +
                ", allowableValues=" + allowableValues +
                ", items=" + items +
                ", mostInnerItems=" + mostInnerItems +
                ", vendorExtensions=" + vendorExtensions +
                ", hasValidation=" + hasValidation +
                ", isInherited=" + isInherited +
                ", discriminatorValue='" + discriminatorValue + '\'' +
                ", nameInCamelCase='" + nameInCamelCase + '\'' +
                ", enumName='" + enumName + '\'' +
                ", maxItems=" + maxItems +
                ", minItems=" + minItems +
                ", isXmlAttribute=" + isXmlAttribute +
                ", xmlPrefix='" + xmlPrefix + '\'' +
                ", xmlName='" + xmlName + '\'' +
                ", xmlNamespace='" + xmlNamespace + '\'' +
                ", isXmlWrapped=" + isXmlWrapped +
                '}';
    }


}
