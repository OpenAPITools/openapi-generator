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
            isBoolean, isDate, isDateTime, isUuid, isUri, isEmail, isFreeFormObject;
    public boolean isListContainer, isMapContainer, isModelContainer;
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


    @Override
    public String toString() {
        final StringBuffer sb = new StringBuffer("CodegenProperty{");
        sb.append("openApiType='").append(openApiType).append('\'');
        sb.append(", baseName='").append(baseName).append('\'');
        sb.append(", complexType='").append(complexType).append('\'');
        sb.append(", getter='").append(getter).append('\'');
        sb.append(", setter='").append(setter).append('\'');
        sb.append(", description='").append(description).append('\'');
        sb.append(", dataType='").append(dataType).append('\'');
        sb.append(", datatypeWithEnum='").append(datatypeWithEnum).append('\'');
        sb.append(", dataFormat='").append(dataFormat).append('\'');
        sb.append(", name='").append(name).append('\'');
        sb.append(", min='").append(min).append('\'');
        sb.append(", max='").append(max).append('\'');
        sb.append(", defaultValue='").append(defaultValue).append('\'');
        sb.append(", defaultValueWithParam='").append(defaultValueWithParam).append('\'');
        sb.append(", baseType='").append(baseType).append('\'');
        sb.append(", containerType='").append(containerType).append('\'');
        sb.append(", title='").append(title).append('\'');
        sb.append(", unescapedDescription='").append(unescapedDescription).append('\'');
        sb.append(", maxLength=").append(maxLength);
        sb.append(", minLength=").append(minLength);
        sb.append(", pattern='").append(pattern).append('\'');
        sb.append(", example='").append(example).append('\'');
        sb.append(", jsonSchema='").append(jsonSchema).append('\'');
        sb.append(", minimum='").append(minimum).append('\'');
        sb.append(", maximum='").append(maximum).append('\'');
        sb.append(", exclusiveMinimum=").append(exclusiveMinimum);
        sb.append(", exclusiveMaximum=").append(exclusiveMaximum);
        sb.append(", hasMore=").append(hasMore);
        sb.append(", required=").append(required);
        sb.append(", secondaryParam=").append(secondaryParam);
        sb.append(", hasMoreNonReadOnly=").append(hasMoreNonReadOnly);
        sb.append(", isPrimitiveType=").append(isPrimitiveType);
        sb.append(", isModel=").append(isModel);
        sb.append(", isContainer=").append(isContainer);
        sb.append(", isString=").append(isString);
        sb.append(", isNumeric=").append(isNumeric);
        sb.append(", isInteger=").append(isInteger);
        sb.append(", isLong=").append(isLong);
        sb.append(", isNumber=").append(isNumber);
        sb.append(", isFloat=").append(isFloat);
        sb.append(", isDouble=").append(isDouble);
        sb.append(", isByteArray=").append(isByteArray);
        sb.append(", isBinary=").append(isBinary);
        sb.append(", isFile=").append(isFile);
        sb.append(", isBoolean=").append(isBoolean);
        sb.append(", isDate=").append(isDate);
        sb.append(", isDateTime=").append(isDateTime);
        sb.append(", isUuid=").append(isUuid);
        sb.append(", isUri=").append(isUri);
        sb.append(", isEmail=").append(isEmail);
        sb.append(", isFreeFormObject=").append(isFreeFormObject);
        sb.append(", isListContainer=").append(isListContainer);
        sb.append(", isMapContainer=").append(isMapContainer);
        sb.append(", isModelContainer=").append(isModelContainer);
        sb.append(", isEnum=").append(isEnum);
        sb.append(", isReadOnly=").append(isReadOnly);
        sb.append(", isWriteOnly=").append(isWriteOnly);
        sb.append(", isNullable=").append(isNullable);
        sb.append(", isSelfReference=").append(isSelfReference);
        sb.append(", _enum=").append(_enum);
        sb.append(", allowableValues=").append(allowableValues);
        sb.append(", items=").append(items);
        sb.append(", mostInnerItems=").append(mostInnerItems);
        sb.append(", vendorExtensions=").append(vendorExtensions);
        sb.append(", hasValidation=").append(hasValidation);
        sb.append(", isInherited=").append(isInherited);
        sb.append(", discriminatorValue='").append(discriminatorValue).append('\'');
        sb.append(", nameInCamelCase='").append(nameInCamelCase).append('\'');
        sb.append(", nameInSnakeCase='").append(nameInSnakeCase).append('\'');
        sb.append(", enumName='").append(enumName).append('\'');
        sb.append(", maxItems=").append(maxItems);
        sb.append(", minItems=").append(minItems);
        sb.append(", isXmlAttribute=").append(isXmlAttribute);
        sb.append(", xmlPrefix='").append(xmlPrefix).append('\'');
        sb.append(", xmlName='").append(xmlName).append('\'');
        sb.append(", xmlNamespace='").append(xmlNamespace).append('\'');
        sb.append(", isXmlWrapped=").append(isXmlWrapped);
        sb.append('}');
        return sb.toString();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        CodegenProperty that = (CodegenProperty) o;
        return exclusiveMinimum == that.exclusiveMinimum &&
                exclusiveMaximum == that.exclusiveMaximum &&
                hasMore == that.hasMore &&
                required == that.required &&
                secondaryParam == that.secondaryParam &&
                hasMoreNonReadOnly == that.hasMoreNonReadOnly &&
                isPrimitiveType == that.isPrimitiveType &&
                isModel == that.isModel &&
                isContainer == that.isContainer &&
                isString == that.isString &&
                isNumeric == that.isNumeric &&
                isInteger == that.isInteger &&
                isLong == that.isLong &&
                isNumber == that.isNumber &&
                isFloat == that.isFloat &&
                isDouble == that.isDouble &&
                isByteArray == that.isByteArray &&
                isBinary == that.isBinary &&
                isFile == that.isFile &&
                isBoolean == that.isBoolean &&
                isDate == that.isDate &&
                isDateTime == that.isDateTime &&
                isUuid == that.isUuid &&
                isUri == that.isUri &&
                isEmail == that.isEmail &&
                isFreeFormObject == that.isFreeFormObject &&
                isListContainer == that.isListContainer &&
                isMapContainer == that.isMapContainer &&
                isModelContainer == that.isModelContainer &&
                isEnum == that.isEnum &&
                isReadOnly == that.isReadOnly &&
                isWriteOnly == that.isWriteOnly &&
                isNullable == that.isNullable &&
                isSelfReference == that.isSelfReference &&
                hasValidation == that.hasValidation &&
                isInherited == that.isInherited &&
                isXmlAttribute == that.isXmlAttribute &&
                isXmlWrapped == that.isXmlWrapped &&
                Objects.equals(openApiType, that.openApiType) &&
                Objects.equals(baseName, that.baseName) &&
                Objects.equals(complexType, that.complexType) &&
                Objects.equals(getter, that.getter) &&
                Objects.equals(setter, that.setter) &&
                Objects.equals(description, that.description) &&
                Objects.equals(dataType, that.dataType) &&
                Objects.equals(datatypeWithEnum, that.datatypeWithEnum) &&
                Objects.equals(dataFormat, that.dataFormat) &&
                Objects.equals(name, that.name) &&
                Objects.equals(min, that.min) &&
                Objects.equals(max, that.max) &&
                Objects.equals(defaultValue, that.defaultValue) &&
                Objects.equals(defaultValueWithParam, that.defaultValueWithParam) &&
                Objects.equals(baseType, that.baseType) &&
                Objects.equals(containerType, that.containerType) &&
                Objects.equals(title, that.title) &&
                Objects.equals(unescapedDescription, that.unescapedDescription) &&
                Objects.equals(maxLength, that.maxLength) &&
                Objects.equals(minLength, that.minLength) &&
                Objects.equals(pattern, that.pattern) &&
                Objects.equals(example, that.example) &&
                Objects.equals(jsonSchema, that.jsonSchema) &&
                Objects.equals(minimum, that.minimum) &&
                Objects.equals(maximum, that.maximum) &&
                Objects.equals(_enum, that._enum) &&
                Objects.equals(allowableValues, that.allowableValues) &&
                Objects.equals(items, that.items) &&
                Objects.equals(mostInnerItems, that.mostInnerItems) &&
                Objects.equals(vendorExtensions, that.vendorExtensions) &&
                Objects.equals(discriminatorValue, that.discriminatorValue) &&
                Objects.equals(nameInCamelCase, that.nameInCamelCase) &&
                Objects.equals(nameInSnakeCase, that.nameInSnakeCase) &&
                Objects.equals(enumName, that.enumName) &&
                Objects.equals(maxItems, that.maxItems) &&
                Objects.equals(minItems, that.minItems) &&
                Objects.equals(xmlPrefix, that.xmlPrefix) &&
                Objects.equals(xmlName, that.xmlName) &&
                Objects.equals(xmlNamespace, that.xmlNamespace);
    }

    @Override
    public int hashCode() {

        return Objects.hash(openApiType, baseName, complexType, getter, setter, description, dataType,
                datatypeWithEnum, dataFormat, name, min, max, defaultValue, defaultValueWithParam, baseType,
                containerType, title, unescapedDescription, maxLength, minLength, pattern, example, jsonSchema,
                minimum, maximum, exclusiveMinimum, exclusiveMaximum, hasMore, required, secondaryParam,
                hasMoreNonReadOnly, isPrimitiveType, isModel, isContainer, isString, isNumeric, isInteger,
                isLong, isNumber, isFloat, isDouble, isByteArray, isBinary, isFile, isBoolean, isDate, isDateTime,
                isUuid, isUri, isEmail, isFreeFormObject, isListContainer, isMapContainer, isModelContainer, isEnum,
                isReadOnly, isWriteOnly, isNullable, isSelfReference, _enum, allowableValues, items, mostInnerItems,
                vendorExtensions, hasValidation, isInherited, discriminatorValue, nameInCamelCase, nameInSnakeCase,
                enumName, maxItems, minItems, isXmlAttribute, xmlPrefix, xmlName, xmlNamespace, isXmlWrapped);
    }
}
