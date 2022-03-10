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

package org.openapitools.codegen;

import java.util.*;

public class CodegenProperty implements Cloneable, IJsonSchemaValidationProperties {
    /**
     * The value of the 'type' attribute in the OpenAPI schema.
     * The per-language codegen logic may change to a language-specific type.
     */
    public String openApiType;
    public String baseName;
    public String complexType;
    public String getter;
    public String setter;
    /**
     * The value of the 'description' attribute in the OpenAPI schema.
     */
    public String description;
    /**
     * The language-specific data type for this property. For example, the OpenAPI type 'integer'
     * may be represented as 'int', 'int32', 'Integer', etc, depending on the programming language.
     */
    public String dataType;
    public String datatypeWithEnum;
    public String dataFormat;
    /**
     * The name of this property in the OpenAPI schema.
     */
    public String name;
    public String min; // TODO: is this really used?
    public String max; // TODO: is this really used?
    public String defaultValue;
    public String defaultValueWithParam;
    public String baseType;
    public String containerType;
    /**
     * The value of the 'title' attribute in the OpenAPI schema.
     */
    public String title;

    /**
     * The 'description' string without escape characters needed by some programming languages/targets
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
    /**
     * The value of the 'minimum' attribute in the OpenAPI schema.
     * The value of "minimum" MUST be a number, representing an inclusive lower limit for a numeric instance.
     */
    public String minimum;
    /**
     * The value of the 'maximum' attribute in the OpenAPI schema.
     * The value of "maximum" MUST be a number, representing an inclusive upper limit for a numeric instance.
     */
    public String maximum;
    /**
     * The value of the 'multipleOf' attribute in the OpenAPI schema.
     * The value of "multipleOf" MUST be a number, strictly greater than 0.
     */
    public Number multipleOf;
    /**
     * The value of the 'exclusiveMinimum' attribute in the OpenAPI schema.
     * The value of "exclusiveMinimum" MUST be number, representing an exclusive lower limit for a numeric instance.
     */
    public boolean exclusiveMinimum;
    /**
     * The value of the 'exclusiveMaximum' attribute in the OpenAPI schema.
     * The value of "exclusiveMaximum" MUST be number, representing an exclusive upper limit for a numeric instance.
     */
    public boolean exclusiveMaximum;
    public boolean required;
    public boolean deprecated;
    public boolean hasMoreNonReadOnly; // for model constructor, true if next property is not readonly
    public boolean isPrimitiveType;
    public boolean isModel;
    /**
     * True if this property is an array of items or a map container.
     * See:
     * - ModelUtils.isArraySchema()
     * - ModelUtils.isMapSchema()
     */
    public boolean isContainer;
    public boolean isString;
    public boolean isNumeric;
    public boolean isInteger;
    public boolean isShort;
    public boolean isLong;
    public boolean isUnboundedInteger;
    public boolean isNumber;
    public boolean isFloat;
    public boolean isDouble;
    public boolean isDecimal;
    public boolean isByteArray;
    public boolean isBinary;
    public boolean isFile;
    public boolean isBoolean;
    public boolean isDate; // full-date notation as defined by RFC 3339, section 5.6, for example, 2017-07-21
    public boolean isDateTime; // the date-time notation as defined by RFC 3339, section 5.6, for example, 2017-07-21T17:32:28Z
    public boolean isUuid;
    public boolean isUri;
    public boolean isEmail;
    public boolean isNull;
    /**
     * The type is a free-form object, i.e. it is a map of string to values with no declared properties.
     * A OAS free-form schema may include the 'additionalProperties' attribute, which puts a constraint
     * on the type of the undeclared properties.
     */
    public boolean isFreeFormObject;
    /**
     * The 'type' in the OAS schema is unspecified (i.e. not set). The value can be number, integer, string, object or array.
     * If the nullable attribute is set to true, the 'null' value is valid.
     */
    public boolean isAnyType;
    public boolean isArray;
    public boolean isMap;
    public boolean isEnum;
    public boolean isReadOnly;
    public boolean isWriteOnly;
    public boolean isNullable;
    public boolean isSelfReference;
    public boolean isCircularReference;
    public boolean isDiscriminator;
    public List<String> _enum;
    public Map<String, Object> allowableValues;
    // If 'additionalProperties' is not set, items is null.
    // If 'additionalProperties' is set to a type or refers to a type, 'items' provides the type information for
    // the undeclared properties.
    public CodegenProperty items;
    public CodegenProperty additionalProperties;
    public List<CodegenProperty> vars = new ArrayList<CodegenProperty>(); // all properties (without parent's properties)
    public List<CodegenProperty> requiredVars = new ArrayList<>();
    public CodegenProperty mostInnerItems;
    public Map<String, Object> vendorExtensions = new HashMap<String, Object>();
    public boolean hasValidation; // true if pattern, maximum, etc are set (only used in the mustache template)
    public boolean isInherited;
    public String discriminatorValue;
    public String nameInLowerCase; // property name in lower case
    public String nameInCamelCase; // property name in camel case
    public String nameInSnakeCase; // property name in upper snake case
    // enum name based on the property name, usually use as a prefix (e.g. VAR_NAME) for enum name (e.g. VAR_NAME_VALUE1)
    public String enumName;
    public Integer maxItems;
    public Integer minItems;

    private Integer maxProperties;
    private Integer minProperties;
    private boolean uniqueItems;

    // XML
    public boolean isXmlAttribute = false;
    public String xmlPrefix;
    public String xmlName;
    public String xmlNamespace;
    public boolean isXmlWrapped = false;
    private boolean additionalPropertiesIsAnyType;
    private boolean hasVars;
    private boolean hasRequired;
    private boolean hasDiscriminatorWithNonEmptyMapping;
    private CodegenComposedSchemas composedSchemas = null;
    private boolean hasMultipleTypes = false;

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

    @Override
    public Integer getMaxLength() {
        return maxLength;
    }

    @Override
    public void setMaxLength(Integer maxLength) {
        this.maxLength = maxLength;
    }

    @Override
    public Integer getMinLength() {
        return minLength;
    }

    @Override
    public void setMinLength(Integer minLength) {
        this.minLength = minLength;
    }

    @Override
    public String getPattern() {
        return pattern;
    }

    @Override
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

    @Override
    public String getMinimum() {
        return minimum;
    }

    @Override
    public void setMinimum(String minimum) {
        this.minimum = minimum;
    }

    @Override
    public boolean getExclusiveMaximum() {
        return this.exclusiveMaximum;
    }

    @Override
    public String getMaximum() {
        return maximum;
    }

    @Override
    public void setMaximum(String maximum) {
        this.maximum = maximum;
    }

    @Override
    public boolean getExclusiveMinimum() {
        return exclusiveMinimum;
    }

    @Override
    public void setExclusiveMinimum(boolean exclusiveMinimum) {
        this.exclusiveMinimum = exclusiveMinimum;
    }

    public boolean getIExclusiveMaximum() {
        return exclusiveMaximum;
    }

    @Override
    public void setExclusiveMaximum(boolean exclusiveMaximum) {
        this.exclusiveMaximum = exclusiveMaximum;
    }

    public boolean getRequired() {
        return required;
    }

    public void setRequired(boolean required) {
        this.required = required;
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

    @Override
    public CodegenProperty getItems() {
        return items;
    }

    @Override
    public void setItems(CodegenProperty items) {
        this.items = items;
    }

    @Override
    public CodegenProperty getAdditionalProperties() {
        return additionalProperties;
    }

    @Override
    public void setAdditionalProperties(CodegenProperty additionalProperties) {
        this.additionalProperties = additionalProperties;
    }

    @Override
    public boolean getIsModel() {
        return isModel;
    }

    @Override
    public void setIsModel(boolean isModel) {
        this.isModel = isModel;
    }

    @Override
    public boolean getIsDate() {
        return isDate;
    }

    @Override
    public void setIsDate(boolean isDate) {
        this.isDate = isDate;
    }

    @Override
    public boolean getIsDateTime() {
        return isDateTime;
    }

    @Override
    public void setIsDateTime(boolean isDateTime) {
        this.isDateTime = isDateTime;
    }

    @Override
    public boolean getIsMap() {
        return isMap;
    }

    @Override
    public void setIsMap(boolean isMap) {
        this.isMap = isMap;
    }

    @Override
    public boolean getIsArray() {
        return isArray;
    }

    @Override
    public void setIsArray(boolean isArray) {
        this.isArray = isArray;
    }

    @Override
    public boolean getIsShort() {
        return isShort;
    }

    @Override
    public void setIsShort(boolean isShort) {
        this.isShort = isShort;
    }

    @Override
    public boolean getIsBoolean() {
        return isBoolean;
    }

    @Override
    public void setIsBoolean(boolean isBoolean) {
        this.isBoolean = isBoolean;
    }

    @Override
    public boolean getIsUnboundedInteger() {
        return isUnboundedInteger;
    }

    @Override
    public void setIsUnboundedInteger(boolean isUnboundedInteger) {
        this.isUnboundedInteger = isUnboundedInteger;
    }

    @Override
    public boolean getIsPrimitiveType() {
        return isPrimitiveType;
    }

    @Override
    public void setIsPrimitiveType(boolean isPrimitiveType) {
        this.isPrimitiveType = isPrimitiveType;
    }

    public Map<String, Object> getVendorExtensions() {
        return vendorExtensions;
    }

    public void setVendorExtensions(Map<String, Object> vendorExtensions) {
        this.vendorExtensions = vendorExtensions;
    }

    public String getNameInLowerCase() {
        return nameInLowerCase;
    }

    public void setNameInLowerCase(String nameInLowerCase) {
        this.nameInLowerCase = nameInLowerCase;
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

    @Override
    public Integer getMaxItems() {
        return maxItems;
    }

    @Override
    public void setMaxItems(Integer maxItems) {
        this.maxItems = maxItems;
    }

    @Override
    public Integer getMinItems() {
        return minItems;
    }

    @Override
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
    public void setComposedSchemas(CodegenComposedSchemas composedSchemas) {
        this.composedSchemas = composedSchemas;
    }

    @Override
    public CodegenComposedSchemas getComposedSchemas() {
        return composedSchemas;
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
            if (this.additionalProperties != null) {
                cp.additionalProperties = this.additionalProperties;
            }
            if (this.vars != null) {
                cp.vars = this.vars;
            }
            if (this.requiredVars != null) {
                cp.requiredVars = this.requiredVars;
            }
            if (this.mostInnerItems != null) {
                cp.mostInnerItems = this.mostInnerItems;
            }
            if (this.vendorExtensions != null) {
                cp.vendorExtensions = new HashMap<String, Object>(this.vendorExtensions);
            }
            if (this.composedSchemas != null) {
                cp.composedSchemas = this.composedSchemas;
            }

            return cp;
        } catch (CloneNotSupportedException e) {
            throw new IllegalStateException(e);
        }
    }

    @Override
    public boolean getUniqueItems() {
        return uniqueItems;
    }

    @Override
    public void setUniqueItems(boolean uniqueItems) {
        this.uniqueItems = uniqueItems;
    }

    @Override
    public Integer getMinProperties() {
        return minProperties;
    }

    @Override
    public void setMinProperties(Integer minProperties) {
        this.minProperties = minProperties;
    }

    @Override
    public Integer getMaxProperties() {
        return maxProperties;
    }

    @Override
    public void setMaxProperties(Integer maxProperties) {
        this.maxProperties = maxProperties;
    }

    @Override
    public Number getMultipleOf() {
        return multipleOf;
    }

    @Override
    public void setMultipleOf(Number multipleOf) {
        this.multipleOf = multipleOf;
    }

    @Override
    public List<CodegenProperty> getVars() {
        return vars;
    }

    @Override
    public void setVars(List<CodegenProperty> vars) {
        this.vars = vars;
    }

    @Override
    public List<CodegenProperty> getRequiredVars() {
        return requiredVars;
    }

    @Override
    public void setRequiredVars(List<CodegenProperty> requiredVars) {
        this.requiredVars = requiredVars;
    }

    @Override
    public boolean getIsNull() {
        return isNull;
    }

    @Override
    public void setIsNull(boolean isNull) {
        this.isNull = isNull;
    }

    @Override
    public boolean getHasValidation() {
        return hasValidation;
    }

    @Override
    public void setHasValidation(boolean hasValidation) {
        this.hasValidation = hasValidation;
    }

    @Override
    public boolean getAdditionalPropertiesIsAnyType() {
        return additionalPropertiesIsAnyType;
    }

    @Override
    public void setAdditionalPropertiesIsAnyType(boolean additionalPropertiesIsAnyType) {
        this.additionalPropertiesIsAnyType = additionalPropertiesIsAnyType;
    }

    @Override
    public boolean getHasVars() {
        return this.hasVars;
    }

    @Override
    public void setHasVars(boolean hasVars) {
        this.hasVars = hasVars;
    }

    @Override
    public boolean getHasRequired() {
        return this.hasRequired;
    }

    @Override
    public void setHasRequired(boolean hasRequired) {
        this.hasRequired = hasRequired;
    }

    @Override
    public boolean getHasDiscriminatorWithNonEmptyMapping() {
        return hasDiscriminatorWithNonEmptyMapping;
    }

    ;

    @Override
    public void setHasDiscriminatorWithNonEmptyMapping(boolean hasDiscriminatorWithNonEmptyMapping) {
        this.hasDiscriminatorWithNonEmptyMapping = hasDiscriminatorWithNonEmptyMapping;
    }

    public boolean getHasItems() {
        return this.items != null;
    }

    @Override
    public boolean getIsString() {
        return isString;
    }

    @Override
    public void setIsString(boolean isString) {
        this.isString = isString;
    }

    @Override
    public boolean getIsNumber() {
        return isNumber;
    }

    @Override
    public void setIsNumber(boolean isNumber) {
        this.isNumber = isNumber;
    }

    @Override
    public boolean getIsAnyType() {
        return isAnyType;
    }

    @Override
    public void setIsAnyType(boolean isAnyType) {
        this.isAnyType = isAnyType;
    }

    @Override
    public boolean getHasMultipleTypes() {
        return hasMultipleTypes;
    }

    @Override
    public void setHasMultipleTypes(boolean hasMultipleTypes) {
        this.hasMultipleTypes = hasMultipleTypes;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("CodegenProperty{");
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
        sb.append(", required=").append(required);
        sb.append(", deprecated=").append(deprecated);
        sb.append(", hasMoreNonReadOnly=").append(hasMoreNonReadOnly);
        sb.append(", isPrimitiveType=").append(isPrimitiveType);
        sb.append(", isModel=").append(isModel);
        sb.append(", isContainer=").append(isContainer);
        sb.append(", isString=").append(isString);
        sb.append(", isNumeric=").append(isNumeric);
        sb.append(", isInteger=").append(isInteger);
        sb.append(", isShort=").append(isShort);
        sb.append(", isLong=").append(isLong);
        sb.append(", isUnboundedInteger=").append(isUnboundedInteger);
        sb.append(", isNumber=").append(isNumber);
        sb.append(", isFloat=").append(isFloat);
        sb.append(", isDouble=").append(isDouble);
        sb.append(", isDecimal=").append(isDecimal);
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
        sb.append(", isArray=").append(isArray);
        sb.append(", isMap=").append(isMap);
        sb.append(", isEnum=").append(isEnum);
        sb.append(", isAnyType=").append(isAnyType);
        sb.append(", isReadOnly=").append(isReadOnly);
        sb.append(", isWriteOnly=").append(isWriteOnly);
        sb.append(", isNullable=").append(isNullable);
        sb.append(", isSelfReference=").append(isSelfReference);
        sb.append(", isCircularReference=").append(isCircularReference);
        sb.append(", isDiscriminator=").append(isDiscriminator);
        sb.append(", _enum=").append(_enum);
        sb.append(", allowableValues=").append(allowableValues);
        sb.append(", items=").append(items);
        sb.append(", additionalProperties=").append(additionalProperties);
        sb.append(", vars=").append(vars);
        sb.append(", requiredVars=").append(requiredVars);
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
        sb.append(", maxProperties=").append(maxProperties);
        sb.append(", minProperties=").append(minProperties);
        sb.append(", uniqueItems=").append(uniqueItems);
        sb.append(", multipleOf=").append(multipleOf);
        sb.append(", isXmlAttribute=").append(isXmlAttribute);
        sb.append(", xmlPrefix='").append(xmlPrefix).append('\'');
        sb.append(", xmlName='").append(xmlName).append('\'');
        sb.append(", xmlNamespace='").append(xmlNamespace).append('\'');
        sb.append(", isXmlWrapped=").append(isXmlWrapped);
        sb.append(", isNull=").append(isNull);
        sb.append(", getAdditionalPropertiesIsAnyType=").append(getAdditionalPropertiesIsAnyType());
        sb.append(", getHasVars=").append(getHasVars());
        sb.append(", getHasRequired=").append(getHasRequired());
        sb.append(", getHasDiscriminatorWithNonEmptyMapping=").append(hasDiscriminatorWithNonEmptyMapping);
        sb.append(", composedSchemas=").append(composedSchemas);
        sb.append(", hasMultipleTypes=").append(hasMultipleTypes);
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
                required == that.required &&
                deprecated == that.deprecated &&
                hasMoreNonReadOnly == that.hasMoreNonReadOnly &&
                isPrimitiveType == that.isPrimitiveType &&
                isModel == that.isModel &&
                isContainer == that.isContainer &&
                isString == that.isString &&
                isNumeric == that.isNumeric &&
                isInteger == that.isInteger &&
                isShort == that.isShort &&
                isLong == that.isLong &&
                isUnboundedInteger == that.isUnboundedInteger &&
                isNumber == that.isNumber &&
                isFloat == that.isFloat &&
                isDouble == that.isDouble &&
                isDecimal == that.isDecimal &&
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
                isArray == that.isArray &&
                isMap == that.isMap &&
                isEnum == that.isEnum &&
                isAnyType == that.isAnyType &&
                isReadOnly == that.isReadOnly &&
                isWriteOnly == that.isWriteOnly &&
                isNullable == that.isNullable &&
                isSelfReference == that.isSelfReference &&
                isCircularReference == that.isCircularReference &&
                isDiscriminator == that.isDiscriminator &&
                hasValidation == that.hasValidation &&
                isInherited == that.isInherited &&
                isXmlAttribute == that.isXmlAttribute &&
                isXmlWrapped == that.isXmlWrapped &&
                isNull == that.isNull &&
                hasMultipleTypes == that.getHasMultipleTypes() &&
                hasDiscriminatorWithNonEmptyMapping == that.hasDiscriminatorWithNonEmptyMapping &&
                getAdditionalPropertiesIsAnyType() == that.getAdditionalPropertiesIsAnyType() &&
                getHasVars() == that.getHasVars() &&
                getHasRequired() == that.getHasRequired() &&
                Objects.equals(composedSchemas, that.composedSchemas) &&
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
                Objects.equals(additionalProperties, that.additionalProperties) &&
                Objects.equals(vars, that.vars) &&
                Objects.equals(requiredVars, that.requiredVars) &&
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
                Objects.equals(xmlNamespace, that.xmlNamespace) &&
                Objects.equals(multipleOf, that.multipleOf);
    }

    @Override
    public int hashCode() {

        return Objects.hash(openApiType, baseName, complexType, getter, setter, description,
                dataType, datatypeWithEnum, dataFormat, name, min, max, defaultValue,
                defaultValueWithParam, baseType, containerType, title, unescapedDescription,
                maxLength, minLength, pattern, example, jsonSchema, minimum, maximum,
                exclusiveMinimum, exclusiveMaximum, required, deprecated,
                hasMoreNonReadOnly, isPrimitiveType, isModel, isContainer, isString, isNumeric,
                isInteger, isLong, isNumber, isFloat, isDouble, isDecimal, isByteArray, isBinary, isFile,
                isBoolean, isDate, isDateTime, isUuid, isUri, isEmail, isFreeFormObject,
                isArray, isMap, isEnum, isAnyType, isReadOnly, isWriteOnly, isNullable, isShort,
                isUnboundedInteger, isSelfReference, isCircularReference, isDiscriminator, _enum,
                allowableValues, items, mostInnerItems, additionalProperties, vars, requiredVars,
                vendorExtensions, hasValidation, isInherited, discriminatorValue, nameInCamelCase,
                nameInSnakeCase, enumName, maxItems, minItems, isXmlAttribute, xmlPrefix, xmlName,
                xmlNamespace, isXmlWrapped, isNull, additionalPropertiesIsAnyType, hasVars, hasRequired,
                hasDiscriminatorWithNonEmptyMapping, composedSchemas, hasMultipleTypes);
    }
}
