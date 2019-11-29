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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

public class CodegenParameter {
    public boolean isFormParam, isQueryParam, isPathParam, isHeaderParam,
            isCookieParam, isBodyParam, hasMore, isContainer,
            secondaryParam, isCollectionFormatMulti, isPrimitiveType, isModel, isExplode;
    public String baseName, paramName, dataType, datatypeWithEnum, dataFormat,
            collectionFormat, description, unescapedDescription, baseType, defaultValue, enumName, style;

    public String example; // example value (x-example)
    public String jsonSchema;
    public boolean isString, isNumeric, isInteger, isLong, isNumber, isFloat, isDouble, isByteArray, isBinary,
            isBoolean, isDate, isDateTime, isUuid, isUri, isEmail, isFreeFormObject;
    public boolean isListContainer, isMapContainer;
    public boolean isFile;
    public boolean isEnum;
    public List<String> _enum;
    public Map<String, Object> allowableValues;
    public CodegenProperty items;
    public CodegenProperty mostInnerItems;
    public Map<String, Object> vendorExtensions = new HashMap<String, Object>();
    public boolean hasValidation;
    public boolean isNullable;

    /**
     * Determines whether this parameter is mandatory. If the parameter is in "path",
     * this property is required and its value MUST be true. Otherwise, the property
     * MAY be included and its default value is false.
     */
    public boolean required;

    /**
     * See http://json-schema.org/latest/json-schema-validation.html#anchor17.
     */
    public String maximum;
    /**
     * See http://json-schema.org/latest/json-schema-validation.html#anchor17
     */
    public boolean exclusiveMaximum;
    /**
     * See http://json-schema.org/latest/json-schema-validation.html#anchor21
     */
    public String minimum;
    /**
     * See http://json-schema.org/latest/json-schema-validation.html#anchor21
     */
    public boolean exclusiveMinimum;
    /**
     * See http://json-schema.org/latest/json-schema-validation.html#anchor26
     */
    public Integer maxLength;
    /**
     * See http://json-schema.org/latest/json-schema-validation.html#anchor29
     */
    public Integer minLength;
    /**
     * See http://json-schema.org/latest/json-schema-validation.html#anchor33
     */
    public String pattern;
    /**
     * See http://json-schema.org/latest/json-schema-validation.html#anchor42
     */
    public Integer maxItems;
    /**
     * See http://json-schema.org/latest/json-schema-validation.html#anchor45
     */
    public Integer minItems;
    /**
     * See http://json-schema.org/latest/json-schema-validation.html#anchor49
     */
    public boolean uniqueItems;
    /**
     * See http://json-schema.org/latest/json-schema-validation.html#anchor14
     */
    public Number multipleOf;

    public CodegenParameter copy() {
        CodegenParameter output = new CodegenParameter();
        output.isFile = this.isFile;
        output.hasMore = this.hasMore;
        output.isContainer = this.isContainer;
        output.secondaryParam = this.secondaryParam;
        output.baseName = this.baseName;
        output.paramName = this.paramName;
        output.dataType = this.dataType;
        output.datatypeWithEnum = this.datatypeWithEnum;
        output.enumName = this.enumName;
        output.dataFormat = this.dataFormat;
        output.collectionFormat = this.collectionFormat;
        output.isCollectionFormatMulti = this.isCollectionFormatMulti;
        output.isPrimitiveType = this.isPrimitiveType;
        output.isModel = this.isModel;
        output.description = this.description;
        output.unescapedDescription = this.unescapedDescription;
        output.baseType = this.baseType;
        output.isFormParam = this.isFormParam;
        output.isQueryParam = this.isQueryParam;
        output.isPathParam = this.isPathParam;
        output.isHeaderParam = this.isHeaderParam;
        output.isCookieParam = this.isCookieParam;
        output.isBodyParam = this.isBodyParam;
        output.required = this.required;
        output.maximum = this.maximum;
        output.exclusiveMaximum = this.exclusiveMaximum;
        output.minimum = this.minimum;
        output.exclusiveMinimum = this.exclusiveMinimum;
        output.maxLength = this.maxLength;
        output.minLength = this.minLength;
        output.pattern = this.pattern;
        output.maxItems = this.maxItems;
        output.minItems = this.minItems;
        output.uniqueItems = this.uniqueItems;
        output.multipleOf = this.multipleOf;
        output.jsonSchema = this.jsonSchema;
        output.defaultValue = this.defaultValue;
        output.example = this.example;
        output.isEnum = this.isEnum;
        if (this._enum != null) {
            output._enum = new ArrayList<String>(this._enum);
        }
        if (this.allowableValues != null) {
            output.allowableValues = new HashMap<String, Object>(this.allowableValues);
        }
        if (this.items != null) {
            output.items = this.items;
        }
        if (this.mostInnerItems != null) {
            output.mostInnerItems = this.mostInnerItems;
        }
        if (this.vendorExtensions != null) {
            output.vendorExtensions = new HashMap<String, Object>(this.vendorExtensions);
        }
        output.hasValidation = this.hasValidation;
        output.isNullable = this.isNullable;
        output.isBinary = this.isBinary;
        output.isByteArray = this.isByteArray;
        output.isString = this.isString;
        output.isNumeric = this.isNumeric;
        output.isInteger = this.isInteger;
        output.isLong = this.isLong;
        output.isDouble = this.isDouble;
        output.isFloat = this.isFloat;
        output.isNumber = this.isNumber;
        output.isBoolean = this.isBoolean;
        output.isDate = this.isDate;
        output.isDateTime = this.isDateTime;
        output.isUuid = this.isUuid;
        output.isUri = this.isUri;
        output.isEmail = this.isEmail;
        output.isFreeFormObject = this.isFreeFormObject;
        output.isListContainer = this.isListContainer;
        output.isMapContainer = this.isMapContainer;
        output.isExplode = this.isExplode;
        output.style = this.style;

        return output;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        CodegenParameter that = (CodegenParameter) o;

        return Objects.equals(isEnum, that.isEnum) &&
            Objects.equals(isFormParam, that.isFormParam) &&
            Objects.equals(isQueryParam, that.isQueryParam) &&
            Objects.equals(isPathParam, that.isPathParam) &&
            Objects.equals(isHeaderParam, that.isHeaderParam) &&
            Objects.equals(isCookieParam, that.isCookieParam) &&
            Objects.equals(isBodyParam, that.isBodyParam) &&
            Objects.equals(hasMore, that.hasMore) &&
            Objects.equals(isContainer, that.isContainer) &&
            Objects.equals(secondaryParam, that.secondaryParam) &&
            Objects.equals(isCollectionFormatMulti, that.isCollectionFormatMulti) &&
            Objects.equals(isPrimitiveType, that.isPrimitiveType) &&
            Objects.equals(isModel, that.isModel) &&
            Objects.equals(baseName, that.baseName) &&
            Objects.equals(paramName, that.paramName) &&
            Objects.equals(dataType, that.dataType) &&
            Objects.equals(datatypeWithEnum, that.datatypeWithEnum) &&
            Objects.equals(enumName, that.enumName) &&
            Objects.equals(dataFormat, that.dataFormat) &&
            Objects.equals(collectionFormat, that.collectionFormat) &&
            Objects.equals(description, that.description) &&
            Objects.equals(unescapedDescription, that.unescapedDescription) &&
            Objects.equals(baseType, that.baseType) &&
            Objects.equals(defaultValue, that.defaultValue) &&
            Objects.equals(example, that.example) &&
            Objects.equals(jsonSchema, that.jsonSchema) &&
            Objects.equals(isString, that.isString) &&
            Objects.equals(isNumeric, that.isNumeric) &&
            Objects.equals(isInteger, that.isInteger) &&
            Objects.equals(isLong, that.isLong) &&
            Objects.equals(isNumber, that.isNumber) &&
            Objects.equals(isFloat, that.isFloat) &&
            Objects.equals(isDouble, that.isDouble) &&
            Objects.equals(isByteArray, that.isByteArray) &&
            Objects.equals(isBinary, that.isBinary) &&
            Objects.equals(isBoolean, that.isBoolean) &&
            Objects.equals(isDate, that.isDate) &&
            Objects.equals(isDateTime, that.isDateTime) &&
            Objects.equals(isUuid, that.isUuid) &&
            Objects.equals(isUri, that.isUri) &&
            Objects.equals(isEmail, that.isEmail) &&
            Objects.equals(isFreeFormObject, that.isFreeFormObject) &&
            Objects.equals(isListContainer, that.isListContainer) &&
            Objects.equals(isMapContainer, that.isMapContainer) &&
            Objects.equals(isFile, that.isFile) &&
            Objects.equals(_enum, that._enum) &&
            Objects.equals(allowableValues, that.allowableValues) &&
            Objects.equals(items, that.items) &&
            Objects.equals(mostInnerItems, that.mostInnerItems) &&
            Objects.equals(vendorExtensions, that.vendorExtensions) &&
            Objects.equals(hasValidation, that.hasValidation) &&
            Objects.equals(isNullable, that.isNullable) &&
            Objects.equals(required, that.required) &&
            Objects.equals(maximum, that.maximum) &&
            Objects.equals(exclusiveMaximum, that.exclusiveMaximum) &&
            Objects.equals(minimum, that.minimum) &&
            Objects.equals(exclusiveMinimum, that.exclusiveMinimum) &&
            Objects.equals(maxLength, that.maxLength) &&
            Objects.equals(minLength, that.minLength) &&
            Objects.equals(pattern, that.pattern) &&
            Objects.equals(maxItems, that.maxItems) &&
            Objects.equals(minItems, that.minItems) &&
            Objects.equals(uniqueItems, that.uniqueItems) &&
            Objects.equals(multipleOf, that.multipleOf) &&
            Objects.equals(isExplode, that.isExplode) &&
            Objects.equals(style, that.style);
    }

    @Override
    public int hashCode() {
        return Objects.hash(
            isFormParam,
            isQueryParam,
            isPathParam,
            isHeaderParam,
            isCookieParam,
            isBodyParam,
            hasMore,
            isContainer,
            secondaryParam,
            isCollectionFormatMulti,
            isPrimitiveType,
            isModel,
            baseName,
            paramName,
            dataType,
            datatypeWithEnum,
            enumName,
            dataFormat,
            collectionFormat,
            description,
            unescapedDescription,
            baseType,
            defaultValue,
            example,
            jsonSchema,
            isString,
            isNumeric,
            isInteger,
            isLong,
            isFloat,
            isNumber,
            isDouble,
            isByteArray,
            isBinary,
            isBoolean,
            isDate,
            isDateTime,
            isUuid,
            isUri,
            isEmail,
            isFreeFormObject,
            isListContainer,
            isMapContainer,
            isFile,
            isEnum,
            _enum,
            allowableValues,
            items,
            mostInnerItems,
            vendorExtensions,
            hasValidation,
            isNullable,
            required,
            maximum,
            exclusiveMaximum,
            minimum,
            exclusiveMinimum,
            maxLength,
            minLength,
            pattern,
            maxItems,
            minItems,
            uniqueItems,
            multipleOf,
            isExplode,
            style);
    }

    @java.lang.Override
    public java.lang.String toString() {
        return "CodegenParameter{" +
                "isFormParam=" + isFormParam +
                ", isQueryParam=" + isQueryParam +
                ", isPathParam=" + isPathParam +
                ", isHeaderParam=" + isHeaderParam +
                ", isCookieParam=" + isCookieParam +
                ", isBodyParam=" + isBodyParam +
                ", hasMore=" + hasMore +
                ", isContainer=" + isContainer +
                ", secondaryParam=" + secondaryParam +
                ", isCollectionFormatMulti=" + isCollectionFormatMulti +
                ", isPrimitiveType=" + isPrimitiveType +
                ", isModel=" + isModel +
                ", baseName='" + baseName + '\'' +
                ", paramName='" + paramName + '\'' +
                ", dataType='" + dataType + '\'' +
                ", datatypeWithEnum='" + datatypeWithEnum + '\'' +
                ", dataFormat='" + dataFormat + '\'' +
                ", collectionFormat='" + collectionFormat + '\'' +
                ", description='" + description + '\'' +
                ", unescapedDescription='" + unescapedDescription + '\'' +
                ", baseType='" + baseType + '\'' +
                ", defaultValue='" + defaultValue + '\'' +
                ", enumName='" + enumName + '\'' +
                ", example='" + example + '\'' +
                ", jsonSchema='" + jsonSchema + '\'' +
                ", isString=" + isString +
                ", isNumeric=" + isNumeric +
                ", isInteger=" + isInteger +
                ", isLong=" + isLong +
                ", isNumber=" + isNumber +
                ", isFloat=" + isFloat +
                ", isDouble=" + isDouble +
                ", isByteArray=" + isByteArray +
                ", isBinary=" + isBinary +
                ", isBoolean=" + isBoolean +
                ", isDate=" + isDate +
                ", isDateTime=" + isDateTime +
                ", isUuid=" + isUuid +
                ", isUri=" + isUri +
                ", isEmail=" + isEmail +
                ", isFreeFormObject=" + isFreeFormObject +
                ", isListContainer=" + isListContainer +
                ", isMapContainer=" + isMapContainer +
                ", isFile=" + isFile +
                ", isEnum=" + isEnum +
                ", _enum=" + _enum +
                ", allowableValues=" + allowableValues +
                ", items=" + items +
                ", mostInnerItems=" + mostInnerItems +
                ", vendorExtensions=" + vendorExtensions +
                ", hasValidation=" + hasValidation +
                ", isNullable=" + isNullable +
                ", required=" + required +
                ", maximum='" + maximum + '\'' +
                ", exclusiveMaximum=" + exclusiveMaximum +
                ", minimum='" + minimum + '\'' +
                ", exclusiveMinimum=" + exclusiveMinimum +
                ", maxLength=" + maxLength +
                ", minLength=" + minLength +
                ", pattern='" + pattern + '\'' +
                ", maxItems=" + maxItems +
                ", minItems=" + minItems +
                ", uniqueItems=" + uniqueItems +
                ", multipleOf=" + multipleOf +
                ", isExplode=" + isExplode +
                ", style='" + style + '\'' +
                '}';
    }
}

