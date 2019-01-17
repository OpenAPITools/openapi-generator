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

public class CodegenParameter {
    public boolean isFormParam, isQueryParam, isPathParam, isHeaderParam,
            isCookieParam, isBodyParam, hasMore, isContainer,
            secondaryParam, isCollectionFormatMulti, isPrimitiveType, isModel;
    public String baseName, paramName, dataType, datatypeWithEnum, dataFormat,
            collectionFormat, description, unescapedDescription, baseType, defaultValue, enumName;

    public String example; // example value (x-example)
    public String jsonSchema;
    public boolean isString, isNumeric, isInteger, isLong, isNumber, isFloat, isDouble, isByteArray, isBinary,
            isBoolean, isDate, isDateTime, isUuid, isEmail, isFreeFormObject;
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
        output.isEmail = this.isEmail;
        output.isFreeFormObject = this.isFreeFormObject;
        output.isListContainer = this.isListContainer;
        output.isMapContainer = this.isMapContainer;

        return output;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        CodegenParameter that = (CodegenParameter) o;

        if (isEnum != that.isEnum) return false;
        if (isFormParam != that.isFormParam)
            return false;
        if (isQueryParam != that.isQueryParam)
            return false;
        if (isPathParam != that.isPathParam)
            return false;
        if (isHeaderParam != that.isHeaderParam)
            return false;
        if (isCookieParam != that.isCookieParam)
            return false;
        if (isBodyParam != that.isBodyParam)
            return false;
        if (hasMore != that.hasMore)
            return false;
        if (isContainer != that.isContainer)
            return false;
        if (secondaryParam != that.secondaryParam)
            return false;
        if (isCollectionFormatMulti != that.isCollectionFormatMulti)
            return false;
        if (isPrimitiveType != that.isPrimitiveType)
            return false;
        if (isModel != that.isModel)
            return false;
        if (baseName != null ? !baseName.equals(that.baseName) : that.baseName != null)
            return false;
        if (paramName != null ? !paramName.equals(that.paramName) : that.paramName != null)
            return false;
        if (dataType != null ? !dataType.equals(that.dataType) : that.dataType != null)
            return false;
        if (datatypeWithEnum != null ? !datatypeWithEnum.equals(that.datatypeWithEnum) : that.datatypeWithEnum != null)
            return false;
        if (enumName != null ? !enumName.equals(that.enumName) : that.enumName != null)
            return false;
        if (dataFormat != null ? !dataFormat.equals(that.dataFormat) : that.dataFormat != null)
            return false;
        if (collectionFormat != null ? !collectionFormat.equals(that.collectionFormat) : that.collectionFormat != null)
            return false;
        if (description != null ? !description.equals(that.description) : that.description != null)
            return false;
        if (unescapedDescription != null ? !unescapedDescription.equals(that.unescapedDescription) : that.unescapedDescription != null)
            return false;
        if (baseType != null ? !baseType.equals(that.baseType) : that.baseType != null)
            return false;
        if (defaultValue != null ? !defaultValue.equals(that.defaultValue) : that.defaultValue != null)
            return false;
        if (example != null ? !example.equals(that.example) : that.example != null)
            return false;
        if (jsonSchema != null ? !jsonSchema.equals(that.jsonSchema) : that.jsonSchema != null)
            return false;
        if (isString != that.isString)
            return false;
        if (isNumeric != that.isNumeric)
            return false;
        if (isInteger != that.isInteger)
            return false;
        if (isLong != that.isLong)
            return false;
        if (isNumber != that.isNumber)
            return false;
        if (isFloat != that.isFloat)
            return false;
        if (isDouble != that.isDouble)
            return false;
        if (isByteArray != that.isByteArray)
            return false;
        if (isBinary != that.isBinary)
            return false;
        if (isBoolean != that.isBoolean)
            return false;
        if (isDate != that.isDate)
            return false;
        if (isDateTime != that.isDateTime)
            return false;
        if (isUuid != that.isUuid)
            return false;
        if (isEmail != that.isEmail)
            return false;
        if (isFreeFormObject != that.isFreeFormObject)
            return false;
        if (isListContainer != that.isListContainer)
            return false;
        if (isMapContainer != that.isMapContainer)
            return false;
        if (isFile != that.isFile)
            return false;
        if (_enum != null ? !_enum.equals(that._enum) : that._enum != null)
            return false;
        if (allowableValues != null ? !allowableValues.equals(that.allowableValues) : that.allowableValues != null)
            return false;
        if (items != null ? !items.equals(that.items) : that.items != null)
            return false;
        if (mostInnerItems != null ? !mostInnerItems.equals(that.mostInnerItems) : that.mostInnerItems != null)
            return false;
        if (vendorExtensions != null ? !vendorExtensions.equals(that.vendorExtensions) : that.vendorExtensions != null)
            return false;
        if (hasValidation != that.hasValidation)
            return false;
        if (isNullable != that.isNullable)
            return false;
        if (required != that.required)
            return false;
        if (maximum != null ? !maximum.equals(that.maximum) : that.maximum != null)
            return false;
        if (exclusiveMaximum != that.exclusiveMaximum)
            return false;
        if (minimum != null ? !minimum.equals(that.minimum) : that.minimum != null)
            return false;
        if (exclusiveMinimum != that.exclusiveMinimum)
            return false;
        if (maxLength != null ? !maxLength.equals(that.maxLength) : that.maxLength != null)
            return false;
        if (minLength != null ? !minLength.equals(that.minLength) : that.minLength != null)
            return false;
        if (pattern != null ? !pattern.equals(that.pattern) : that.pattern != null)
            return false;
        if (maxItems != null ? !maxItems.equals(that.maxItems) : that.maxItems != null)
            return false;
        if (minItems != null ? !minItems.equals(that.minItems) : that.minItems != null)
            return false;
        if (uniqueItems != that.uniqueItems)
            return false;
        return multipleOf != null ? multipleOf.equals(that.multipleOf) : that.multipleOf == null;

    }

    @Override
    public int hashCode() {
        int result = isFormParam ? 13 : 31;
        result = 31 * result + (isQueryParam ? 13 : 31);
        result = 31 * result + (isPathParam ? 13 : 31);
        result = 31 * result + (isHeaderParam ? 13 : 31);
        result = 31 * result + (isCookieParam ? 13 : 31);
        result = 31 * result + (isBodyParam ? 13 : 31);
        result = 31 * result + (hasMore ? 13 : 31);
        result = 31 * result + (isContainer ? 13 : 31);
        result = 31 * result + (secondaryParam ? 13 : 31);
        result = 31 * result + (isCollectionFormatMulti ? 13 : 31);
        result = 31 * result + (isPrimitiveType ? 13 : 31);
        result = 31 * result + (isModel ? 13 : 31);
        result = 31 * result + (baseName != null ? baseName.hashCode() : 0);
        result = 31 * result + (paramName != null ? paramName.hashCode() : 0);
        result = 31 * result + (dataType != null ? dataType.hashCode() : 0);
        result = 31 * result + (datatypeWithEnum != null ? datatypeWithEnum.hashCode() : 0);
        result = 31 * result + (enumName != null ? enumName.hashCode() : 0);
        result = 31 * result + (dataFormat != null ? dataFormat.hashCode() : 0);
        result = 31 * result + (collectionFormat != null ? collectionFormat.hashCode() : 0);
        result = 31 * result + (description != null ? description.hashCode() : 0);
        result = 31 * result + (unescapedDescription != null ? unescapedDescription.hashCode() : 0);
        result = 31 * result + (baseType != null ? baseType.hashCode() : 0);
        result = 31 * result + (defaultValue != null ? defaultValue.hashCode() : 0);
        result = 31 * result + (example != null ? example.hashCode() : 0);
        result = 31 * result + (jsonSchema != null ? jsonSchema.hashCode() : 0);
        result = 31 * result + (isString ? 13 : 31);
        result = 31 * result + (isNumeric ? 13 : 31);
        result = 31 * result + (isInteger ? 13 : 31);
        result = 31 * result + (isLong ? 13 : 31);
        result = 31 * result + (isFloat ? 13 : 31);
        result = 31 * result + (isNumber ? 13 : 31);
        result = 31 * result + (isDouble ? 13 : 31);
        result = 31 * result + (isByteArray ? 13 : 31);
        result = 31 * result + (isBinary ? 13 : 31);
        result = 31 * result + (isBoolean ? 13 : 31);
        result = 31 * result + (isDate ? 13 : 31);
        result = 31 * result + (isDateTime ? 13 : 31);
        result = 31 * result + (isUuid ? 13 : 31);
        result = 31 * result + (isEmail ? 13 : 31);
        result = 31 * result + (isFreeFormObject ? 13 : 31);
        result = 31 * result + (isListContainer ? 13 : 31);
        result = 31 * result + (isMapContainer ? 13 : 31);
        result = 31 * result + (isFile ? 13 : 31);
        result = 31 * result + (isEnum ? 1 : 0);
        result = 31 * result + (_enum != null ? _enum.hashCode() : 0);
        result = 31 * result + (allowableValues != null ? allowableValues.hashCode() : 0);
        result = 31 * result + (items != null ? items.hashCode() : 0);
        result = 31 * result + (mostInnerItems != null ? mostInnerItems.hashCode() : 0);
        result = 31 * result + (vendorExtensions != null ? vendorExtensions.hashCode() : 0);
        result = 31 * result + (hasValidation ? 13 : 31);
        result = 31 * result + (isNullable ? 13 : 31);
        result = 31 * result + (required ? 13 : 31);
        result = 31 * result + (maximum != null ? maximum.hashCode() : 0);
        result = 31 * result + (exclusiveMaximum ? 13 : 31);
        result = 31 * result + (minimum != null ? minimum.hashCode() : 0);
        result = 31 * result + (exclusiveMinimum ? 13 : 31);
        result = 31 * result + (maxLength != null ? maxLength.hashCode() : 0);
        result = 31 * result + (minLength != null ? minLength.hashCode() : 0);
        result = 31 * result + (pattern != null ? pattern.hashCode() : 0);
        result = 31 * result + (maxItems != null ? maxItems.hashCode() : 0);
        result = 31 * result + (minItems != null ? minItems.hashCode() : 0);
        result = 31 * result + (uniqueItems ? 13 : 31);
        result = 31 * result + (multipleOf != null ? multipleOf.hashCode() : 0);
        return result;
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
                '}';
    }
}

