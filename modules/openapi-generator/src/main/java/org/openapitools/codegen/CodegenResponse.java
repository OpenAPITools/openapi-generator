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

public class CodegenResponse implements IJsonSchemaValidationProperties {
    public final List<CodegenProperty> headers = new ArrayList<CodegenProperty>();
    public String code;
    public boolean is1xx;
    public boolean is2xx;
    public boolean is3xx;
    public boolean is4xx;
    public boolean is5xx;
    public String wildcardFirstChar; // store the first character of 2XX, 5XX, etc
    public String message;
    public boolean hasMore;
    public List<Map<String, Object>> examples;
    public String dataType;
    public String baseType;
    public String containerType;
    public boolean hasHeaders;
    public boolean isString;
    public boolean isNumeric;
    public boolean isInteger;
    public boolean isLong;
    public boolean isNumber;
    public boolean isFloat;
    public boolean isDouble;
    public boolean isByteArray;
    public boolean isBoolean;
    public boolean isDate;
    public boolean isDateTime;
    public boolean isUuid;
    public boolean isEmail;
    public boolean isModel;
    public boolean isFreeFormObject;
    public boolean isAnyType;
    public boolean isDefault;
    public boolean simpleType;
    public boolean primitiveType;
    public boolean isMapContainer;
    public boolean isListContainer;
    public boolean isBinary = false;
    public boolean isFile = false;
    public Object schema;
    public String jsonSchema;
    public Map<String, Object> vendorExtensions = new HashMap<String, Object>();
    private Integer maxProperties;
    private Integer minProperties;
    private boolean uniqueItems;
    private Integer maxItems;
    private Integer minItems;
    private Integer maxLength;
    private Integer minLength;
    private boolean exclusiveMinimum;
    private boolean exclusiveMaximum;
    private String minimum;
    private String maximum;
    public String pattern;
    public Number multipleOf;

    @Override
    public String getPattern() {
        return pattern;
    }

    @Override
    public void setPattern(String pattern) {
        this.pattern = pattern;
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
    public String getMinimum() {
        return minimum;
    }

    @Override
    public void setMinimum(String minimum) {
        this.minimum = minimum;
    }

    @Override
    public boolean getExclusiveMaximum() {
        return exclusiveMaximum;
    }

    @Override
    public void setExclusiveMaximum(boolean exclusiveMaximum) {
        this.exclusiveMaximum = exclusiveMaximum;
    }

    @Override
    public boolean getExclusiveMinimum() {
        return exclusiveMinimum;
    }

    @Override
    public void setExclusiveMinimum(boolean exclusiveMinimum) {
        this.exclusiveMinimum = exclusiveMinimum;
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
    public Integer getMaxLength() {
        return maxLength;
    }

    @Override
    public void setMaxLength(Integer maxLength) {
        this.maxLength = maxLength;
    }

    @Override
    public Integer getMinItems() {
        return minItems;
    }

    @Override
    public void setMinItems(Integer minItems) {
        this.minItems = minItems;
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
    public String toString() {
        final StringBuffer sb = new StringBuffer("CodegenResponse{");
        sb.append("headers=").append(headers);
        sb.append(", code='").append(code).append('\'');
        sb.append(", is1xx=").append(is1xx);
        sb.append(", is2xx=").append(is2xx);
        sb.append(", is3xx=").append(is3xx);
        sb.append(", is4xx=").append(is4xx);
        sb.append(", is5xx=").append(is5xx);
        sb.append(", wildcardFirstChar=").append(wildcardFirstChar);
        sb.append(", message='").append(message).append('\'');
        sb.append(", hasMore=").append(hasMore);
        sb.append(", examples=").append(examples);
        sb.append(", dataType='").append(dataType).append('\'');
        sb.append(", baseType='").append(baseType).append('\'');
        sb.append(", containerType='").append(containerType).append('\'');
        sb.append(", hasHeaders=").append(hasHeaders);
        sb.append(", isString=").append(isString);
        sb.append(", isNumeric=").append(isNumeric);
        sb.append(", isInteger=").append(isInteger);
        sb.append(", isLong=").append(isLong);
        sb.append(", isNumber=").append(isNumber);
        sb.append(", isFloat=").append(isFloat);
        sb.append(", isDouble=").append(isDouble);
        sb.append(", isByteArray=").append(isByteArray);
        sb.append(", isBoolean=").append(isBoolean);
        sb.append(", isDate=").append(isDate);
        sb.append(", isDateTime=").append(isDateTime);
        sb.append(", isUuid=").append(isUuid);
        sb.append(", isEmail=").append(isEmail);
        sb.append(", isModel=").append(isModel);
        sb.append(", isFreeFormObject=").append(isFreeFormObject);
        sb.append(", isAnyType=").append(isAnyType);
        sb.append(", isDefault=").append(isDefault);
        sb.append(", simpleType=").append(simpleType);
        sb.append(", primitiveType=").append(primitiveType);
        sb.append(", isMapContainer=").append(isMapContainer);
        sb.append(", isListContainer=").append(isListContainer);
        sb.append(", isBinary=").append(isBinary);
        sb.append(", isFile=").append(isFile);
        sb.append(", schema=").append(schema);
        sb.append(", jsonSchema='").append(jsonSchema).append('\'');
        sb.append(", vendorExtensions=").append(vendorExtensions);
        sb.append(", maxProperties=").append(maxProperties);
        sb.append(", minProperties=").append(minProperties);
        sb.append(", uniqueItems=").append(uniqueItems);
        sb.append(", maxItems=").append(maxItems);
        sb.append(", minItems=").append(minItems);
        sb.append(", maxLength=").append(maxLength);
        sb.append(", minLength=").append(minLength);
        sb.append(", exclusiveMinimum=").append(exclusiveMinimum);
        sb.append(", exclusiveMaximum=").append(exclusiveMaximum);
        sb.append(", minimum='").append(minimum).append('\'');
        sb.append(", maximum='").append(maximum).append('\'');
        sb.append(", pattern='").append(pattern).append('\'');
        sb.append(", multipleOf=").append(multipleOf);
        sb.append('}');
        return sb.toString();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        CodegenResponse that = (CodegenResponse) o;
        return is1xx == that.is1xx &&
                is2xx == that.is2xx &&
                is3xx == that.is3xx &&
                is4xx == that.is4xx &&
                is5xx == that.is5xx &&
                wildcardFirstChar == that.wildcardFirstChar &&
                hasMore == that.hasMore &&
                hasHeaders == that.hasHeaders &&
                isString == that.isString &&
                isNumeric == that.isNumeric &&
                isInteger == that.isInteger &&
                isLong == that.isLong &&
                isNumber == that.isNumber &&
                isFloat == that.isFloat &&
                isDouble == that.isDouble &&
                isByteArray == that.isByteArray &&
                isBoolean == that.isBoolean &&
                isDate == that.isDate &&
                isDateTime == that.isDateTime &&
                isUuid == that.isUuid &&
                isEmail == that.isEmail &&
                isModel == that.isModel &&
                isFreeFormObject == that.isFreeFormObject &&
                isAnyType == that.isAnyType &&
                isDefault == that.isDefault &&
                simpleType == that.simpleType &&
                primitiveType == that.primitiveType &&
                isMapContainer == that.isMapContainer &&
                isListContainer == that.isListContainer &&
                isBinary == that.isBinary &&
                isFile == that.isFile &&
                uniqueItems == that.uniqueItems &&
                exclusiveMinimum == that.exclusiveMinimum &&
                exclusiveMaximum == that.exclusiveMaximum &&
                Objects.equals(headers, that.headers) &&
                Objects.equals(code, that.code) &&
                Objects.equals(message, that.message) &&
                Objects.equals(examples, that.examples) &&
                Objects.equals(dataType, that.dataType) &&
                Objects.equals(baseType, that.baseType) &&
                Objects.equals(containerType, that.containerType) &&
                Objects.equals(schema, that.schema) &&
                Objects.equals(jsonSchema, that.jsonSchema) &&
                Objects.equals(vendorExtensions, that.vendorExtensions) &&
                Objects.equals(maxProperties, that.maxProperties) &&
                Objects.equals(minProperties, that.minProperties) &&
                Objects.equals(maxItems, that.maxItems) &&
                Objects.equals(minItems, that.minItems) &&
                Objects.equals(maxLength, that.maxLength) &&
                Objects.equals(minLength, that.minLength) &&
                Objects.equals(minimum, that.minimum) &&
                Objects.equals(maximum, that.maximum) &&
                Objects.equals(pattern, that.pattern) &&
                Objects.equals(multipleOf, that.multipleOf);
    }

    @Override
    public int hashCode() {

        return Objects.hash(headers, code, is1xx, is2xx, is3xx, is4xx, is5xx, wildcardFirstChar, message, hasMore,
                examples, dataType, baseType, containerType, hasHeaders, isString, isNumeric, isInteger, isLong,
                isNumber, isFloat, isDouble, isByteArray, isBoolean, isDate, isDateTime, isUuid, isEmail, isModel,
                isFreeFormObject, isAnyType, isDefault, simpleType, primitiveType, isMapContainer, isListContainer,
                isBinary, isFile, schema, jsonSchema, vendorExtensions, maxProperties, minProperties, uniqueItems,
                maxItems, minItems, maxLength, minLength, exclusiveMinimum, exclusiveMaximum, minimum, maximum,
                pattern, multipleOf);
    }

    // this is used in templates. Do not remove it.
    @SuppressWarnings("unused")
    public boolean isWildcard() {
        return "0".equals(code) || "default".equals(code);
    }
}
