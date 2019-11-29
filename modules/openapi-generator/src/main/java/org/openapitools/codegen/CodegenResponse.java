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

public class CodegenResponse {
    public final List<CodegenProperty> headers = new ArrayList<CodegenProperty>();
    public String code, message;
    public boolean hasMore;
    public List<Map<String, Object>> examples;
    public String dataType, baseType, containerType;
    public boolean hasHeaders;
    public boolean isString, isNumeric, isInteger, isLong, isNumber, isFloat, isDouble, isByteArray, isBoolean, isDate,
            isDateTime, isUuid, isEmail, isModel, isFreeFormObject;
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

    public boolean isWildcard() {
        return "0".equals(code) || "default".equals(code);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        CodegenResponse that = (CodegenResponse) o;
        return hasMore == that.hasMore &&
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
                isDefault == that.isDefault &&
                simpleType == that.simpleType &&
                primitiveType == that.primitiveType &&
                isMapContainer == that.isMapContainer &&
                isListContainer == that.isListContainer &&
                isBinary == that.isBinary &&
                isFile == that.isFile &&
                Objects.equals(headers, that.headers) &&
                Objects.equals(code, that.code) &&
                Objects.equals(message, that.message) &&
                Objects.equals(examples, that.examples) &&
                Objects.equals(dataType, that.dataType) &&
                Objects.equals(baseType, that.baseType) &&
                Objects.equals(containerType, that.containerType) &&
                Objects.equals(schema, that.schema) &&
                Objects.equals(jsonSchema, that.jsonSchema) &&
                Objects.equals(vendorExtensions, that.vendorExtensions);
    }

    @Override
    public int hashCode() {

        return Objects.hash(headers, code, message, hasMore, examples, dataType, baseType, containerType,
                hasHeaders, isString, isNumeric, isInteger, isLong, isNumber, isFloat, isDouble, isByteArray,
                isBoolean, isDate, isDateTime, isUuid, isEmail, isModel, isFreeFormObject, isDefault, simpleType,
                primitiveType, isMapContainer, isListContainer, isBinary, isFile, schema, jsonSchema, vendorExtensions);
    }

    @Override
    public String toString() {
        final StringBuffer sb = new StringBuffer("CodegenResponse{");
        sb.append("headers=").append(headers);
        sb.append(", code='").append(code).append('\'');
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
        sb.append('}');
        return sb.toString();
    }
}
