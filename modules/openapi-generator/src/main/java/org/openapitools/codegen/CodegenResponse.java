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

    public boolean isStatus1xx() { return code != null && code.length() == 3 && code.startsWith("1"); }
    public boolean isStatus100() { return "100".equals(code); }
    public boolean isStatus101() { return "101".equals(code); }
    public boolean isStatus102() { return "102".equals(code); }

    public boolean isStatus2xx() { return code != null && code.length() == 3 && code.startsWith("2"); }
    public boolean isStatus200() { return "200".equals(code); }
    public boolean isStatus201() { return "201".equals(code); }
    public boolean isStatus202() { return "202".equals(code); }
    public boolean isStatus203() { return "203".equals(code); }
    public boolean isStatus204() { return "204".equals(code); }
    public boolean isStatus205() { return "205".equals(code); }
    public boolean isStatus206() { return "206".equals(code); }
    public boolean isStatus207() { return "207".equals(code); }

    public boolean isStatus3xx() { return code != null && code.length() == 3 && code.startsWith("3"); }
    public boolean isStatus300() { return "300".equals(code); }
    public boolean isStatus301() { return "301".equals(code); }
    public boolean isStatus302() { return "302".equals(code); }
    public boolean isStatus303() { return "303".equals(code); }
    public boolean isStatus304() { return "304".equals(code); }
    public boolean isStatus305() { return "305".equals(code); }
    public boolean isStatus307() { return "307".equals(code); }

    public boolean isStatus4xx() { return code != null && code.length() == 3 && code.startsWith("4"); }
    public boolean isStatus400() { return "400".equals(code); }
    public boolean isStatus401() { return "401".equals(code); }
    public boolean isStatus402() { return "402".equals(code); }
    public boolean isStatus403() { return "403".equals(code); }
    public boolean isStatus404() { return "404".equals(code); }
    public boolean isStatus405() { return "405".equals(code); }
    public boolean isStatus406() { return "406".equals(code); }
    public boolean isStatus407() { return "407".equals(code); }
    public boolean isStatus408() { return "408".equals(code); }
    public boolean isStatus409() { return "409".equals(code); }
    public boolean isStatus410() { return "410".equals(code); }
    public boolean isStatus411() { return "411".equals(code); }
    public boolean isStatus412() { return "412".equals(code); }
    public boolean isStatus413() { return "413".equals(code); }
    public boolean isStatus414() { return "414".equals(code); }
    public boolean isStatus415() { return "415".equals(code); }
    public boolean isStatus416() { return "416".equals(code); }
    public boolean isStatus417() { return "417".equals(code); }
    public boolean isStatus418() { return "418".equals(code); }
    public boolean isStatus419() { return "419".equals(code); }
    public boolean isStatus420() { return "420".equals(code); }
    public boolean isStatus422() { return "422".equals(code); }
    public boolean isStatus423() { return "423".equals(code); }
    public boolean isStatus424() { return "424".equals(code); }

    public boolean isStatus5xx() { return code != null && code.length() == 3 && code.startsWith("5"); }
    public boolean isStatus500() { return "500".equals(code); }
    public boolean isStatus501() { return "501".equals(code); }
    public boolean isStatus502() { return "502".equals(code); }
    public boolean isStatus503() { return "503".equals(code); }
    public boolean isStatus504() { return "504".equals(code); }
    public boolean isStatus505() { return "505".equals(code); }
    public boolean isStatus507() { return "507".equals(code); }

    @Override
    public String toString() {
        return "CodegenResponse{" +
                "headers=" + headers +
                ", code='" + code + '\'' +
                ", message='" + message + '\'' +
                ", hasMore=" + hasMore +
                ", examples=" + examples +
                ", dataType='" + dataType + '\'' +
                ", baseType='" + baseType + '\'' +
                ", containerType='" + containerType + '\'' +
                ", hasHeaders=" + hasHeaders +
                ", isString=" + isString +
                ", isNumeric=" + isNumeric +
                ", isInteger=" + isInteger +
                ", isLong=" + isLong +
                ", isNumber=" + isNumber +
                ", isFloat=" + isFloat +
                ", isDouble=" + isDouble +
                ", isByteArray=" + isByteArray +
                ", isBoolean=" + isBoolean +
                ", isDate=" + isDate +
                ", isDateTime=" + isDateTime +
                ", isUuid=" + isUuid +
                ", isEmail=" + isEmail +
                ", isFreeFormObject=" + isFreeFormObject +
                ", isModel=" + isModel +
                ", isDefault=" + isDefault +
                ", simpleType=" + simpleType +
                ", primitiveType=" + primitiveType +
                ", isMapContainer=" + isMapContainer +
                ", isListContainer=" + isListContainer +
                ", isBinary=" + isBinary +
                ", isFile=" + isFile +
                ", schema=" + schema +
                ", jsonSchema='" + jsonSchema + '\'' +
                ", vendorExtensions=" + vendorExtensions +
                '}';
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
                isFreeFormObject == that.isFreeFormObject &&
                isModel == that.isModel &&
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
        return Objects.hash(headers, code, message, hasMore, examples, dataType, baseType, containerType, hasHeaders,
                isString, isNumeric, isInteger, isLong, isNumber, isFloat, isDouble, isByteArray, isBoolean, isDate,
                isDateTime, isUuid, isEmail, isFreeFormObject, isModel, isDefault, simpleType, primitiveType, isMapContainer,
                isListContainer, isBinary, isFile, schema, jsonSchema, vendorExtensions);
    }
}
