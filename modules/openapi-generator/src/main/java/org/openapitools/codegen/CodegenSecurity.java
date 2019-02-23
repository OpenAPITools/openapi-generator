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

import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;

public class CodegenSecurity {
    public String name;
    public String type;
    public String scheme;
    public Boolean hasMore, isBasic, isOAuth, isApiKey;
    // is Basic is true for all http authentication type. Those are to differentiate basic and bearer authentication
    public Boolean isBasicBasic, isBasicBearer;
    public String bearerFormat;
    public Map<String, Object> vendorExtensions = new HashMap<String, Object>();
    // ApiKey specific
    public String keyParamName;
    public Boolean isKeyInQuery, isKeyInHeader, isKeyInCookie;
    // Oauth specific
    public String flow, authorizationUrl, tokenUrl;
    public List<Map<String, Object>> scopes;
    public Boolean isCode, isPassword, isApplication, isImplicit;

    @Override
    public String toString() {
        return String.format(Locale.ROOT, "%s(%s)", name, type);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        CodegenSecurity that = (CodegenSecurity) o;

        return Objects.equals(name, that.name) &&
            Objects.equals(type, that.type) &&
            Objects.equals(hasMore, that.hasMore) &&
            Objects.equals(isBasic, that.isBasic) &&
            Objects.equals(isBasicBasic, that.isBasicBasic) &&
            Objects.equals(isBasicBearer, that.isBasicBearer) &&
            Objects.equals(bearerFormat, that.bearerFormat) &&
            Objects.equals(isOAuth, that.isOAuth) &&
            Objects.equals(isApiKey, that.isApiKey) &&
            Objects.equals(vendorExtensions, that.vendorExtensions) &&
            Objects.equals(keyParamName, that.keyParamName) &&
            Objects.equals(isKeyInQuery, that.isKeyInQuery) &&
            Objects.equals(isKeyInHeader, that.isKeyInHeader) &&
            Objects.equals(flow, that.flow) &&
            Objects.equals(authorizationUrl, that.authorizationUrl) &&
            Objects.equals(tokenUrl, that.tokenUrl) &&
            Objects.equals(isCode, that.isCode) &&
            Objects.equals(isPassword, that.isPassword) &&
            Objects.equals(isApplication, that.isApplication) &&
            Objects.equals(isImplicit, that.isImplicit) &&
            Objects.equals(scopes, that.scopes);
    }

    @Override
    public int hashCode() {
        int result = name != null ? name.hashCode() : 0;
        result = 31 * result + (type != null ? type.hashCode() : 0);
        result = 31 * result + (hasMore != null ? hasMore.hashCode() : 0);
        result = 31 * result + (isBasic != null ? isBasic.hashCode() : 0);
        result = 31 * result + (isBasicBasic != null ? isBasicBasic.hashCode() : 0);
        result = 31 * result + (isBasicBearer != null ? isBasicBearer.hashCode() : 0);
        result = 31 * result + (bearerFormat != null ? bearerFormat.hashCode() : 0);
        result = 31 * result + (isOAuth != null ? isOAuth.hashCode() : 0);
        result = 31 * result + (isApiKey != null ? isApiKey.hashCode() : 0);
        result = 31 * result + (vendorExtensions != null ? vendorExtensions.hashCode() : 0);
        result = 31 * result + (keyParamName != null ? keyParamName.hashCode() : 0);
        result = 31 * result + (isKeyInQuery != null ? isKeyInQuery.hashCode() : 0);
        result = 31 * result + (isKeyInHeader != null ? isKeyInHeader.hashCode() : 0);
        result = 31 * result + (flow != null ? flow.hashCode() : 0);
        result = 31 * result + (authorizationUrl != null ? authorizationUrl.hashCode() : 0);
        result = 31 * result + (tokenUrl != null ? tokenUrl.hashCode() : 0);
        result = 31 * result + (isCode != null ? isCode.hashCode() : 0);
        result = 31 * result + (isPassword != null ? isPassword.hashCode() : 0);
        result = 31 * result + (isApplication != null ? isApplication.hashCode() : 0);
        result = 31 * result + (isImplicit != null ? isImplicit.hashCode() : 0);
        result = 31 * result + (scopes != null ? scopes.hashCode() : 0);
        return result;
    }
}
