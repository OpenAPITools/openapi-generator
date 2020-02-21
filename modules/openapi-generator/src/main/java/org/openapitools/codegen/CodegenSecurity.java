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

import java.util.ArrayList;
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
    // is Basic is true for all http authentication type.
    // Those are to differentiate basic and bearer authentication
    // isHttpSignature is to support HTTP signature authorization scheme.
    // https://datatracker.ietf.org/doc/draft-cavage-http-signatures/
    public Boolean isBasicBasic, isBasicBearer, isHttpSignature;
    public String bearerFormat;
    public Map<String, Object> vendorExtensions = new HashMap<String, Object>();
    // ApiKey specific
    public String keyParamName;
    public Boolean isKeyInQuery, isKeyInHeader, isKeyInCookie;
    // Oauth specific
    public String flow, authorizationUrl, tokenUrl;
    public List<Map<String, Object>> scopes;
    public Boolean isCode, isPassword, isApplication, isImplicit;

    // Return a copy of the security object, filtering out any scopes from the passed-in list.
    public CodegenSecurity filterByScopeNames(List<String> filterScopes) {
        CodegenSecurity filteredSecurity = new CodegenSecurity();
        // Copy all fields except the scopes.
        filteredSecurity.name = name;
        filteredSecurity.type = type;
        filteredSecurity.hasMore = false;
        filteredSecurity.isBasic = isBasic;
        filteredSecurity.isBasicBasic = isBasicBasic;
        filteredSecurity.isHttpSignature = isHttpSignature;
        filteredSecurity.isBasicBearer = isBasicBearer;
        filteredSecurity.isApiKey = isApiKey;
        filteredSecurity.isOAuth = isOAuth;
        filteredSecurity.keyParamName = keyParamName;
        filteredSecurity.isCode = isCode;
        filteredSecurity.isImplicit = isImplicit;
        filteredSecurity.isApplication = isApplication;
        filteredSecurity.isPassword = isPassword;
        filteredSecurity.isKeyInCookie = isKeyInCookie;
        filteredSecurity.isKeyInHeader = isKeyInHeader;
        filteredSecurity.isKeyInQuery = isKeyInQuery;
        filteredSecurity.flow = flow;
        filteredSecurity.tokenUrl = tokenUrl;
        filteredSecurity.authorizationUrl = authorizationUrl;
        // It is not possible to deep copy the extensions, as we have no idea what types they are.
        // So the filtered method *will* refer to the original extensions, if any.
        filteredSecurity.vendorExtensions = new HashMap<String, Object>(vendorExtensions);
        List<Map<String, Object>> returnedScopes = new ArrayList<Map<String, Object>>();
        Map<String, Object> lastScope = null;
        for (String filterScopeName : filterScopes) {
            for (Map<String, Object> scope : scopes) {
                String name = (String) scope.get("scope");
                if (filterScopeName.equals(name)) {
                    returnedScopes.add(scope);
                    lastScope = scope;
                    break;
                }
            }
        }
        filteredSecurity.scopes = returnedScopes;

        return filteredSecurity;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        CodegenSecurity that = (CodegenSecurity) o;
        return Objects.equals(name, that.name) &&
                Objects.equals(type, that.type) &&
                Objects.equals(scheme, that.scheme) &&
                Objects.equals(hasMore, that.hasMore) &&
                Objects.equals(isBasic, that.isBasic) &&
                Objects.equals(isOAuth, that.isOAuth) &&
                Objects.equals(isApiKey, that.isApiKey) &&
                Objects.equals(isBasicBasic, that.isBasicBasic) &&
                Objects.equals(isHttpSignature, that.isHttpSignature) &&
                Objects.equals(isBasicBearer, that.isBasicBearer) &&
                Objects.equals(bearerFormat, that.bearerFormat) &&
                Objects.equals(vendorExtensions, that.vendorExtensions) &&
                Objects.equals(keyParamName, that.keyParamName) &&
                Objects.equals(isKeyInQuery, that.isKeyInQuery) &&
                Objects.equals(isKeyInHeader, that.isKeyInHeader) &&
                Objects.equals(isKeyInCookie, that.isKeyInCookie) &&
                Objects.equals(flow, that.flow) &&
                Objects.equals(authorizationUrl, that.authorizationUrl) &&
                Objects.equals(tokenUrl, that.tokenUrl) &&
                Objects.equals(scopes, that.scopes) &&
                Objects.equals(isCode, that.isCode) &&
                Objects.equals(isPassword, that.isPassword) &&
                Objects.equals(isApplication, that.isApplication) &&
                Objects.equals(isImplicit, that.isImplicit);
    }

    @Override
    public int hashCode() {

        return Objects.hash(name, type, scheme, hasMore, isBasic, isOAuth, isApiKey,
                isBasicBasic, isHttpSignature, isBasicBearer, bearerFormat, vendorExtensions,
                keyParamName, isKeyInQuery, isKeyInHeader, isKeyInCookie, flow,
                authorizationUrl, tokenUrl, scopes, isCode, isPassword, isApplication, isImplicit);
    }

    @Override
    public String toString() {
        final StringBuffer sb = new StringBuffer("CodegenSecurity{");
        sb.append("name='").append(name).append('\'');
        sb.append(", type='").append(type).append('\'');
        sb.append(", scheme='").append(scheme).append('\'');
        sb.append(", hasMore=").append(hasMore);
        sb.append(", isBasic=").append(isBasic);
        sb.append(", isOAuth=").append(isOAuth);
        sb.append(", isApiKey=").append(isApiKey);
        sb.append(", isBasicBasic=").append(isBasicBasic);
        sb.append(", isHttpSignature=").append(isHttpSignature);
        sb.append(", isBasicBearer=").append(isBasicBearer);
        sb.append(", bearerFormat='").append(bearerFormat).append('\'');
        sb.append(", vendorExtensions=").append(vendorExtensions);
        sb.append(", keyParamName='").append(keyParamName).append('\'');
        sb.append(", isKeyInQuery=").append(isKeyInQuery);
        sb.append(", isKeyInHeader=").append(isKeyInHeader);
        sb.append(", isKeyInCookie=").append(isKeyInCookie);
        sb.append(", flow='").append(flow).append('\'');
        sb.append(", authorizationUrl='").append(authorizationUrl).append('\'');
        sb.append(", tokenUrl='").append(tokenUrl).append('\'');
        sb.append(", scopes=").append(scopes);
        sb.append(", isCode=").append(isCode);
        sb.append(", isPassword=").append(isPassword);
        sb.append(", isApplication=").append(isApplication);
        sb.append(", isImplicit=").append(isImplicit);
        sb.append('}');
        return sb.toString();
    }
}
