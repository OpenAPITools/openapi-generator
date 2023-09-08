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
import java.util.Map;
import java.util.Objects;

public class CodegenSecurity {
    public String name;
    public String description;
    public String type;
    public String scheme;
    public Boolean isBasic, isOAuth, isApiKey, isOpenId;
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
    public String flow, authorizationUrl, tokenUrl, refreshUrl;
    public List<Map<String, Object>> scopes;
    public Boolean isCode, isPassword, isApplication, isImplicit;
    // OpenId specific
    public String openIdConnectUrl;

    public CodegenSecurity () {
    }

    public CodegenSecurity (CodegenSecurity original) {
        this.name = original.name;
        this.description = original.description;
        this.type = original.type;
        this.scheme = original.scheme;
        this.isBasic = original.isBasic;
        this.isBasicBasic = original.isBasicBasic;
        this.isHttpSignature = original.isHttpSignature;
        this.bearerFormat = original.bearerFormat;
        this.isBasicBearer = original.isBasicBearer;
        this.isApiKey = original.isApiKey;
        this.isOAuth = original.isOAuth;
        this.isOpenId = original.isOpenId;
        this.keyParamName = original.keyParamName;
        this.isCode = original.isCode;
        this.isImplicit = original.isImplicit;
        this.isApplication = original.isApplication;
        this.isPassword = original.isPassword;
        this.isKeyInCookie = original.isKeyInCookie;
        this.isKeyInHeader = original.isKeyInHeader;
        this.isKeyInQuery = original.isKeyInQuery;
        this.flow = original.flow;
        this.tokenUrl = original.tokenUrl;
        this.authorizationUrl = original.authorizationUrl;
        this.refreshUrl = original.refreshUrl;
        this.openIdConnectUrl = original.openIdConnectUrl;

        // It is not possible to deep copy the extensions, as we have no idea what types they are.
        // So the filtered method *will* refer to the original extensions, if any.
        this.vendorExtensions = original.vendorExtensions == null ? null : new HashMap<String, Object>(original.vendorExtensions);

        // It is not possible to deep copy the extensions, as we have no idea what type their values are.
        // So the filtered method *will* refer to the original scopes, if any.
        this.scopes = original.scopes == null ? null : new ArrayList<Map<String, Object>>(original.scopes);
    }

    // Return a copy of the security object, filtering out any scopes from the passed-in list.
    public CodegenSecurity filterByScopeNames(List<String> filterScopes) {
        CodegenSecurity filteredSecurity = new CodegenSecurity(this);

        if (scopes == null) {
            return filteredSecurity;
        }

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
                Objects.equals(description, that.description) &&
                Objects.equals(type, that.type) &&
                Objects.equals(scheme, that.scheme) &&
                Objects.equals(isBasic, that.isBasic) &&
                Objects.equals(isOAuth, that.isOAuth) &&
                Objects.equals(isOpenId, that.isOpenId) &&
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
                Objects.equals(refreshUrl, that.refreshUrl) &&
                Objects.equals(scopes, that.scopes) &&
                Objects.equals(isCode, that.isCode) &&
                Objects.equals(isPassword, that.isPassword) &&
                Objects.equals(isApplication, that.isApplication) &&
                Objects.equals(isImplicit, that.isImplicit) &&
                Objects.equals(openIdConnectUrl, that.openIdConnectUrl);
    }

    @Override
    public int hashCode() {

        return Objects.hash(name, description, type, scheme, isBasic, isOAuth, isOpenId, isApiKey,
                isBasicBasic, isHttpSignature, isBasicBearer, bearerFormat, vendorExtensions,
                keyParamName, isKeyInQuery, isKeyInHeader, isKeyInCookie, flow,
                authorizationUrl, tokenUrl, refreshUrl, scopes, isCode, isPassword, isApplication, isImplicit,
                openIdConnectUrl);
    }

    @Override
    public String toString() {
        final StringBuffer sb = new StringBuffer("CodegenSecurity{");
        sb.append("name='").append(name).append('\'');
        sb.append("description='").append(description).append('\'');
        sb.append(", type='").append(type).append('\'');
        sb.append(", scheme='").append(scheme).append('\'');
        sb.append(", isBasic=").append(isBasic);
        sb.append(", isOAuth=").append(isOAuth);
        sb.append(", isOpenIdConnect=").append(isOpenId);
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
        sb.append(", refreshUrl='").append(refreshUrl).append('\'');
        sb.append(", scopes=").append(scopes);
        sb.append(", isCode=").append(isCode);
        sb.append(", isPassword=").append(isPassword);
        sb.append(", isApplication=").append(isApplication);
        sb.append(", isImplicit=").append(isImplicit);
        sb.append(", openIdConnectUrl=").append(openIdConnectUrl);
        sb.append('}');
        return sb.toString();
    }
}
