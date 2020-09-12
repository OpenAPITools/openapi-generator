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

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.parser.core.models.AuthorizationValue;

import org.openapitools.codegen.auth.AuthParser;

import java.util.List;

public class ClientOptInput {
    private CodegenConfig config;
    private OpenAPI openAPI;
    private List<AuthorizationValue> auths;

    public ClientOptInput openAPI(OpenAPI openAPI) {
        this.setOpenAPI(openAPI);
        return this;
    }

    public ClientOptInput config(CodegenConfig codegenConfig) {
        this.setConfig(codegenConfig);
        return this;
    }

    @Deprecated
    public ClientOptInput auth(String urlEncodedAuthString) {
        this.setAuth(urlEncodedAuthString);
        return this;
    }

    @Deprecated
    public String getAuth() {
        return AuthParser.reconstruct(auths);
    }

    @Deprecated
    public void setAuth(String urlEncodedAuthString) {
        this.auths = AuthParser.parse(urlEncodedAuthString);
    }

    @Deprecated
    public List<AuthorizationValue> getAuthorizationValues() {
        return auths;
    }

    @Deprecated
    public CodegenConfig getConfig() {
        return config;
    }

    /**
     * @deprecated use {@link #config(CodegenConfig)} instead
     * @param config codegen config
     */
    @Deprecated
    public void setConfig(CodegenConfig config) {
        this.config = config;
        // TODO: ClientOptInputs needs to be retired
        if (this.openAPI != null) {
            this.config.setOpenAPI(this.openAPI);
        }
    }

    @Deprecated
    public OpenAPI getOpenAPI() {
        return openAPI;
    }

    /**
     * @deprecated use {@link #openAPI(OpenAPI)} instead
     * @param openAPI the specification
     */
    @Deprecated
    public void setOpenAPI(OpenAPI openAPI) {
        this.openAPI = openAPI;
        // TODO: ClientOptInputs needs to be retired
        if (this.config != null) {
            this.config.setOpenAPI(this.openAPI);
        }
    }
}
