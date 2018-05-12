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

package org.openapitools.codegen.online.model;

import com.fasterxml.jackson.databind.JsonNode;
import io.swagger.annotations.ApiModelProperty;
import io.swagger.models.auth.AuthorizationValue;
import io.swagger.models.auth.SecuritySchemeDefinition;

import java.util.Map;

public class GeneratorInput {
    private JsonNode spec;
    private Map<String, String> options;
    private String swaggerUrl;
    private SecuritySchemeDefinition auth;
    private AuthorizationValue authorizationValue;

    public AuthorizationValue getAuthorizationValue() {
        return authorizationValue;
    }

    public void setAuthorizationValue(AuthorizationValue authorizationValue) {
        this.authorizationValue = authorizationValue;
    }

    @ApiModelProperty(dataType = "Object")
    public JsonNode getSpec() {
        return spec;
    }

    public void setSpec(JsonNode spec) {
        this.spec = spec;
    }

    public Map<String, String> getOptions() {
        return options;
    }

    public void setOptions(Map<String, String> options) {
        this.options = options;
    }

    @ApiModelProperty(example = "http://petstore.swagger.io/v2/swagger.json")
    public String getSwaggerUrl() {
        return swaggerUrl;
    }

    public void setSwaggerUrl(String url) {
        this.swaggerUrl = url;
    }

    @Deprecated
    public SecuritySchemeDefinition getSecurityDefinition() {
        return auth;
    }

    @Deprecated
    public void setSecurityDefinition(SecuritySchemeDefinition auth) {
        this.auth = auth;
    }
}
