package org.openapitools.codegen.online.model;

import com.fasterxml.jackson.databind.JsonNode;
import io.swagger.annotations.ApiModelProperty;
import io.swagger.models.auth.SecuritySchemeDefinition;
import io.swagger.v3.parser.core.models.AuthorizationValue;

import java.util.Map;

public class GeneratorInput {
    private JsonNode spec;
    private Map<String, String> options;
    private String openAPIUrl;
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
    public String getOpenAPIUrl() {
        return openAPIUrl;
    }

    public void setOpenAPIUrl(String url) {
        this.openAPIUrl = url;
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
