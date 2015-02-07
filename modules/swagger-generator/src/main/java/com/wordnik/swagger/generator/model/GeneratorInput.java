package com.wordnik.swagger.generator.model;

import com.wordnik.swagger.annotations.ApiModelProperty;
import com.wordnik.swagger.models.auth.SecuritySchemeDefinition;

import com.fasterxml.jackson.databind.*;

import java.util.*;

public class GeneratorInput {
  private JsonNode spec;
  private Map<String, String> options;
  private String swaggerUrl;
  private SecuritySchemeDefinition auth;

  @ApiModelProperty(dataType="Object")
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

  public String getSwaggerUrl() {
    return swaggerUrl;
  }
  public void setSwaggerUrl(String url) {
    this.swaggerUrl = url;
  }

  public SecuritySchemeDefinition getSecurityDefinition() {
    return auth;
  }
  public void setSecurityDefinition(SecuritySchemeDefinition auth) {
    this.auth = auth;
  }
}