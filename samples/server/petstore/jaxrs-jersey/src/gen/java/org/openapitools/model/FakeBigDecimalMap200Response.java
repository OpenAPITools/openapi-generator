/*
 * OpenAPI Petstore
 * This spec is mainly for testing Petstore server and contains fake endpoints, models. Please do not use this for any other purpose. Special characters: \" \\
 *
 * The version of the OpenAPI document: 1.0.0
 * 
 *
 * NOTE: This class is auto generated by OpenAPI Generator (https://openapi-generator.tech).
 * https://openapi-generator.tech
 * Do not edit the class manually.
 */


package org.openapitools.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.math.BigDecimal;
import java.util.HashMap;
import java.util.Map;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;
import javax.validation.constraints.*;
import javax.validation.Valid;

/**
 * FakeBigDecimalMap200Response
 */
@JsonPropertyOrder({
  FakeBigDecimalMap200Response.JSON_PROPERTY_SOME_ID,
  FakeBigDecimalMap200Response.JSON_PROPERTY_SOME_MAP
})
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJerseyServerCodegen", comments = "Generator version: 7.15.0-SNAPSHOT")
public class FakeBigDecimalMap200Response   {
  public static final String JSON_PROPERTY_SOME_ID = "someId";
  @JsonProperty(JSON_PROPERTY_SOME_ID)
  private BigDecimal someId;

  public static final String JSON_PROPERTY_SOME_MAP = "someMap";
  @JsonProperty(JSON_PROPERTY_SOME_MAP)
  private Map<String, BigDecimal> someMap = new HashMap<>();

  public FakeBigDecimalMap200Response someId(BigDecimal someId) {
    this.someId = someId;
    return this;
  }

  /**
   * Get someId
   * @return someId
   **/
  @JsonProperty(value = "someId")
  @ApiModelProperty(value = "")
  @Valid 
  public BigDecimal getSomeId() {
    return someId;
  }

  public void setSomeId(BigDecimal someId) {
    this.someId = someId;
  }

  public FakeBigDecimalMap200Response someMap(Map<String, BigDecimal> someMap) {
    this.someMap = someMap;
    return this;
  }

  public FakeBigDecimalMap200Response putSomeMapItem(String key, BigDecimal someMapItem) {
    if (this.someMap == null) {
      this.someMap = new HashMap<>();
    }
    this.someMap.put(key, someMapItem);
    return this;
  }

  /**
   * Get someMap
   * @return someMap
   **/
  @JsonProperty(value = "someMap")
  @ApiModelProperty(value = "")
  @Valid 
  public Map<String, BigDecimal> getSomeMap() {
    return someMap;
  }

  public void setSomeMap(Map<String, BigDecimal> someMap) {
    this.someMap = someMap;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    FakeBigDecimalMap200Response fakeBigDecimalMap200Response = (FakeBigDecimalMap200Response) o;
    return Objects.equals(this.someId, fakeBigDecimalMap200Response.someId) &&
        Objects.equals(this.someMap, fakeBigDecimalMap200Response.someMap);
  }

  @Override
  public int hashCode() {
    return Objects.hash(someId, someMap);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class FakeBigDecimalMap200Response {\n");
    
    sb.append("    someId: ").append(toIndentedString(someId)).append("\n");
    sb.append("    someMap: ").append(toIndentedString(someMap)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

