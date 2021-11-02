package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.math.BigDecimal;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;

/**
 * TestResponse
 */
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class TestResponse   {
  @JsonProperty("id")
  private Integer id;

  @JsonProperty("stringField")
  private String stringField = "asd";

  @JsonProperty("numberField")
  private BigDecimal numberField = new BigDecimal("11");

  @JsonProperty("booleanField")
  private Boolean booleanField = true;

  public TestResponse id(Integer id) {
    this.id = id;
    return this;
  }

  /**
   * Get id
   * @return id
  */
  @ApiModelProperty(required = true, value = "")
  @NotNull


  public Integer getId() {
    return id;
  }

  public void setId(Integer id) {
    this.id = id;
  }

  public TestResponse stringField(String stringField) {
    this.stringField = stringField;
    return this;
  }

  /**
   * Get stringField
   * @return stringField
  */
  @ApiModelProperty(required = true, value = "")
  @NotNull


  public String getStringField() {
    return stringField;
  }

  public void setStringField(String stringField) {
    this.stringField = stringField;
  }

  public TestResponse numberField(BigDecimal numberField) {
    this.numberField = numberField;
    return this;
  }

  /**
   * Get numberField
   * @return numberField
  */
  @ApiModelProperty(required = true, value = "")
  @NotNull

  @Valid

  public BigDecimal getNumberField() {
    return numberField;
  }

  public void setNumberField(BigDecimal numberField) {
    this.numberField = numberField;
  }

  public TestResponse booleanField(Boolean booleanField) {
    this.booleanField = booleanField;
    return this;
  }

  /**
   * Get booleanField
   * @return booleanField
  */
  @ApiModelProperty(required = true, value = "")
  @NotNull


  public Boolean getBooleanField() {
    return booleanField;
  }

  public void setBooleanField(Boolean booleanField) {
    this.booleanField = booleanField;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    TestResponse testResponse = (TestResponse) o;
    return Objects.equals(this.id, testResponse.id) &&
        Objects.equals(this.stringField, testResponse.stringField) &&
        Objects.equals(this.numberField, testResponse.numberField) &&
        Objects.equals(this.booleanField, testResponse.booleanField);
  }

  @Override
  public int hashCode() {
    return Objects.hash(id, stringField, numberField, booleanField);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class TestResponse {\n");
    
    sb.append("    id: ").append(toIndentedString(id)).append("\n");
    sb.append("    stringField: ").append(toIndentedString(stringField)).append("\n");
    sb.append("    numberField: ").append(toIndentedString(numberField)).append("\n");
    sb.append("    booleanField: ").append(toIndentedString(booleanField)).append("\n");
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

