package org.openapitools.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.math.BigDecimal;
import javax.validation.constraints.*;


import io.swagger.annotations.*;
import java.util.Objects;

import javax.xml.bind.annotation.*;


public class TestResponse   {
  
  private Integer id;

  private String stringField = "asd";

  private BigDecimal numberField = new BigDecimal("11");

  private Boolean booleanField = true;


  /**
   **/
  public TestResponse id(Integer id) {
    this.id = id;
    return this;
  }

  
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("id")
  @NotNull
  public Integer getId() {
    return id;
  }
  public void setId(Integer id) {
    this.id = id;
  }


  /**
   **/
  public TestResponse stringField(String stringField) {
    this.stringField = stringField;
    return this;
  }

  
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("stringField")
  @NotNull
  public String getStringField() {
    return stringField;
  }
  public void setStringField(String stringField) {
    this.stringField = stringField;
  }


  /**
   **/
  public TestResponse numberField(BigDecimal numberField) {
    this.numberField = numberField;
    return this;
  }

  
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("numberField")
  @NotNull
  public BigDecimal getNumberField() {
    return numberField;
  }
  public void setNumberField(BigDecimal numberField) {
    this.numberField = numberField;
  }


  /**
   **/
  public TestResponse booleanField(Boolean booleanField) {
    this.booleanField = booleanField;
    return this;
  }

  
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("booleanField")
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
    return Objects.equals(id, testResponse.id) &&
        Objects.equals(stringField, testResponse.stringField) &&
        Objects.equals(numberField, testResponse.numberField) &&
        Objects.equals(booleanField, testResponse.booleanField);
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

