package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.ApiModelProperty;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;


/**
 * Model for testing model name starting with number
 */
@ApiModel(description="Model for testing model name starting with number")

public class Model200Response  {
  
  @ApiModelProperty(value = "")
  private Integer name;

  @ApiModelProperty(value = "")
  private String propertyClass;
 /**
  * Get name
  * @return name
  */
  @JsonProperty("name")
  public Integer getName() {
    return name;
  }

  /**
   * Sets the <code>name</code> property.
   */
 public void setName(Integer name) {
    this.name = name;
  }

  /**
   * Sets the <code>name</code> property.
   */
  public Model200Response name(Integer name) {
    this.name = name;
    return this;
  }

 /**
  * Get propertyClass
  * @return propertyClass
  */
  @JsonProperty("class")
  public String getPropertyClass() {
    return propertyClass;
  }

  /**
   * Sets the <code>propertyClass</code> property.
   */
 public void setPropertyClass(String propertyClass) {
    this.propertyClass = propertyClass;
  }

  /**
   * Sets the <code>propertyClass</code> property.
   */
  public Model200Response propertyClass(String propertyClass) {
    this.propertyClass = propertyClass;
    return this;
  }


  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Model200Response {\n");
    
    sb.append("    name: ").append(toIndentedString(name)).append("\n");
    sb.append("    propertyClass: ").append(toIndentedString(propertyClass)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private static String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

