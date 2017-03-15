package io.swagger.model;

import io.swagger.annotations.ApiModel;
import javax.validation.constraints.*;


/**
 * Model for testing model name starting with number
 **/
import io.swagger.annotations.*;
import java.util.Objects;
@ApiModel(description = "Model for testing model name starting with number")

public class Model200Response   {
  
  private Integer name = null;
  private String propertyClass = null;

  /**
   **/
  public Model200Response name(Integer name) {
    this.name = name;
    return this;
  }

  
  @ApiModelProperty(value = "")
  public Integer getName() {
    return name;
  }
  public void setName(Integer name) {
    this.name = name;
  }

  /**
   **/
  public Model200Response propertyClass(String propertyClass) {
    this.propertyClass = propertyClass;
    return this;
  }

  
  @ApiModelProperty(value = "")
  public String getPropertyClass() {
    return propertyClass;
  }
  public void setPropertyClass(String propertyClass) {
    this.propertyClass = propertyClass;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Model200Response _200Response = (Model200Response) o;
    return Objects.equals(name, _200Response.name) &&
        Objects.equals(propertyClass, _200Response.propertyClass);
  }

  @Override
  public int hashCode() {
    return Objects.hash(name, propertyClass);
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
  private String toIndentedString(java.lang.Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}
