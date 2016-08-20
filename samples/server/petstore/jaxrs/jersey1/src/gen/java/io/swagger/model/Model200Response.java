package io.swagger.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;



/**
 * Model for testing model name starting with number
 **/

/**
 * Model for testing model name starting with number
 */
@ApiModel(description = "Model for testing model name starting with number")

public class Model200Response   {
  @JsonProperty("name")
  private Integer name = null;

  @JsonProperty("class")
  private String PropertyClass = null;

  public Model200Response name(Integer name) {
    this.name = name;
    return this;
  }

   /**
   * Get name
   * @return name
  **/
  @ApiModelProperty(value = "")
  public Integer getName() {
    return name;
  }

  public void setName(Integer name) {
    this.name = name;
  }

  public Model200Response PropertyClass(String PropertyClass) {
    this.PropertyClass = PropertyClass;
    return this;
  }

   /**
   * Get PropertyClass
   * @return PropertyClass
  **/
  @ApiModelProperty(value = "")
  public String getPropertyClass() {
    return PropertyClass;
  }

  public void setPropertyClass(String PropertyClass) {
    this.PropertyClass = PropertyClass;
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
    return Objects.equals(this.name, _200Response.name) &&
        Objects.equals(this.PropertyClass, _200Response.PropertyClass);
  }

  @Override
  public int hashCode() {
    return Objects.hash(name, PropertyClass);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Model200Response {\n");
    
    sb.append("    name: ").append(toIndentedString(name)).append("\n");
    sb.append("    PropertyClass: ").append(toIndentedString(PropertyClass)).append("\n");
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

