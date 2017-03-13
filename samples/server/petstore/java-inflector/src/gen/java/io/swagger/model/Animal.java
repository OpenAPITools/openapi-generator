package io.swagger.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;






public class Animal   {
  @JsonProperty("className")
  private String className = null;

  @JsonProperty("color")
  private String color = "red";

  /**
   **/
  public Animal className(String className) {
    this.className = className;
    return this;
  }

  
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("className")
  public String getClassName() {
    return className;
  }
  public void setClassName(String className) {
    this.className = className;
  }

  /**
   **/
  public Animal color(String color) {
    this.color = color;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("color")
  public String getColor() {
    return color;
  }
  public void setColor(String color) {
    this.color = color;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Animal animal = (Animal) o;
    return Objects.equals(className, animal.className) &&
        Objects.equals(color, animal.color);
  }

  @Override
  public int hashCode() {
    return Objects.hash(className, color);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Animal {\n");
    
    sb.append("    className: ").append(toIndentedString(className)).append("\n");
    sb.append("    color: ").append(toIndentedString(color)).append("\n");
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

