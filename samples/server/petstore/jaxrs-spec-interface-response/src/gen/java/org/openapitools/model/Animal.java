package org.openapitools.model;

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.*;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;

@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "className", visible = true)
@JsonSubTypes({
  @JsonSubTypes.Type(value = Dog.class, name = "Dog"),
  @JsonSubTypes.Type(value = Cat.class, name = "Cat"),
  @JsonSubTypes.Type(value = BigCat.class, name = "BigCat"),
})


public class Animal  implements Serializable {
  
  private @Valid String className;
  private @Valid String color = "red";

  public Animal(String className, String color) {
    this.className = className;
    this.color = color;
  }

  
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("className")
  @NotNull
  public String getClassName() {
    return className;
  }

  public void setClassName(String className) {
    this.className = className;
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
    return Objects.equals(this.className, animal.className) &&
        Objects.equals(this.color, animal.color);
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

  public static Builder builder() {
    return new Builder();
  }

  public static class Builder {
    private String className;
    private String color = "red";

    /**
      **/
    public Builder className(String className) {
      this.className = className;
      return this;
    }
    /**
      **/
    public Builder color(String color) {
      this.color = color;
      return this;
    }

    public Animal build() {
      return new Animal(className, color);
    }
  }
}

