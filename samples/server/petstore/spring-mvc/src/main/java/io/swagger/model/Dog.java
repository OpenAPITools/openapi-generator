package io.swagger.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import io.swagger.model.Animal;

import io.swagger.annotations.*;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.Objects;


@ApiModel(description = "")
@javax.annotation.Generated(value = "class io.swagger.codegen.languages.SpringMVCServerCodegen", date = "2016-04-15T00:36:54.567+08:00")
public class Dog extends Animal {
  
  private String className = null;
  private String breed = null;

  /**
   **/
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
  @ApiModelProperty(value = "")
  @JsonProperty("breed")
  public String getBreed() {
    return breed;
  }
  public void setBreed(String breed) {
    this.breed = breed;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Dog dog = (Dog) o;
    return Objects.equals(className, dog.className) &&
        Objects.equals(breed, dog.breed);
  }

  @Override
  public int hashCode() {
    return Objects.hash(className, breed);
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class Dog {\n");
    sb.append("  " + super.toString()).append("\n");
    sb.append("  className: ").append(className).append("\n");
    sb.append("  breed: ").append(breed).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
