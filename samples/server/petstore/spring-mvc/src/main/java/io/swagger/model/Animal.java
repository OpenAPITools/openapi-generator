package io.swagger.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;

import io.swagger.annotations.*;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.Objects;


@ApiModel(description = "")
@javax.annotation.Generated(value = "class io.swagger.codegen.languages.SpringMVCServerCodegen", date = "2016-04-15T00:36:54.567+08:00")
public class Animal  {
  
  private String className = null;

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


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Animal animal = (Animal) o;
    return Objects.equals(className, animal.className);
  }

  @Override
  public int hashCode() {
    return Objects.hash(className);
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class Animal {\n");
    
    sb.append("  className: ").append(className).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
