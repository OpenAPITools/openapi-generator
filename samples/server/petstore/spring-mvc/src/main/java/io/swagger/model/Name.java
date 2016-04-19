package io.swagger.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;

import io.swagger.annotations.*;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.Objects;


/**
 * Model for testing model name same as property name
 **/
@ApiModel(description = "Model for testing model name same as property name")
@javax.annotation.Generated(value = "class io.swagger.codegen.languages.SpringMVCServerCodegen", date = "2016-04-15T00:36:54.567+08:00")
public class Name  {
  
  private Integer name = null;
  private Integer snakeCase = null;

  /**
   **/
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("name")
  public Integer getName() {
    return name;
  }
  public void setName(Integer name) {
    this.name = name;
  }

  /**
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("snake_case")
  public Integer getSnakeCase() {
    return snakeCase;
  }
  public void setSnakeCase(Integer snakeCase) {
    this.snakeCase = snakeCase;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Name name = (Name) o;
    return Objects.equals(name, name.name) &&
        Objects.equals(snakeCase, name.snakeCase);
  }

  @Override
  public int hashCode() {
    return Objects.hash(name, snakeCase);
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class Name {\n");
    
    sb.append("  name: ").append(name).append("\n");
    sb.append("  snakeCase: ").append(snakeCase).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
