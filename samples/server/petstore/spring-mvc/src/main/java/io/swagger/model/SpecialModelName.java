package io.swagger.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;

import io.swagger.annotations.*;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.Objects;


@ApiModel(description = "")
@javax.annotation.Generated(value = "class io.swagger.codegen.languages.SpringMVCServerCodegen", date = "2016-04-15T00:36:54.567+08:00")
public class SpecialModelName  {
  
  private Long specialPropertyName = null;

  /**
   **/
  @ApiModelProperty(value = "")
  @JsonProperty("$special[property.name]")
  public Long getSpecialPropertyName() {
    return specialPropertyName;
  }
  public void setSpecialPropertyName(Long specialPropertyName) {
    this.specialPropertyName = specialPropertyName;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    SpecialModelName specialModelName = (SpecialModelName) o;
    return Objects.equals(specialPropertyName, specialModelName.specialPropertyName);
  }

  @Override
  public int hashCode() {
    return Objects.hash(specialPropertyName);
  }

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class SpecialModelName {\n");
    
    sb.append("  specialPropertyName: ").append(specialPropertyName).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
