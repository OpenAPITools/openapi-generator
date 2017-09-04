package io.swagger.model;

import javax.validation.constraints.*;
import javax.validation.Valid;


import io.swagger.annotations.*;
import java.util.Objects;


public class SpecialModelName   {
  
  private @Valid Long specialPropertyName = null;

  /**
   **/
  public SpecialModelName specialPropertyName(Long specialPropertyName) {
    this.specialPropertyName = specialPropertyName;
    return this;
  }

  
  @ApiModelProperty(value = "")
  public Long getSpecialPropertyName() {
    return specialPropertyName;
  }
  public void setSpecialPropertyName(Long specialPropertyName) {
    this.specialPropertyName = specialPropertyName;
  }


  @Override
  public boolean equals(java.lang.Object o) {
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
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class SpecialModelName {\n");
    
    sb.append("    specialPropertyName: ").append(toIndentedString(specialPropertyName)).append("\n");
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

