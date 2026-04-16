package org.openapitools.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.openapitools.model.PetRequest;
import javax.validation.constraints.*;
import javax.validation.Valid;


import io.swagger.annotations.*;
import java.util.Objects;



public class DogRequest extends PetRequest  {
  
  private String bark;

  /**
   **/
  public DogRequest bark(String bark) {
    this.bark = bark;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("bark")
  public String getBark() {
    return bark;
  }
  public void setBark(String bark) {
    this.bark = bark;
  }



  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    DogRequest dogRequest = (DogRequest) o;
    return super.equals(o) && Objects.equals(this.bark, dogRequest.bark);
  }

  @Override
  public int hashCode() {
    return Objects.hash(super.hashCode(), bark);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class DogRequest {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
    sb.append("    bark: ").append(toIndentedString(bark)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private String toIndentedString(Object o) {
    return o == null ? "null" : o.toString().replace("\n", "\n    ");
  }
}

