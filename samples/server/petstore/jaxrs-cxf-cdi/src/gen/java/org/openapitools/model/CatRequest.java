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



public class CatRequest extends PetRequest  {
  
  private Boolean indoor;

  /**
   **/
  public CatRequest indoor(Boolean indoor) {
    this.indoor = indoor;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("indoor")
  public Boolean getIndoor() {
    return indoor;
  }
  public void setIndoor(Boolean indoor) {
    this.indoor = indoor;
  }



  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    CatRequest catRequest = (CatRequest) o;
    return super.equals(o) && Objects.equals(this.indoor, catRequest.indoor);
  }

  @Override
  public int hashCode() {
    return Objects.hash(super.hashCode(), indoor);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class CatRequest {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
    sb.append("    indoor: ").append(toIndentedString(indoor)).append("\n");
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

