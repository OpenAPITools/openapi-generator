package org.openapitools.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import javax.validation.constraints.*;


import io.swagger.annotations.*;
import java.util.Objects;

import javax.xml.bind.annotation.*;


public class UpdatePetWithFormBody   {
  
  private String name;

  private String status;


  /**
   * Updated name of the pet
   **/
  public UpdatePetWithFormBody name(String name) {
    this.name = name;
    return this;
  }

  
  @ApiModelProperty(value = "Updated name of the pet")
  @JsonProperty("name")
  public String getName() {
    return name;
  }
  public void setName(String name) {
    this.name = name;
  }


  /**
   * Updated status of the pet
   **/
  public UpdatePetWithFormBody status(String status) {
    this.status = status;
    return this;
  }

  
  @ApiModelProperty(value = "Updated status of the pet")
  @JsonProperty("status")
  public String getStatus() {
    return status;
  }
  public void setStatus(String status) {
    this.status = status;
  }



  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    UpdatePetWithFormBody updatePetWithFormBody = (UpdatePetWithFormBody) o;
    return Objects.equals(name, updatePetWithFormBody.name) &&
        Objects.equals(status, updatePetWithFormBody.status);
  }

  @Override
  public int hashCode() {
    return Objects.hash(name, status);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class UpdatePetWithFormBody {\n");
    
    sb.append("    name: ").append(toIndentedString(name)).append("\n");
    sb.append("    status: ").append(toIndentedString(status)).append("\n");
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

