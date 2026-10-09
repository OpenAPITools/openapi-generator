package org.openapitools.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.openapitools.model.CatRequest;
import org.openapitools.model.DogRequest;
import javax.validation.constraints.*;
import javax.validation.Valid;


import io.swagger.annotations.*;
import java.util.Objects;


@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "petType", visible = true)
@JsonSubTypes({
  @JsonSubTypes.Type(value = CatRequest.class, name = "CAT"),
  @JsonSubTypes.Type(value = DogRequest.class, name = "DOG"),
})

public class PetRequest   {
  
  private String petType;

  private String name;

  private String bark;

  private Boolean indoor;

  /**
   **/
  public PetRequest petType(String petType) {
    this.petType = petType;
    return this;
  }

  
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("petType")
  @NotNull
  public String getPetType() {
    return petType;
  }
  public void setPetType(String petType) {
    this.petType = petType;
  }


  /**
   **/
  public PetRequest name(String name) {
    this.name = name;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("name")
  public String getName() {
    return name;
  }
  public void setName(String name) {
    this.name = name;
  }


  /**
   **/
  public PetRequest bark(String bark) {
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


  /**
   **/
  public PetRequest indoor(Boolean indoor) {
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
    PetRequest petRequest = (PetRequest) o;
    return Objects.equals(this.petType, petRequest.petType) &&
        Objects.equals(this.name, petRequest.name) &&
        Objects.equals(this.bark, petRequest.bark) &&
        Objects.equals(this.indoor, petRequest.indoor);
  }

  @Override
  public int hashCode() {
    return Objects.hash(petType, name, bark, indoor);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class PetRequest {\n");
    
    sb.append("    petType: ").append(toIndentedString(petType)).append("\n");
    sb.append("    name: ").append(toIndentedString(name)).append("\n");
    sb.append("    bark: ").append(toIndentedString(bark)).append("\n");
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

