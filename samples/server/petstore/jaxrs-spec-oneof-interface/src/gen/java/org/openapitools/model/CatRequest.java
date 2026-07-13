package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.openapitools.model.PetType;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.*;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.annotation.JsonTypeName;
import org.openapitools.jackson.nullable.JsonNullable;



@JsonTypeName("CatRequest")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.24.0-SNAPSHOT")
public class CatRequest  implements Serializable, PetRequest {
  private PetType petType;
  private String name;
  private Boolean indoor;

  public CatRequest() {
  }

  @JsonCreator
  public CatRequest(
    @JsonProperty(required = true, value = "petType") PetType petType,
    @JsonProperty(required = true, value = "name") String name,
    @JsonProperty(required = true, value = "indoor") Boolean indoor
  ) {
    this.petType = petType;
    this.name = name;
    this.indoor = indoor;
  }

  /**
   **/
  public CatRequest petType(PetType petType) {
    this.petType = petType;
    return this;
  }

  
  @ApiModelProperty(required = true, value = "")
  @JsonProperty(required = true, value = "petType")
  @NotNull public PetType getPetType() {
    return petType;
  }

  @JsonProperty(required = true, value = "petType")
  public void setPetType(PetType petType) {
    this.petType = petType;
  }

  /**
   **/
  public CatRequest name(String name) {
    this.name = name;
    return this;
  }

  
  @ApiModelProperty(required = true, value = "")
  @JsonProperty(required = true, value = "name")
  @NotNull public String getName() {
    return name;
  }

  @JsonProperty(required = true, value = "name")
  public void setName(String name) {
    this.name = name;
  }

  /**
   **/
  public CatRequest indoor(Boolean indoor) {
    this.indoor = indoor;
    return this;
  }

  
  @ApiModelProperty(required = true, value = "")
  @JsonProperty(required = true, value = "indoor")
  @NotNull public Boolean getIndoor() {
    return indoor;
  }

  @JsonProperty(required = true, value = "indoor")
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
    return Objects.equals(this.petType, catRequest.petType) &&
        Objects.equals(this.name, catRequest.name) &&
        Objects.equals(this.indoor, catRequest.indoor);
  }

  @Override
  public int hashCode() {
    return Objects.hash(petType, name, indoor);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class CatRequest {\n");
    
    sb.append("    petType: ").append(toIndentedString(petType)).append("\n");
    sb.append("    name: ").append(toIndentedString(name)).append("\n");
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
