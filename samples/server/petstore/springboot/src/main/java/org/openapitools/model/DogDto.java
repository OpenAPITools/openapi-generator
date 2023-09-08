package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.annotation.JsonTypeName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.openapitools.model.AnimalDto;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * DogDto
 */


@JsonTypeName("Dog")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class DogDto extends AnimalDto {

  private String breed;

  public DogDto() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public DogDto(String className) {
    super(className);
  }

  public DogDto breed(String breed) {
    this.breed = breed;
    return this;
  }

  /**
   * Get breed
   * @return breed
  */
  
  @ApiModelProperty(value = "")
  @JsonProperty("breed")
  public String getBreed() {
    return breed;
  }

  public void setBreed(String breed) {
    this.breed = breed;
  }


  public DogDto className(String className) {
    super.className(className);
    return this;
  }

  public DogDto color(String color) {
    super.color(color);
    return this;
  }
  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    DogDto dog = (DogDto) o;
    return Objects.equals(this.breed, dog.breed) &&
        super.equals(o);
  }

  @Override
  public int hashCode() {
    return Objects.hash(breed, super.hashCode());
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class DogDto {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
    sb.append("    breed: ").append(toIndentedString(breed)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

