package org.openapitools.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;





@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaInflectorServerCodegen")
public class DogAllOf   {
  @JsonProperty("breed")
  private String breed;

  /**
   **/
  public DogAllOf breed(String breed) {
    this.breed = breed;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("breed")
  public String getBreed() {
    return breed;
  }
  public void setBreed(String breed) {
    this.breed = breed;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    DogAllOf dogAllOf = (DogAllOf) o;
    return Objects.equals(breed, dogAllOf.breed);
  }

  @Override
  public int hashCode() {
    return Objects.hash(breed);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class DogAllOf {\n");
    
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

