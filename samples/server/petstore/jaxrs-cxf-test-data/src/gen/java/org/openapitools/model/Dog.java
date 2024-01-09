package org.openapitools.model;

import org.openapitools.model.Animal;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.ApiModelProperty;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;


public class Dog extends Animal {
  
  @ApiModelProperty(value = "")
  private String breed;
 /**
  * Get breed
  * @return breed
  */
  @JsonProperty("breed")
  public String getBreed() {
    return breed;
  }

  /**
   * Sets the <code>breed</code> property.
   */
 public void setBreed(String breed) {
    this.breed = breed;
  }

  /**
   * Sets the <code>breed</code> property.
   */
  public Dog breed(String breed) {
    this.breed = breed;
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
    Dog dog = (Dog) o;
    return super.equals(o) && Objects.equals(breed, dog.breed);
  }

  @Override
  public int hashCode() {
    return Objects.hash(super.hashCode(), breed);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Dog {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
    sb.append("    breed: ").append(toIndentedString(breed)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private static String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

