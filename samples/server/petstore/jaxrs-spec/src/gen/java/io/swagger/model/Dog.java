package io.swagger.model;

import io.swagger.model.Animal;
import javax.validation.constraints.*;


import io.swagger.annotations.*;
import java.util.Objects;


public class Dog extends Animal  {
  
  private String breed = null;

  /**
   **/
  public Dog breed(String breed) {
    this.breed = breed;
    return this;
  }

  
  @ApiModelProperty(value = "")
  public String getBreed() {
    return breed;
  }
  public void setBreed(String breed) {
    this.breed = breed;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Dog dog = (Dog) o;
    return Objects.equals(breed, dog.breed);
  }

  @Override
  public int hashCode() {
    return Objects.hash(breed);
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
  private String toIndentedString(java.lang.Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

