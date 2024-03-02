package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.openapitools.model.Animal;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * Dog
 */


@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class Dog extends Animal {

  private String breed;

  public Dog() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public Dog(String className) {
    super(className);
  }

  public Dog breed(String breed) {
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


  public Dog className(String className) {
    super.className(className);
    return this;
  }

  public Dog color(String color) {
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
    Dog dog = (Dog) o;
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
  private String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
  
  public static class Builder extends Animal.Builder {

    private Dog instance;

    public Builder() {
      this(new Dog());
    }

    protected Builder(Dog instance) {
      super(instance);
      this.instance = instance;
    }

    public Dog.Builder breed(String breed) {
      this.instance.breed(breed);
      return this;
    }

    @Override
    public Dog.Builder className(String className) {
      this.instance.className(className);
      return this;
    }

    @Override
    public Dog.Builder color(String color) {
      this.instance.color(color);
      return this;
    }
    /**
    * returns a built Dog instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public Dog build() {
      try {
        return this.instance;
      } finally {
        // ensure that this.instance is not reused
        super.build();
        this.instance = null;
      }
  }

    @Override
    public String toString() {
      return getClass() + "=(" + instance + ")";
    }
  }

  /**
  * Create a builder with no initialized field.
  */
  public static Dog.Builder builder() {
    return new Dog.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public Dog.Builder toBuilder() {
    Dog.Builder builder = new Dog.Builder();
    builder.instance.setBreed(breed);
    builder.instance.setClassName(getClassName());
    builder.instance.setColor(getColor());
    return builder;
  }

}

