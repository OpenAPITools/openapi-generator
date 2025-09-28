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
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * Dog
 */


@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")
public class Dog extends Animal {

  private Optional<String> breed = Optional.empty();

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
    this.breed = Optional.ofNullable(breed);
    return this;
  }

  /**
   * Get breed
   * @return breed
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("breed")
  public Optional<String> getBreed() {
    return breed;
  }

  public void setBreed(Optional<String> breed) {
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
      super(instance); // the parent builder shares the same instance
      this.instance = instance;
    }

    protected Builder copyOf(Dog value) { 
      super.copyOf(value);
      this.instance.setBreed(value.breed);
      return this;
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
  * Create a builder with no initialized field (except for the default values).
  */
  public static Dog.Builder builder() {
    return new Dog.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public Dog.Builder toBuilder() {
    Dog.Builder builder = new Dog.Builder();
    return builder.copyOf(this);
  }

}

