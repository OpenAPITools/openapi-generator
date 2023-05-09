package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.openapitools.model.Animal;
import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.*;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.annotation.JsonTypeName;



@org.eclipse.microprofile.openapi.annotations.media.Schema(description="")
@JsonTypeName("Dog")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen")
public class Dog extends Animal implements Serializable {
  private @Valid String breed;

  protected Dog(DogBuilder<?, ?> b) {
    super(b);
    this.breed = b.breed;
  }

  public Dog() {
  }

  /**
   **/
  public Dog breed(String breed) {
    this.breed = breed;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @org.eclipse.microprofile.openapi.annotations.media.Schema(description = "")
  @JsonProperty("breed")
  public String getBreed() {
    return breed;
  }

  @JsonProperty("breed")
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


  public static DogBuilder<?, ?> builder() {
    return new DogBuilderImpl();
  }

  private static final class DogBuilderImpl extends DogBuilder<Dog, DogBuilderImpl> {

    @Override
    protected DogBuilderImpl self() {
      return this;
    }

    @Override
    public Dog build() {
      return new Dog(this);
    }
  }

  public static abstract class DogBuilder<C extends Dog, B extends DogBuilder<C, B>> extends AnimalBuilder<C, B> {
    private String breed;

    public B breed(String breed) {
      this.breed = breed;
      return self();
    }
  }
}

