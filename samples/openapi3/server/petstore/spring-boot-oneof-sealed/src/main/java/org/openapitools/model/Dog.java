package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import javax.annotation.Generated;

/**
 * Dog
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")
public final class Dog implements Animal {

  private @Nullable Boolean bark;

  public Dog bark(@Nullable Boolean bark) {
    this.bark = bark;
    return this;
  }

  /**
   * Get bark
   * @return bark
   */
  
  @Schema(name = "bark", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("bark")
  public @Nullable Boolean getBark() {
    return bark;
  }

  public void setBark(@Nullable Boolean bark) {
    this.bark = bark;
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
    return Objects.equals(this.bark, dog.bark);
  }

  @Override
  public int hashCode() {
    return Objects.hash(bark);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Dog {\n");
    sb.append("    bark: ").append(toIndentedString(bark)).append("\n");
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
  
  public static class Builder {

    private Dog instance;

    public Builder() {
      this(new Dog());
    }

    protected Builder(Dog instance) {
      this.instance = instance;
    }

    protected Builder copyOf(Dog value) { 
      this.instance.setBark(value.bark);
      return this;
    }

    public Dog.Builder bark(Boolean bark) {
      this.instance.bark(bark);
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

