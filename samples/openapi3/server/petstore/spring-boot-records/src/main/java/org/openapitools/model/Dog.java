package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import org.springframework.lang.Nullable;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * Dog
 *
 * @param name Get name
 * @param barks Get barks
 * @param animalType Get animalType
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
public record Dog(
    @JsonInclude(JsonInclude.Include.NON_NULL)
    @NotNull @Size(min = 2) 
    @Schema(name = "name", requiredMode = Schema.RequiredMode.REQUIRED)
    @JsonProperty("name")
    String name,

    @JsonInclude(JsonInclude.Include.NON_NULL)
    
    @Schema(name = "barks", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
    @JsonProperty("barks")
    @Nullable Boolean barks,

    @JsonInclude(JsonInclude.Include.NON_NULL)
    @NotNull 
    @Schema(name = "animalType", requiredMode = Schema.RequiredMode.REQUIRED)
    @JsonProperty("animalType")
    String animalType
) implements Animal {

  @JsonProperty("animalType")
  public String getAnimalType() {
    return animalType;
  }

  /**
   * Builder of Dog. Components that are not set get their initial value from the constructor.
   * The builder can be used again after {@link #build()}.
   */
  public static class Builder {

    private String name;

    private Boolean barks;

    private String animalType;

    public Builder() {
    }

    protected Builder copyOf(Dog value) {
      this.name = value.name();
      this.barks = value.barks();
      this.animalType = value.animalType();
      return this;
    }

    public Dog.Builder name(String name) {
      this.name = name;
      return this;
    }

    public Dog.Builder barks(Boolean barks) {
      this.barks = barks;
      return this;
    }

    public Dog.Builder animalType(String animalType) {
      this.animalType = animalType;
      return this;
    }

    /**
     * Returns a new Dog from the values set so far; the builder stays usable.
     */
    public Dog build() {
      return new Dog(this.name, this.barks, this.animalType);
    }
  }

  /**
   * Create a builder with no initialized component (the constructor applies the initial values).
   */
  public static Dog.Builder builder() {
    return new Dog.Builder();
  }

  /**
   * Create a builder holding every component of this record.
   */
  public Dog.Builder toBuilder() {
    return new Dog.Builder().copyOf(this);
  }

}

