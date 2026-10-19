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
 * Cat
 *
 * @param animalType Get animalType
 * @param name Get name
 * @param lives Get lives
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
public record Cat(
    @JsonInclude(JsonInclude.Include.NON_NULL)
    @NotNull 
    @Schema(name = "animalType", requiredMode = Schema.RequiredMode.REQUIRED)
    @JsonProperty("animalType")
    String animalType,

    @JsonInclude(JsonInclude.Include.NON_NULL)
    @NotNull @Size(min = 2) 
    @Schema(name = "name", requiredMode = Schema.RequiredMode.REQUIRED)
    @JsonProperty("name")
    String name,

    @JsonInclude(JsonInclude.Include.NON_NULL)
    @Min(value = 1) @Max(value = 9) 
    @Schema(name = "lives", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
    @JsonProperty("lives")
    Integer lives
) implements Animal {

  public Cat {
    if (lives == null) {
      lives = 9;
    }
  }

  @JsonProperty("animalType")
  public String getAnimalType() {
    return animalType;
  }

  /**
   * Builder of Cat. Components that are not set get their initial value from the constructor.
   * The builder can be used again after {@link #build()}.
   */
  public static class Builder {

    private String animalType;

    private String name;

    private Integer lives;

    public Builder() {
    }

    protected Builder copyOf(Cat value) {
      this.animalType = value.animalType();
      this.name = value.name();
      this.lives = value.lives();
      return this;
    }

    public Cat.Builder animalType(String animalType) {
      this.animalType = animalType;
      return this;
    }

    public Cat.Builder name(String name) {
      this.name = name;
      return this;
    }

    public Cat.Builder lives(Integer lives) {
      this.lives = lives;
      return this;
    }

    /**
     * Returns a new Cat from the values set so far; the builder stays usable.
     */
    public Cat build() {
      return new Cat(this.animalType, this.name, this.lives);
    }
  }

  /**
   * Create a builder with no initialized component (the constructor applies the initial values).
   */
  public static Cat.Builder builder() {
    return new Cat.Builder();
  }

  /**
   * Create a builder holding every component of this record.
   */
  public Cat.Builder toBuilder() {
    return new Cat.Builder().copyOf(this);
  }

}

