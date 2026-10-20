package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.openapitools.model.Animal;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * Zoo
 *
 * @param keeper Get keeper
 * @param animals Get animals
 * @param mascot Get mascot
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
public record Zoo(
    @JsonInclude(JsonInclude.Include.NON_NULL)
    
    @Schema(name = "keeper", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
    @JsonProperty("keeper")
    @Nullable String keeper,

    @JsonInclude(JsonInclude.Include.NON_NULL)
    
    @Schema(name = "animals", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
    @JsonProperty("animals")
    List<@Valid Animal> animals,

    @JsonInclude(JsonInclude.Include.NON_NULL)
    @Valid 
    @Schema(name = "mascot", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
    @JsonProperty("mascot")
    @Nullable Animal mascot
) {

  public Zoo {
    if (animals == null) {
      animals = new ArrayList<>();
    }
  }

  /**
   * Builder of Zoo. Components that are not set get their initial value from the constructor.
   * The builder can be used again after {@link #build()}.
   */
  public static class Builder {

    private String keeper;

    private List<Animal> animals;

    private Animal mascot;

    public Builder() {
    }

    protected Builder copyOf(Zoo value) {
      this.keeper = value.keeper();
      this.animals = value.animals();
      this.mascot = value.mascot();
      return this;
    }

    public Zoo.Builder keeper(String keeper) {
      this.keeper = keeper;
      return this;
    }

    public Zoo.Builder animals(List<Animal> animals) {
      this.animals = animals;
      return this;
    }

    public Zoo.Builder mascot(Animal mascot) {
      this.mascot = mascot;
      return this;
    }

    /**
     * Returns a new Zoo from the values set so far; the builder stays usable.
     */
    public Zoo build() {
      return new Zoo(this.keeper, this.animals, this.mascot);
    }
  }

  /**
   * Create a builder with no initialized component (the constructor applies the initial values).
   */
  public static Zoo.Builder builder() {
    return new Zoo.Builder();
  }

  /**
   * Create a builder holding every component of this record.
   */
  public Zoo.Builder toBuilder() {
    return new Zoo.Builder().copyOf(this);
  }

}

