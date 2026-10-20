package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * Square
 *
 * @param side Get side
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
public record Square(
    @JsonInclude(JsonInclude.Include.NON_NULL)
    @NotNull 
    @Schema(name = "side", requiredMode = Schema.RequiredMode.REQUIRED)
    @JsonProperty("side")
    Double side
) implements Shape {

  /**
   * Builder of Square. Components that are not set get their initial value from the constructor.
   * The builder can be used again after {@link #build()}.
   */
  public static class Builder {

    private Double side;

    public Builder() {
    }

    protected Builder copyOf(Square value) {
      this.side = value.side();
      return this;
    }

    public Square.Builder side(Double side) {
      this.side = side;
      return this;
    }

    /**
     * Returns a new Square from the values set so far; the builder stays usable.
     */
    public Square build() {
      return new Square(this.side);
    }
  }

  /**
   * Create a builder with no initialized component (the constructor applies the initial values).
   */
  public static Square.Builder builder() {
    return new Square.Builder();
  }

  /**
   * Create a builder holding every component of this record.
   */
  public Square.Builder toBuilder() {
    return new Square.Builder().copyOf(this);
  }

}

