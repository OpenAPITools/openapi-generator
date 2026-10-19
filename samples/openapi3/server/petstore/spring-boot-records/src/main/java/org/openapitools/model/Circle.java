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
 * Circle
 *
 * @param radius Get radius
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
public record Circle(
    @JsonInclude(JsonInclude.Include.NON_NULL)
    @NotNull 
    @Schema(name = "radius", requiredMode = Schema.RequiredMode.REQUIRED)
    @JsonProperty("radius")
    Double radius
) implements Shape {

  /**
   * Builder of Circle. Components that are not set get their initial value from the constructor.
   * The builder can be used again after {@link #build()}.
   */
  public static class Builder {

    private Double radius;

    public Builder() {
    }

    protected Builder copyOf(Circle value) {
      this.radius = value.radius();
      return this;
    }

    public Circle.Builder radius(Double radius) {
      this.radius = radius;
      return this;
    }

    /**
     * Returns a new Circle from the values set so far; the builder stays usable.
     */
    public Circle build() {
      return new Circle(this.radius);
    }
  }

  /**
   * Create a builder with no initialized component (the constructor applies the initial values).
   */
  public static Circle.Builder builder() {
    return new Circle.Builder();
  }

  /**
   * Create a builder holding every component of this record.
   */
  public Circle.Builder toBuilder() {
    return new Circle.Builder().copyOf(this);
  }

}

