package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import java.util.Map;
import org.openapitools.model.Base;
import org.springframework.lang.Nullable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * Child
 *
 * @param kind Get kind
 * @param x Get x
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
public record Child(
    @NotNull 
    @Schema(name = "kind", requiredMode = Schema.RequiredMode.REQUIRED)
    @JsonProperty("kind")
    String kind,

    
    @Schema(name = "x", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
    @JsonProperty("x")
    @Nullable String x
) implements Base {

  /**
   * Builder of Child. Components that are not set get their initial value from the constructor.
   * The builder can be used again after {@link #build()}.
   */
  public static class Builder {

    private String kind;

    private String x;

    public Builder() {
    }

    protected Builder copyOf(Child value) {
      this.kind = value.kind();
      this.x = value.x();
      return this;
    }

    public Child.Builder kind(String kind) {
      this.kind = kind;
      return this;
    }

    public Child.Builder x(String x) {
      this.x = x;
      return this;
    }

    /**
     * Returns a new Child from the values set so far; the builder stays usable.
     */
    public Child build() {
      return new Child(this.kind, this.x);
    }
  }

  /**
   * Create a builder with no initialized component (the constructor applies the initial values).
   */
  public static Child.Builder builder() {
    return new Child.Builder();
  }

  /**
   * Create a builder holding every component of this record.
   */
  public Child.Builder toBuilder() {
    return new Child.Builder().copyOf(this);
  }

}

