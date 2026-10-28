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
 * Range
 *
 * @param from Get from
 * @param to Get to
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
public record Range(
    @JsonInclude(JsonInclude.Include.NON_NULL)
    
    @Schema(name = "from", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
    @JsonProperty("from")
    @Nullable Integer from,

    @JsonInclude(JsonInclude.Include.NON_NULL)
    
    @Schema(name = "to", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
    @JsonProperty("to")
    @Nullable Integer to
) {

  /**
   * Builder of Range. Components that are not set get their initial value from the constructor.
   * The builder can be used again after {@link #build()}.
   */
  public static class Builder {

    private Integer from;

    private Integer to;

    public Builder() {
    }

    protected Builder copyOf(Range value) {
      this.from = value.from();
      this.to = value.to();
      return this;
    }

    public Range.Builder from(Integer from) {
      this.from = from;
      return this;
    }

    public Range.Builder to(Integer to) {
      this.to = to;
      return this;
    }

    /**
     * Returns a new Range from the values set so far; the builder stays usable.
     */
    public Range build() {
      return new Range(this.from, this.to);
    }
  }

  /**
   * Create a builder with no initialized component (the constructor applies the initial values).
   */
  public static Range.Builder builder() {
    return new Range.Builder();
  }

  /**
   * Create a builder holding every component of this record.
   */
  public Range.Builder toBuilder() {
    return new Range.Builder().copyOf(this);
  }

}

