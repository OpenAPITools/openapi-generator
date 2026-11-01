package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import org.openapitools.model.Loose;
import org.openapitools.model.Tagged;
import org.springframework.lang.Nullable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * Holder
 *
 * @param tagged Get tagged
 * @param loose Get loose
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
public record Holder(
    @Valid 
    @Schema(name = "tagged", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
    @JsonProperty("tagged")
    @Nullable Tagged tagged,

    @Valid 
    @Schema(name = "loose", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
    @JsonProperty("loose")
    @Nullable Loose loose
) {

  /**
   * Builder of Holder. Components that are not set get their initial value from the constructor.
   * The builder can be used again after {@link #build()}.
   */
  public static class Builder {

    private Tagged tagged;

    private Loose loose;

    public Builder() {
    }

    protected Builder copyOf(Holder value) {
      this.tagged = value.tagged();
      this.loose = value.loose();
      return this;
    }

    public Holder.Builder tagged(Tagged tagged) {
      this.tagged = tagged;
      return this;
    }

    public Holder.Builder loose(Loose loose) {
      this.loose = loose;
      return this;
    }

    /**
     * Returns a new Holder from the values set so far; the builder stays usable.
     */
    public Holder build() {
      return new Holder(this.tagged, this.loose);
    }
  }

  /**
   * Create a builder with no initialized component (the constructor applies the initial values).
   */
  public static Holder.Builder builder() {
    return new Holder.Builder();
  }

  /**
   * Create a builder holding every component of this record.
   */
  public Holder.Builder toBuilder() {
    return new Holder.Builder().copyOf(this);
  }

}

