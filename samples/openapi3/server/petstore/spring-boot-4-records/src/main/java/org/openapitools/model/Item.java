package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import org.springframework.lang.Nullable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * Item
 *
 * @param value Get value
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
public record Item(
    
    @Schema(name = "value", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
    @JsonProperty("value")
    @Nullable String value
) {

  /**
   * Builder of Item. Components that are not set get their initial value from the constructor.
   * The builder can be used again after {@link #build()}.
   */
  public static class Builder {

    private String value;

    public Builder() {
    }

    protected Builder copyOf(Item value) {
      this.value = value.value();
      return this;
    }

    public Item.Builder value(String value) {
      this.value = value;
      return this;
    }

    /**
     * Returns a new Item from the values set so far; the builder stays usable.
     */
    public Item build() {
      return new Item(this.value);
    }
  }

  /**
   * Create a builder with no initialized component (the constructor applies the initial values).
   */
  public static Item.Builder builder() {
    return new Item.Builder();
  }

  /**
   * Create a builder holding every component of this record.
   */
  public Item.Builder toBuilder() {
    return new Item.Builder().copyOf(this);
  }

}

