package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import org.openapitools.model.Item;
import org.springframework.lang.Nullable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import jakarta.annotation.Generated;

import java.util.Map;
import java.util.HashMap;
import com.fasterxml.jackson.annotation.JsonAnyGetter;
import com.fasterxml.jackson.annotation.JsonAnySetter;
/**
 * Typed
 *
 * @param id Get id
 * @param additionalProperties the additional (undeclared) properties, by name
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
public record Typed(
    
    @Schema(name = "id", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
    @JsonProperty("id")
    @Nullable String id,

    @JsonAnySetter
    @JsonAnyGetter
    Map<String, Item> additionalProperties
) {

  public Typed {
    if (additionalProperties == null) {
      additionalProperties = new LinkedHashMap<>();
    }
  }

  /**
   * Return the additional (undeclared) property with the specified name.
   */
  public Item getAdditionalProperty(String key) {
    return additionalProperties.get(key);
  }

  /**
   * Builder of Typed. Components that are not set get their initial value from the constructor.
   * The builder can be used again after {@link #build()}.
   */
  public static class Builder {

    private String id;

    private Map<String, Item> additionalProperties;

    public Builder() {
    }

    protected Builder copyOf(Typed value) {
      this.id = value.id();
      this.additionalProperties = value.additionalProperties() == null ? null : new LinkedHashMap<>(value.additionalProperties());
      return this;
    }

    public Typed.Builder id(String id) {
      this.id = id;
      return this;
    }

    public Typed.Builder additionalProperties(Map<String, Item> additionalProperties) {
      this.additionalProperties = additionalProperties;
      return this;
    }

    /**
     * Returns a new Typed from the values set so far; the builder stays usable.
     */
    public Typed build() {
      return new Typed(this.id, this.additionalProperties);
    }
  }

  /**
   * Create a builder with no initialized component (the constructor applies the initial values).
   */
  public static Typed.Builder builder() {
    return new Typed.Builder();
  }

  /**
   * Create a builder holding every component of this record.
   */
  public Typed.Builder toBuilder() {
    return new Typed.Builder().copyOf(this);
  }

}

