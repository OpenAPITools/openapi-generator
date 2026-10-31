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

import java.util.Map;
import java.util.HashMap;
import com.fasterxml.jackson.annotation.JsonAnyGetter;
import com.fasterxml.jackson.annotation.JsonAnySetter;
/**
 * A string-valued map next to declared properties
 *
 * @param name Get name
 * @param count Get count
 * @param note Get note
 * @param secret Get secret
 * @param additionalProperties the additional (undeclared) properties, by name
 */

@Schema(name = "Tagged", description = "A string-valued map next to declared properties")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
public record Tagged(
    @NotNull 
    @Schema(name = "name", requiredMode = Schema.RequiredMode.REQUIRED)
    @JsonProperty("name")
    String name,

    
    @Schema(name = "count", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
    @JsonProperty("count")
    Integer count,

    
    @Schema(name = "note", requiredMode = Schema.RequiredMode.NOT_REQUIRED, nullable = true)
    @JsonProperty("note")
    @Nullable String note,

    
    @Schema(name = "secret", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
    @JsonProperty("secret")
    @Nullable String secret,

    @JsonAnySetter
    @JsonAnyGetter
    Map<String, String> additionalProperties
) {

  public Tagged {
    if (count == null) {
      count = 3;
    }
    if (additionalProperties == null) {
      additionalProperties = new LinkedHashMap<>();
    }
  }

  /**
   * Return the additional (undeclared) property with the specified name.
   */
  public String getAdditionalProperty(String key) {
    return additionalProperties.get(key);
  }

  @Override
  public String toString() {
    return "Tagged["
        + "name=" + name
        + ", count=" + count
        + ", note=" + note
        + ", secret=" + "*"
        + ", additionalProperties=" + additionalProperties
        + "]";
  }

  /**
   * Builder of Tagged. Components that are not set get their initial value from the constructor.
   * The builder can be used again after {@link #build()}.
   */
  public static class Builder {

    private String name;

    private Integer count;

    private String note;

    private String secret;

    private Map<String, String> additionalProperties;

    public Builder() {
    }

    protected Builder copyOf(Tagged value) {
      this.name = value.name();
      this.count = value.count();
      this.note = value.note();
      this.secret = value.secret();
      this.additionalProperties = value.additionalProperties() == null ? null : new LinkedHashMap<>(value.additionalProperties());
      return this;
    }

    public Tagged.Builder name(String name) {
      this.name = name;
      return this;
    }

    public Tagged.Builder count(Integer count) {
      this.count = count;
      return this;
    }

    public Tagged.Builder note(String note) {
      this.note = note;
      return this;
    }

    public Tagged.Builder secret(String secret) {
      this.secret = secret;
      return this;
    }

    public Tagged.Builder additionalProperties(Map<String, String> additionalProperties) {
      this.additionalProperties = additionalProperties;
      return this;
    }

    /**
     * Returns a new Tagged from the values set so far; the builder stays usable.
     */
    public Tagged build() {
      return new Tagged(this.name, this.count, this.note, this.secret, this.additionalProperties);
    }
  }

  /**
   * Create a builder with no initialized component (the constructor applies the initial values).
   */
  public static Tagged.Builder builder() {
    return new Tagged.Builder();
  }

  /**
   * Create a builder holding every component of this record.
   */
  public Tagged.Builder toBuilder() {
    return new Tagged.Builder().copyOf(this);
  }

}

