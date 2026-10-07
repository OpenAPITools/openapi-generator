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
 * Owner
 *
 * @param id Get id
 * @param email Get email
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
public record Owner(
    @JsonInclude(JsonInclude.Include.NON_NULL)
    @NotNull 
    @Schema(name = "id", requiredMode = Schema.RequiredMode.REQUIRED)
    @JsonProperty("id")
    Long id,

    @JsonInclude(JsonInclude.Include.NON_NULL)
    @jakarta.validation.constraints.Email 
    @Schema(name = "email", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
    @JsonProperty("email")
    @Nullable String email
) {

  /**
   * Builder of Owner. Components that are not set get their initial value from the constructor.
   * The builder can be used again after {@link #build()}.
   */
  public static class Builder {

    private Long id;

    private String email;

    public Builder() {
    }

    protected Builder copyOf(Owner value) {
      this.id = value.id();
      this.email = value.email();
      return this;
    }

    public Owner.Builder id(Long id) {
      this.id = id;
      return this;
    }

    public Owner.Builder email(String email) {
      this.email = email;
      return this;
    }

    /**
     * Returns a new Owner from the values set so far; the builder stays usable.
     */
    public Owner build() {
      return new Owner(this.id, this.email);
    }
  }

  /**
   * Create a builder with no initialized component (the constructor applies the initial values).
   */
  public static Owner.Builder builder() {
    return new Owner.Builder();
  }

  /**
   * Create a builder holding every component of this record.
   */
  public Owner.Builder toBuilder() {
    return new Owner.Builder().copyOf(this);
  }

}

