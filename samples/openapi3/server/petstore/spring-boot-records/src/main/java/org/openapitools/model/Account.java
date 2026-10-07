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
 * Account
 *
 * @param username Get username
 * @param password Get password
 * @param displayName Get displayName
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
public record Account(
    @JsonInclude(JsonInclude.Include.NON_NULL)
    @NotNull 
    @Schema(name = "username", requiredMode = Schema.RequiredMode.REQUIRED)
    @JsonProperty("username")
    String username,

    @JsonInclude(JsonInclude.Include.NON_NULL)
    @NotNull 
    @Schema(name = "password", requiredMode = Schema.RequiredMode.REQUIRED)
    @JsonProperty("password")
    String password,

    @JsonInclude(JsonInclude.Include.NON_NULL)
    
    @Schema(name = "displayName", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
    @JsonProperty("displayName")
    @Nullable String displayName
) {

  @Override
  public String toString() {
    return "Account["
        + "username=" + username
        + ", password=" + "*"
        + ", displayName=" + displayName
        + "]";
  }

  /**
   * Builder of Account. Components that are not set get their initial value from the constructor.
   * The builder can be used again after {@link #build()}.
   */
  public static class Builder {

    private String username;

    private String password;

    private String displayName;

    public Builder() {
    }

    protected Builder copyOf(Account value) {
      this.username = value.username();
      this.password = value.password();
      this.displayName = value.displayName();
      return this;
    }

    public Account.Builder username(String username) {
      this.username = username;
      return this;
    }

    public Account.Builder password(String password) {
      this.password = password;
      return this;
    }

    public Account.Builder displayName(String displayName) {
      this.displayName = displayName;
      return this;
    }

    /**
     * Returns a new Account from the values set so far; the builder stays usable.
     */
    public Account build() {
      return new Account(this.username, this.password, this.displayName);
    }
  }

  /**
   * Create a builder with no initialized component (the constructor applies the initial values).
   */
  public static Account.Builder builder() {
    return new Account.Builder();
  }

  /**
   * Create a builder holding every component of this record.
   */
  public Account.Builder toBuilder() {
    return new Account.Builder().copyOf(this);
  }

}

