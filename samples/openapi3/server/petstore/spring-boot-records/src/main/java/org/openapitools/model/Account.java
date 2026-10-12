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
}

