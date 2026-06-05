package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * A User who is purchasing from the pet store
 */
@lombok.Data
@lombok.Builder
@lombok.NoArgsConstructor
@lombok.AllArgsConstructor

@Schema(name = "User", description = "A User who is purchasing from the pet store")
@JsonPropertyOrder({
    User.JSON_PROPERTY_ID,
    User.JSON_PROPERTY_USERNAME,
    User.JSON_PROPERTY_FIRST_NAME,
    User.JSON_PROPERTY_LAST_NAME,
    User.JSON_PROPERTY_EMAIL,
    User.JSON_PROPERTY_PASSWORD,
    User.JSON_PROPERTY_PHONE,
    User.JSON_PROPERTY_USER_STATUS
})
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.23.0-SNAPSHOT")
public class User {

    public static final String JSON_PROPERTY_ID = "id";
  
  @Schema(name = "id", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("id")

  private @Nullable Long id;

    public static final String JSON_PROPERTY_USERNAME = "username";
  
  @Schema(name = "username", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("username")

  private @Nullable String username;

    public static final String JSON_PROPERTY_FIRST_NAME = "firstName";
  
  @Schema(name = "firstName", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("firstName")

  private @Nullable String firstName;

    public static final String JSON_PROPERTY_LAST_NAME = "lastName";
  
  @Schema(name = "lastName", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("lastName")

  private @Nullable String lastName;

    public static final String JSON_PROPERTY_EMAIL = "email";
  
  @Schema(name = "email", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("email")

  private @Nullable String email;

    public static final String JSON_PROPERTY_PASSWORD = "password";
  
  @Schema(name = "password", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("password")

  private @Nullable String password;

    public static final String JSON_PROPERTY_PHONE = "phone";
  
  @Schema(name = "phone", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("phone")

  private @Nullable String phone;

    public static final String JSON_PROPERTY_USER_STATUS = "userStatus";
  
  @Schema(name = "userStatus", description = "User Status", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("userStatus")

  private @Nullable Integer userStatus;

}

