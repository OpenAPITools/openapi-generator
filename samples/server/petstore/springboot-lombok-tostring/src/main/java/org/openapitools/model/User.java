package org.openapitools.model;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.annotation.Generated;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import java.net.URI;
import java.time.OffsetDateTime;
import java.util.*;
import java.util.Objects;
import org.hibernate.validator.constraints.*;
import org.springframework.lang.Nullable;

/**
 * A User who is purchasing from the pet store
 */
@lombok.Getter
@lombok.Setter
@lombok.ToString
@lombok.EqualsAndHashCode

@Schema(name = "User", description = "A User who is purchasing from the pet store")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.28.0-SNAPSHOT")
public class User {

  
  @Schema(name = "id", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("id")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable Long id;

  
  @Schema(name = "username", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("username")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable String username;

  
  @Schema(name = "firstName", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("firstName")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable String firstName;

  
  @Schema(name = "lastName", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("lastName")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable String lastName;

  
  @Schema(name = "email", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("email")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable String email;

  
  @Schema(name = "password", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("password")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable String password;

  
  @Schema(name = "phone", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("phone")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable String phone;

  
  @Schema(name = "userStatus", description = "User Status", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("userStatus")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable Integer userStatus;

  public User id(@Nullable Long id) {
    this.id = id;
    return this;
  }


  public User username(@Nullable String username) {
    this.username = username;
    return this;
  }


  public User firstName(@Nullable String firstName) {
    this.firstName = firstName;
    return this;
  }


  public User lastName(@Nullable String lastName) {
    this.lastName = lastName;
    return this;
  }


  public User email(@Nullable String email) {
    this.email = email;
    return this;
  }


  public User password(@Nullable String password) {
    this.password = password;
    return this;
  }


  public User phone(@Nullable String phone) {
    this.phone = phone;
    return this;
  }


  public User userStatus(@Nullable Integer userStatus) {
    this.userStatus = userStatus;
    return this;
  }



}

