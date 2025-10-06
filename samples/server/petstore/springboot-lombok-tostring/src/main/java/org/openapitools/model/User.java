package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import org.hibernate.validator.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * A User who is purchasing from the pet store
 */
@lombok.Getter
@lombok.Setter
@lombok.ToString
@lombok.EqualsAndHashCode

@Schema(name = "User", description = "A User who is purchasing from the pet store")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")
public class User {

  private @Nullable Long id;

  private @Nullable String username;

  private @Nullable String firstName;

  private @Nullable String lastName;

  private @Nullable String email;

  private @Nullable String password;

  private @Nullable String phone;

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

