package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
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
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.6.0-SNAPSHOT")
public class User {

  private Long id;

  private String username;

  private String firstName;

  private String lastName;

  private String email;

  private String password;

  private String phone;

  private Integer userStatus;

  public User id(Long id) {
    this.id = id;
    return this;
  }


  public User username(String username) {
    this.username = username;
    return this;
  }


  public User firstName(String firstName) {
    this.firstName = firstName;
    return this;
  }


  public User lastName(String lastName) {
    this.lastName = lastName;
    return this;
  }


  public User email(String email) {
    this.email = email;
    return this;
  }


  public User password(String password) {
    this.password = password;
    return this;
  }


  public User phone(String phone) {
    this.phone = phone;
    return this;
  }


  public User userStatus(Integer userStatus) {
    this.userStatus = userStatus;
    return this;
  }



}

