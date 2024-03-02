package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import javax.annotation.Generated;

/**
 * A User who is purchasing from the pet store
 */
@lombok.Data
@lombok.Builder
@lombok.NoArgsConstructor
@lombok.AllArgsConstructor

@Schema(name = "User", description = "A User who is purchasing from the pet store")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class User {

  private Long id;

  private String username;

  private String firstName;

  private String lastName;

  private String email;

  private String password;

  private String phone;

  private Integer userStatus;

  
  public static class Builder {

    private User instance;

    public Builder() {
      this(new User());
    }

    protected Builder(User instance) {
      this.instance = instance;
    }

    public User.Builder id(Long id) {
      this.instance.id(id);
      return this;
    }
    public User.Builder username(String username) {
      this.instance.username(username);
      return this;
    }
    public User.Builder firstName(String firstName) {
      this.instance.firstName(firstName);
      return this;
    }
    public User.Builder lastName(String lastName) {
      this.instance.lastName(lastName);
      return this;
    }
    public User.Builder email(String email) {
      this.instance.email(email);
      return this;
    }
    public User.Builder password(String password) {
      this.instance.password(password);
      return this;
    }
    public User.Builder phone(String phone) {
      this.instance.phone(phone);
      return this;
    }
    public User.Builder userStatus(Integer userStatus) {
      this.instance.userStatus(userStatus);
      return this;
    }
    /**
    * returns a built User instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public User build() {
      try {
        return this.instance;
      } finally {
        // ensure that this.instance is not reused
        this.instance = null;
      }
  }

    @Override
    public String toString() {
      return getClass() + "=(" + instance + ")";
    }
  }

  /**
  * Create a builder with no initialized field.
  */
  public static User.Builder builder() {
    return new User.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public User.Builder toBuilder() {
    User.Builder builder = new User.Builder();
    builder.instance.setId(id);
    builder.instance.setUsername(username);
    builder.instance.setFirstName(firstName);
    builder.instance.setLastName(lastName);
    builder.instance.setEmail(email);
    builder.instance.setPassword(password);
    builder.instance.setPhone(phone);
    builder.instance.setUserStatus(userStatus);
    return builder;
  }

}

