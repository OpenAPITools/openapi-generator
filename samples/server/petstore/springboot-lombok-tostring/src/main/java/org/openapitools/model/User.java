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

  public User() {
    super();
  }

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



  
  public static class Builder {

    private User instance;

    public Builder() {
      this(new User());
    }

    protected Builder(User instance) {
      this.instance = instance;
    }

    protected Builder copyOf(User value) { 
      this.instance.setId(value.id);
      this.instance.setUsername(value.username);
      this.instance.setFirstName(value.firstName);
      this.instance.setLastName(value.lastName);
      this.instance.setEmail(value.email);
      this.instance.setPassword(value.password);
      this.instance.setPhone(value.phone);
      this.instance.setUserStatus(value.userStatus);
      return this;
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
  * Create a builder with no initialized field (except for the default values).
  */
  public static User.Builder builder() {
    return new User.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public User.Builder toBuilder() {
    User.Builder builder = new User.Builder();
    return builder.copyOf(this);
  }

}

