package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * User
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")
public class User {

  private Optional<Long> id = Optional.empty();

  private Optional<String> username = Optional.empty();

  private Optional<String> firstName = Optional.empty();

  private Optional<String> lastName = Optional.empty();

  private Optional<String> email = Optional.empty();

  private Optional<String> password = Optional.empty();

  private Optional<String> phone = Optional.empty();

  private Optional<Integer> userStatus = Optional.empty();

  public User id(Long id) {
    this.id = Optional.ofNullable(id);
    return this;
  }

  /**
   * Get id
   * @return id
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("id")
  public Optional<Long> getId() {
    return id;
  }

  public void setId(Optional<Long> id) {
    this.id = id;
  }

  public User username(String username) {
    this.username = Optional.ofNullable(username);
    return this;
  }

  /**
   * Get username
   * @return username
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("username")
  public Optional<String> getUsername() {
    return username;
  }

  public void setUsername(Optional<String> username) {
    this.username = username;
  }

  public User firstName(String firstName) {
    this.firstName = Optional.ofNullable(firstName);
    return this;
  }

  /**
   * Get firstName
   * @return firstName
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("firstName")
  public Optional<String> getFirstName() {
    return firstName;
  }

  public void setFirstName(Optional<String> firstName) {
    this.firstName = firstName;
  }

  public User lastName(String lastName) {
    this.lastName = Optional.ofNullable(lastName);
    return this;
  }

  /**
   * Get lastName
   * @return lastName
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("lastName")
  public Optional<String> getLastName() {
    return lastName;
  }

  public void setLastName(Optional<String> lastName) {
    this.lastName = lastName;
  }

  public User email(String email) {
    this.email = Optional.ofNullable(email);
    return this;
  }

  /**
   * Get email
   * @return email
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("email")
  public Optional<String> getEmail() {
    return email;
  }

  public void setEmail(Optional<String> email) {
    this.email = email;
  }

  public User password(String password) {
    this.password = Optional.ofNullable(password);
    return this;
  }

  /**
   * Get password
   * @return password
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("password")
  public Optional<String> getPassword() {
    return password;
  }

  public void setPassword(Optional<String> password) {
    this.password = password;
  }

  public User phone(String phone) {
    this.phone = Optional.ofNullable(phone);
    return this;
  }

  /**
   * Get phone
   * @return phone
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("phone")
  public Optional<String> getPhone() {
    return phone;
  }

  public void setPhone(Optional<String> phone) {
    this.phone = phone;
  }

  public User userStatus(Integer userStatus) {
    this.userStatus = Optional.ofNullable(userStatus);
    return this;
  }

  /**
   * User Status
   * @return userStatus
   */
  
  @ApiModelProperty(value = "User Status")
  @JsonProperty("userStatus")
  public Optional<Integer> getUserStatus() {
    return userStatus;
  }

  public void setUserStatus(Optional<Integer> userStatus) {
    this.userStatus = userStatus;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    User user = (User) o;
    return Objects.equals(this.id, user.id) &&
        Objects.equals(this.username, user.username) &&
        Objects.equals(this.firstName, user.firstName) &&
        Objects.equals(this.lastName, user.lastName) &&
        Objects.equals(this.email, user.email) &&
        Objects.equals(this.password, user.password) &&
        Objects.equals(this.phone, user.phone) &&
        Objects.equals(this.userStatus, user.userStatus);
  }

  @Override
  public int hashCode() {
    return Objects.hash(id, username, firstName, lastName, email, password, phone, userStatus);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class User {\n");
    sb.append("    id: ").append(toIndentedString(id)).append("\n");
    sb.append("    username: ").append(toIndentedString(username)).append("\n");
    sb.append("    firstName: ").append(toIndentedString(firstName)).append("\n");
    sb.append("    lastName: ").append(toIndentedString(lastName)).append("\n");
    sb.append("    email: ").append(toIndentedString(email)).append("\n");
    sb.append("    password: ").append(toIndentedString(password)).append("\n");
    sb.append("    phone: ").append(toIndentedString(phone)).append("\n");
    sb.append("    userStatus: ").append(toIndentedString(userStatus)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
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

