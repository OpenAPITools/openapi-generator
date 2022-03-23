package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import java.util.Arrays;
import java.util.Optional;
import org.openapitools.jackson.nullable.JsonNullable;
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

@Schema(name = "User", description = "A User who is purchasing from the pet store")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class User {

  @JsonProperty("id")
  private Optional<Long> id = Optional.empty();

  @JsonProperty("username")
  private Optional<String> username = Optional.empty();

  @JsonProperty("firstName")
  private Optional<String> firstName = Optional.empty();

  @JsonProperty("lastName")
  private Optional<String> lastName = Optional.empty();

  @JsonProperty("email")
  private JsonNullable<String> email = JsonNullable.undefined();

  @JsonProperty("password")
  private Optional<String> password = Optional.empty();

  @JsonProperty("phone")
  private Optional<String> phone = Optional.empty();

  @JsonProperty("userStatus")
  private Optional<Integer> userStatus = Optional.empty();

  public User id(Long id) {
    this.id = Optional.ofNullable(id);
    return this;
  }

  /**
   * Get id
   * @return id
  */
  @Schema(name = "id", required = false)
  public Optional<Long> getId() {
    return id;
  }

  @JsonIgnore
  public void setId(Long id) {
    this.id = Optional.ofNullable(id);
  }

  public User username(String username) {
    this.username = Optional.ofNullable(username);
    return this;
  }

  /**
   * Get username
   * @return username
  */
  @Schema(name = "username", required = false)
  public Optional<String> getUsername() {
    return username;
  }

  @JsonIgnore
  public void setUsername(String username) {
    this.username = Optional.ofNullable(username);
  }

  public User firstName(String firstName) {
    this.firstName = Optional.ofNullable(firstName);
    return this;
  }

  /**
   * Get firstName
   * @return firstName
  */
  @Schema(name = "firstName", required = false)
  public Optional<String> getFirstName() {
    return firstName;
  }

  @JsonIgnore
  public void setFirstName(String firstName) {
    this.firstName = Optional.ofNullable(firstName);
  }

  public User lastName(String lastName) {
    this.lastName = Optional.ofNullable(lastName);
    return this;
  }

  /**
   * Get lastName
   * @return lastName
  */
  @Schema(name = "lastName", required = false)
  public Optional<String> getLastName() {
    return lastName;
  }

  @JsonIgnore
  public void setLastName(String lastName) {
    this.lastName = Optional.ofNullable(lastName);
  }

  public User email(String email) {
    this.email = JsonNullable.of(email);
    return this;
  }

  /**
   * Get email
   * @return email
  */
  @Schema(name = "email", required = false)
  public JsonNullable<String> getEmail() {
    return email;
  }

  public void setEmail(JsonNullable<String> email) {
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
  @Schema(name = "password", required = false)
  public Optional<String> getPassword() {
    return password;
  }

  @JsonIgnore
  public void setPassword(String password) {
    this.password = Optional.ofNullable(password);
  }

  public User phone(String phone) {
    this.phone = Optional.ofNullable(phone);
    return this;
  }

  /**
   * Get phone
   * @return phone
  */
  @Schema(name = "phone", required = false)
  public Optional<String> getPhone() {
    return phone;
  }

  @JsonIgnore
  public void setPhone(String phone) {
    this.phone = Optional.ofNullable(phone);
  }

  public User userStatus(Integer userStatus) {
    this.userStatus = Optional.ofNullable(userStatus);
    return this;
  }

  /**
   * User Status
   * @return userStatus
  */
  @Schema(name = "userStatus", description = "User Status", required = false)
  public Optional<Integer> getUserStatus() {
    return userStatus;
  }

  @JsonIgnore
  public void setUserStatus(Integer userStatus) {
    this.userStatus = Optional.ofNullable(userStatus);
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

  private static <T> boolean equalsNullable(JsonNullable<T> a, JsonNullable<T> b) {
    return a == b || (a != null && b != null && a.isPresent() && b.isPresent() && Objects.deepEquals(a.get(), b.get()));
  }

  @Override
  public int hashCode() {
    return Objects.hash(id, username, firstName, lastName, email, password, phone, userStatus);
  }

  private static <T> int hashCodeNullable(JsonNullable<T> a) {
    if (a == null) {
      return 1;
    }
    return a.isPresent() ? Arrays.deepHashCode(new Object[]{a.get()}) : 31;
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
}

