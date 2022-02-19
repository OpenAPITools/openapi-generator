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
 * User
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class User   {

  @JsonProperty("id")
  private Optional<Long> id = Optional.empty();

  @JsonProperty("username")
  private Optional<String> username = Optional.empty();

  @JsonProperty("firstName")
  private Optional<String> firstName = Optional.empty();

  @JsonProperty("lastName")
  private Optional<String> lastName = Optional.empty();

  @JsonProperty("email")
  private Optional<String> email = Optional.empty();

  @JsonProperty("password")
  private Optional<String> password = Optional.empty();

  @JsonProperty("phone")
  private Optional<String> phone = Optional.empty();

  @JsonProperty("userStatus")
  private Optional<Integer> userStatus = Optional.empty();

  public User id(Long id) {
    this.id = Optional.of(id);
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

  public void setId(Optional<Long> id) {
    this.id = id;
  }

  public User username(String username) {
    this.username = Optional.of(username);
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

  public void setUsername(Optional<String> username) {
    this.username = username;
  }

  public User firstName(String firstName) {
    this.firstName = Optional.of(firstName);
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

  public void setFirstName(Optional<String> firstName) {
    this.firstName = firstName;
  }

  public User lastName(String lastName) {
    this.lastName = Optional.of(lastName);
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

  public void setLastName(Optional<String> lastName) {
    this.lastName = lastName;
  }

  public User email(String email) {
    this.email = Optional.of(email);
    return this;
  }

  /**
   * Get email
   * @return email
  */
  @Schema(name = "email", required = false)
  public Optional<String> getEmail() {
    return email;
  }

  public void setEmail(Optional<String> email) {
    this.email = email;
  }

  public User password(String password) {
    this.password = Optional.of(password);
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

  public void setPassword(Optional<String> password) {
    this.password = password;
  }

  public User phone(String phone) {
    this.phone = Optional.of(phone);
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

  public void setPhone(Optional<String> phone) {
    this.phone = phone;
  }

  public User userStatus(Integer userStatus) {
    this.userStatus = Optional.of(userStatus);
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
}

