package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import java.util.Arrays;
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
  private JsonNullable<Long> id = JsonNullable.undefined();

  @JsonProperty("username")
  private JsonNullable<String> username = JsonNullable.undefined();

  @JsonProperty("firstName")
  private JsonNullable<String> firstName = JsonNullable.undefined();

  @JsonProperty("lastName")
  private JsonNullable<String> lastName = JsonNullable.undefined();

  @JsonProperty("email")
  private JsonNullable<String> email = JsonNullable.undefined();

  @JsonProperty("password")
  private JsonNullable<String> password = JsonNullable.undefined();

  @JsonProperty("phone")
  private JsonNullable<String> phone = JsonNullable.undefined();

  @JsonProperty("userStatus")
  private JsonNullable<Integer> userStatus = JsonNullable.undefined();

  public User id(Long id) {
    this.id = JsonNullable.of(id);
    return this;
  }

  /**
   * Get id
   * @return id
  */
  
  @Schema(name = "id", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  public JsonNullable<Long> getId() {
    return id;
  }

  public void setId(JsonNullable<Long> id) {
    this.id = id;
  }

  public User username(String username) {
    this.username = JsonNullable.of(username);
    return this;
  }

  /**
   * Get username
   * @return username
  */
  
  @Schema(name = "username", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  public JsonNullable<String> getUsername() {
    return username;
  }

  public void setUsername(JsonNullable<String> username) {
    this.username = username;
  }

  public User firstName(String firstName) {
    this.firstName = JsonNullable.of(firstName);
    return this;
  }

  /**
   * Get firstName
   * @return firstName
  */
  
  @Schema(name = "firstName", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  public JsonNullable<String> getFirstName() {
    return firstName;
  }

  public void setFirstName(JsonNullable<String> firstName) {
    this.firstName = firstName;
  }

  public User lastName(String lastName) {
    this.lastName = JsonNullable.of(lastName);
    return this;
  }

  /**
   * Get lastName
   * @return lastName
  */
  
  @Schema(name = "lastName", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  public JsonNullable<String> getLastName() {
    return lastName;
  }

  public void setLastName(JsonNullable<String> lastName) {
    this.lastName = lastName;
  }

  public User email(String email) {
    this.email = JsonNullable.of(email);
    return this;
  }

  /**
   * Get email
   * @return email
  */
  
  @Schema(name = "email", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  public JsonNullable<String> getEmail() {
    return email;
  }

  public void setEmail(JsonNullable<String> email) {
    this.email = email;
  }

  public User password(String password) {
    this.password = JsonNullable.of(password);
    return this;
  }

  /**
   * Get password
   * @return password
  */
  
  @Schema(name = "password", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  public JsonNullable<String> getPassword() {
    return password;
  }

  public void setPassword(JsonNullable<String> password) {
    this.password = password;
  }

  public User phone(String phone) {
    this.phone = JsonNullable.of(phone);
    return this;
  }

  /**
   * Get phone
   * @return phone
  */
  
  @Schema(name = "phone", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  public JsonNullable<String> getPhone() {
    return phone;
  }

  public void setPhone(JsonNullable<String> phone) {
    this.phone = phone;
  }

  public User userStatus(Integer userStatus) {
    this.userStatus = JsonNullable.of(userStatus);
    return this;
  }

  /**
   * User Status
   * @return userStatus
  */
  
  @Schema(name = "userStatus", description = "User Status", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  public JsonNullable<Integer> getUserStatus() {
    return userStatus;
  }

  public void setUserStatus(JsonNullable<Integer> userStatus) {
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

