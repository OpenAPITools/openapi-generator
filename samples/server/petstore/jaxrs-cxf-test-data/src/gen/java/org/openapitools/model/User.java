package org.openapitools.model;

import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.ApiModelProperty;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;


public class User  {
  
  @ApiModelProperty(value = "")
  private Long id;

  @ApiModelProperty(value = "")
  private String username;

  @ApiModelProperty(value = "")
  private String firstName;

  @ApiModelProperty(value = "")
  private String lastName;

  @ApiModelProperty(value = "")
  private String email;

  @ApiModelProperty(value = "")
  private String password;

  @ApiModelProperty(value = "")
  private String phone;

 /**
  * User Status
  */
  @ApiModelProperty(value = "User Status")
  private Integer userStatus;
 /**
  * Get id
  * @return id
  */
  @JsonProperty("id")
  public Long getId() {
    return id;
  }

  /**
   * Sets the <code>id</code> property.
   */
 public void setId(Long id) {
    this.id = id;
  }

  /**
   * Sets the <code>id</code> property.
   */
  public User id(Long id) {
    this.id = id;
    return this;
  }

 /**
  * Get username
  * @return username
  */
  @JsonProperty("username")
  public String getUsername() {
    return username;
  }

  /**
   * Sets the <code>username</code> property.
   */
 public void setUsername(String username) {
    this.username = username;
  }

  /**
   * Sets the <code>username</code> property.
   */
  public User username(String username) {
    this.username = username;
    return this;
  }

 /**
  * Get firstName
  * @return firstName
  */
  @JsonProperty("firstName")
  public String getFirstName() {
    return firstName;
  }

  /**
   * Sets the <code>firstName</code> property.
   */
 public void setFirstName(String firstName) {
    this.firstName = firstName;
  }

  /**
   * Sets the <code>firstName</code> property.
   */
  public User firstName(String firstName) {
    this.firstName = firstName;
    return this;
  }

 /**
  * Get lastName
  * @return lastName
  */
  @JsonProperty("lastName")
  public String getLastName() {
    return lastName;
  }

  /**
   * Sets the <code>lastName</code> property.
   */
 public void setLastName(String lastName) {
    this.lastName = lastName;
  }

  /**
   * Sets the <code>lastName</code> property.
   */
  public User lastName(String lastName) {
    this.lastName = lastName;
    return this;
  }

 /**
  * Get email
  * @return email
  */
  @JsonProperty("email")
  public String getEmail() {
    return email;
  }

  /**
   * Sets the <code>email</code> property.
   */
 public void setEmail(String email) {
    this.email = email;
  }

  /**
   * Sets the <code>email</code> property.
   */
  public User email(String email) {
    this.email = email;
    return this;
  }

 /**
  * Get password
  * @return password
  */
  @JsonProperty("password")
  public String getPassword() {
    return password;
  }

  /**
   * Sets the <code>password</code> property.
   */
 public void setPassword(String password) {
    this.password = password;
  }

  /**
   * Sets the <code>password</code> property.
   */
  public User password(String password) {
    this.password = password;
    return this;
  }

 /**
  * Get phone
  * @return phone
  */
  @JsonProperty("phone")
  public String getPhone() {
    return phone;
  }

  /**
   * Sets the <code>phone</code> property.
   */
 public void setPhone(String phone) {
    this.phone = phone;
  }

  /**
   * Sets the <code>phone</code> property.
   */
  public User phone(String phone) {
    this.phone = phone;
    return this;
  }

 /**
  * User Status
  * @return userStatus
  */
  @JsonProperty("userStatus")
  public Integer getUserStatus() {
    return userStatus;
  }

  /**
   * Sets the <code>userStatus</code> property.
   */
 public void setUserStatus(Integer userStatus) {
    this.userStatus = userStatus;
  }

  /**
   * Sets the <code>userStatus</code> property.
   */
  public User userStatus(Integer userStatus) {
    this.userStatus = userStatus;
    return this;
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
  private static String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

