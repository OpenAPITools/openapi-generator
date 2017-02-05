package io.swagger.model;


import io.swagger.annotations.ApiModelProperty;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;

public class User  {
  
  @ApiModelProperty(example = "null", value = "")
  private Long id = null;
  @ApiModelProperty(example = "null", value = "")
  private String username = null;
  @ApiModelProperty(example = "null", value = "")
  private String firstName = null;
  @ApiModelProperty(example = "null", value = "")
  private String lastName = null;
  @ApiModelProperty(example = "null", value = "")
  private String email = null;
  @ApiModelProperty(example = "null", value = "")
  private String password = null;
  @ApiModelProperty(example = "null", value = "")
  private String phone = null;
  @ApiModelProperty(example = "null", value = "User Status")
  private Integer userStatus = null;

 /**
   * Get id
   * @return id
  **/
  public Long getId() {
    return id;
  }

  public void setId(Long id) {
    this.id = id;
  }

  public User id(Long id) {
    this.id = id;
    return this;
  }

 /**
   * Get username
   * @return username
  **/
  public String getUsername() {
    return username;
  }

  public void setUsername(String username) {
    this.username = username;
  }

  public User username(String username) {
    this.username = username;
    return this;
  }

 /**
   * Get firstName
   * @return firstName
  **/
  public String getFirstName() {
    return firstName;
  }

  public void setFirstName(String firstName) {
    this.firstName = firstName;
  }

  public User firstName(String firstName) {
    this.firstName = firstName;
    return this;
  }

 /**
   * Get lastName
   * @return lastName
  **/
  public String getLastName() {
    return lastName;
  }

  public void setLastName(String lastName) {
    this.lastName = lastName;
  }

  public User lastName(String lastName) {
    this.lastName = lastName;
    return this;
  }

 /**
   * Get email
   * @return email
  **/
  public String getEmail() {
    return email;
  }

  public void setEmail(String email) {
    this.email = email;
  }

  public User email(String email) {
    this.email = email;
    return this;
  }

 /**
   * Get password
   * @return password
  **/
  public String getPassword() {
    return password;
  }

  public void setPassword(String password) {
    this.password = password;
  }

  public User password(String password) {
    this.password = password;
    return this;
  }

 /**
   * Get phone
   * @return phone
  **/
  public String getPhone() {
    return phone;
  }

  public void setPhone(String phone) {
    this.phone = phone;
  }

  public User phone(String phone) {
    this.phone = phone;
    return this;
  }

 /**
   * User Status
   * @return userStatus
  **/
  public Integer getUserStatus() {
    return userStatus;
  }

  public void setUserStatus(Integer userStatus) {
    this.userStatus = userStatus;
  }

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

