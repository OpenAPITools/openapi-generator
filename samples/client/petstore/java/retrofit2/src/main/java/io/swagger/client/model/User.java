package io.swagger.client.model;

import io.swagger.client.StringUtil;

import com.google.gson.annotations.SerializedName;



import io.swagger.annotations.*;



@ApiModel(description = "")
public class User   {
  
  @SerializedName("id")
  private Long id = null;
  
  @SerializedName("username")
  private String username = null;
  
  @SerializedName("firstName")
  private String firstName = null;
  
  @SerializedName("lastName")
  private String lastName = null;
  
  @SerializedName("email")
  private String email = null;
  
  @SerializedName("password")
  private String password = null;
  
  @SerializedName("phone")
  private String phone = null;
  
  @SerializedName("userStatus")
  private Integer userStatus = null;
  

  
  /**
   **/
  @ApiModelProperty(value = "")
  public Long getId() {
    return id;
  }
  public void setId(Long id) {
    this.id = id;
  }

  
  /**
   **/
  @ApiModelProperty(value = "")
  public String getUsername() {
    return username;
  }
  public void setUsername(String username) {
    this.username = username;
  }

  
  /**
   **/
  @ApiModelProperty(value = "")
  public String getFirstName() {
    return firstName;
  }
  public void setFirstName(String firstName) {
    this.firstName = firstName;
  }

  
  /**
   **/
  @ApiModelProperty(value = "")
  public String getLastName() {
    return lastName;
  }
  public void setLastName(String lastName) {
    this.lastName = lastName;
  }

  
  /**
   **/
  @ApiModelProperty(value = "")
  public String getEmail() {
    return email;
  }
  public void setEmail(String email) {
    this.email = email;
  }

  
  /**
   **/
  @ApiModelProperty(value = "")
  public String getPassword() {
    return password;
  }
  public void setPassword(String password) {
    this.password = password;
  }

  
  /**
   **/
  @ApiModelProperty(value = "")
  public String getPhone() {
    return phone;
  }
  public void setPhone(String phone) {
    this.phone = phone;
  }

  
  /**
   * User Status
   **/
  @ApiModelProperty(value = "User Status")
  public Integer getUserStatus() {
    return userStatus;
  }
  public void setUserStatus(Integer userStatus) {
    this.userStatus = userStatus;
  }

  

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class User {\n");
    
    sb.append("    id: ").append(StringUtil.toIndentedString(id)).append("\n");
    sb.append("    username: ").append(StringUtil.toIndentedString(username)).append("\n");
    sb.append("    firstName: ").append(StringUtil.toIndentedString(firstName)).append("\n");
    sb.append("    lastName: ").append(StringUtil.toIndentedString(lastName)).append("\n");
    sb.append("    email: ").append(StringUtil.toIndentedString(email)).append("\n");
    sb.append("    password: ").append(StringUtil.toIndentedString(password)).append("\n");
    sb.append("    phone: ").append(StringUtil.toIndentedString(phone)).append("\n");
    sb.append("    userStatus: ").append(StringUtil.toIndentedString(userStatus)).append("\n");
    sb.append("}");
    return sb.toString();
  }
}
