package io.swagger.client.model;


import com.wordnik.swagger.annotations.*;


@ApiModel(description = "")
public class AuthenticationToken  { 
  private String token = null;
  private Long userId = null;
  
  //public enum userIdEnum {  }; 
  
  private String userSignature = null;
  
  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  public String getToken() {
    return token;
  }
  public void setToken(String token) {
    this.token = token;
  }

  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  public Long getUserId() {
    return userId;
  }
  public void setUserId(Long userId) {
    this.userId = userId;
  }

  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  public String getUserSignature() {
    return userSignature;
  }
  public void setUserSignature(String userSignature) {
    this.userSignature = userSignature;
  }

  

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class AuthenticationToken {\n");
    
    sb.append("  token: ").append(token).append("\n");
    sb.append("  userId: ").append(userId).append("\n");
    sb.append("  userSignature: ").append(userSignature).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
