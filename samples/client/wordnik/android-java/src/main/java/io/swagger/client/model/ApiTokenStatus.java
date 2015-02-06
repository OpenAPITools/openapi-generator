package io.swagger.client.model;


import com.wordnik.swagger.annotations.*;


@ApiModel(description = "")
public class ApiTokenStatus  { 
  private Boolean valid = null;
  private String token = null;
  private Long resetsInMillis = null;
  
  //public enum resetsInMillisEnum {  }; 
  
  private Long remainingCalls = null;
  
  //public enum remainingCallsEnum {  }; 
  
  private Long expiresInMillis = null;
  
  //public enum expiresInMillisEnum {  }; 
  
  private Long totalRequests = null;
  
  //public enum totalRequestsEnum {  }; 
  
  
  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  public Boolean getValid() {
    return valid;
  }
  public void setValid(Boolean valid) {
    this.valid = valid;
  }

  
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
  public Long getResetsInMillis() {
    return resetsInMillis;
  }
  public void setResetsInMillis(Long resetsInMillis) {
    this.resetsInMillis = resetsInMillis;
  }

  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  public Long getRemainingCalls() {
    return remainingCalls;
  }
  public void setRemainingCalls(Long remainingCalls) {
    this.remainingCalls = remainingCalls;
  }

  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  public Long getExpiresInMillis() {
    return expiresInMillis;
  }
  public void setExpiresInMillis(Long expiresInMillis) {
    this.expiresInMillis = expiresInMillis;
  }

  
  /**
   **/
  @ApiModelProperty(required = false, value = "")
  public Long getTotalRequests() {
    return totalRequests;
  }
  public void setTotalRequests(Long totalRequests) {
    this.totalRequests = totalRequests;
  }

  

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class ApiTokenStatus {\n");
    
    sb.append("  valid: ").append(valid).append("\n");
    sb.append("  token: ").append(token).append("\n");
    sb.append("  resetsInMillis: ").append(resetsInMillis).append("\n");
    sb.append("  remainingCalls: ").append(remainingCalls).append("\n");
    sb.append("  expiresInMillis: ").append(expiresInMillis).append("\n");
    sb.append("  totalRequests: ").append(totalRequests).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
