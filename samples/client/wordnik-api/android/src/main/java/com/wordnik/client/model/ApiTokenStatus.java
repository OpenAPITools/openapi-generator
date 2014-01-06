package com.wordnik.client.model;

import com.fasterxml.jackson.annotation.JsonProperty;

public class ApiTokenStatus {
  @JsonProperty("valid")
  private Boolean valid = null;
  @JsonProperty("token")
  private String token = null;
  @JsonProperty("resetsInMillis")
  private Long resetsInMillis = null;
  @JsonProperty("remainingCalls")
  private Long remainingCalls = null;
  @JsonProperty("expiresInMillis")
  private Long expiresInMillis = null;
  @JsonProperty("totalRequests")
  private Long totalRequests = null;
  public Boolean getValid() {
    return valid;
  }
  public void setValid(Boolean valid) {
    this.valid = valid;
  }

  public String getToken() {
    return token;
  }
  public void setToken(String token) {
    this.token = token;
  }

  public Long getResetsInMillis() {
    return resetsInMillis;
  }
  public void setResetsInMillis(Long resetsInMillis) {
    this.resetsInMillis = resetsInMillis;
  }

  public Long getRemainingCalls() {
    return remainingCalls;
  }
  public void setRemainingCalls(Long remainingCalls) {
    this.remainingCalls = remainingCalls;
  }

  public Long getExpiresInMillis() {
    return expiresInMillis;
  }
  public void setExpiresInMillis(Long expiresInMillis) {
    this.expiresInMillis = expiresInMillis;
  }

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

