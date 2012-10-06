package com.wordnik.client.model;

public class ApiTokenStatus {
  private Boolean valid = null;
  private String token = null;
  private Long resetsInMillis = null;
  private Long remainingCalls = null;
  private Long expiresInMillis = null;
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

