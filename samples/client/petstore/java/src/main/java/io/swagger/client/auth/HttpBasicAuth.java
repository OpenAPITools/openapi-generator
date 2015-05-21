package io.swagger.client.auth;

import java.util.Map;

import java.io.UnsupportedEncodingException;
import javax.xml.bind.DatatypeConverter;

public class HttpBasicAuth implements Authentication {
  private String username;
  private String password;

  public String getUsername() {
    return username;
  }

  public void setUsername(String username) {
    this.username = username;
  }

  public String getPassword() {
    return password;
  }

  public void setPassword(String password) {
    this.password = password;
  }

  @Override
  public void processParams(Map<String, String> queryParams, Map<String, String> headerParams) {
    try {
      headerParams.put("Authorization", "Basic " + DatatypeConverter.printBase64Binary((username + ":" + password).getBytes("UTF-8")));
    } catch (UnsupportedEncodingException e) {
      throw new RuntimeException(e);
    }
  }
}
