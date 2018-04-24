package org.openapitools.client.auth;

import feign.RequestInterceptor;
import feign.RequestTemplate;
import feign.auth.BasicAuthRequestInterceptor;

/**
 * An interceptor that adds the request header needed to use HTTP basic authentication.
 */
public class HttpBasicAuth implements RequestInterceptor {

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

    public void setCredentials(String username, String password) {
        this.username = username;
        this.password = password;
    }

  @Override
  public void apply(RequestTemplate template) {
      RequestInterceptor requestInterceptor = new BasicAuthRequestInterceptor(username, password);
      requestInterceptor.apply(template);
  }
}
