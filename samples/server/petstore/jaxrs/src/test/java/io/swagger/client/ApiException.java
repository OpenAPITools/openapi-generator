package io.swagger.client;

import java.util.Map;
import java.util.List;

@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaClientCodegen", date = "2015-08-23T11:08:48.917-07:00")
public class ApiException extends Exception {
  private int code = 0;
  private String message = null;
  private Map<String, List<String>> responseHeaders = null;
  private String responseBody = null;

  public ApiException() {}

  public ApiException(int code, String message) {
    this.code = code;
    this.message = message;
  }

  public ApiException(int code, String message, Map<String, List<String>> responseHeaders, String responseBody) {
    this.code = code;
    this.message = message;
    this.responseHeaders = responseHeaders;
    this.responseBody = responseBody;
  }

  public int getCode() {
    return code;
  }

  public String getMessage() {
    return message;
  }

  /**
   * Get the HTTP response headers.
   */
  public Map<String, List<String>> getResponseHeaders() {
    return responseHeaders;
  }

  /**
   * Get the HTTP response body.
   */
  public String getResponseBody() {
    return responseBody;
  }
}
