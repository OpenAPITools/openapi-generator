package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import org.openapitools.client.EncodingUtils;
import org.openapitools.client.model.ApiResponse;


import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import feign.*;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen")
public interface AuthApi extends ApiClient.Api {


  /**
   * To test HTTP basic authentication
   * To test HTTP basic authentication
   * @return String
   */
  @RequestLine("POST /auth/http/basic")
  @Headers({
    "Accept: text/plain",
  })
  String testAuthHttpBasic();

  /**
   * To test HTTP basic authentication
   * Similar to <code>testAuthHttpBasic</code> but it also returns the http response headers .
   * To test HTTP basic authentication
   * @return A ApiResponse that wraps the response boyd and the http headers.
   */
  @RequestLine("POST /auth/http/basic")
  @Headers({
    "Accept: text/plain",
  })
  ApiResponse<String> testAuthHttpBasicWithHttpInfo();


}
