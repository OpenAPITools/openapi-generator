package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import org.openapitools.client.EncodingUtils;
import org.openapitools.client.model.ApiResponse;

import org.openapitools.client.model.InlineResponseDefault;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import feign.*;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen")
public interface DefaultApi extends ApiClient.Api {


  /**
   * 
   * 
   * @return InlineResponseDefault
   */
  @RequestLine("GET /foo")
  @Headers({
    "Accept: application/json",
  })
  InlineResponseDefault fooGet();

  /**
   * 
   * Similar to <code>fooGet</code> but it also returns the http response headers .
   * 
   * @return A ApiResponse that wraps the response boyd and the http headers.
   */
  @RequestLine("GET /foo")
  @Headers({
    "Accept: application/json",
  })
  ApiResponse<InlineResponseDefault> fooGetWithHttpInfo();


}
