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
public interface PathApi extends ApiClient.Api {


  /**
   * Test path parameter(s)
   * Test path parameter(s)
   * @param pathString  (required)
   * @param pathInteger  (required)
   * @return String
   */
  @RequestLine("GET /path/string/{pathString}/integer/{pathInteger}")
  @Headers({
    "Accept: text/plain",
  })
  String testsPathStringPathStringIntegerPathInteger(@Param("pathString") String pathString, @Param("pathInteger") Integer pathInteger);

  /**
   * Test path parameter(s)
   * Similar to <code>testsPathStringPathStringIntegerPathInteger</code> but it also returns the http response headers .
   * Test path parameter(s)
   * @param pathString  (required)
   * @param pathInteger  (required)
   * @return A ApiResponse that wraps the response boyd and the http headers.
   */
  @RequestLine("GET /path/string/{pathString}/integer/{pathInteger}")
  @Headers({
    "Accept: text/plain",
  })
  ApiResponse<String> testsPathStringPathStringIntegerPathIntegerWithHttpInfo(@Param("pathString") String pathString, @Param("pathInteger") Integer pathInteger);


}
