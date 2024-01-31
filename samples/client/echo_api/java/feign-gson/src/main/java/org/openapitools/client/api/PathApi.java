package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import org.openapitools.client.EncodingUtils;
import org.openapitools.client.model.ApiResponse;

import org.openapitools.client.model.StringEnumRef;

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
   * @param enumNonrefStringPath  (required)
   * @param enumRefStringPath  (required)
   * @return String
   */
  @RequestLine("GET /path/string/{pathString}/integer/{pathInteger}/{enumNonrefStringPath}/{enumRefStringPath}")
  @Headers({
    "Accept: text/plain",
  })
  String testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath(@Param("pathString") String pathString, @Param("pathInteger") Integer pathInteger, @Param("enumNonrefStringPath") String enumNonrefStringPath, @Param("enumRefStringPath") StringEnumRef enumRefStringPath);

  /**
   * Test path parameter(s)
   * Similar to <code>testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath</code> but it also returns the http response headers .
   * Test path parameter(s)
   * @param pathString  (required)
   * @param pathInteger  (required)
   * @param enumNonrefStringPath  (required)
   * @param enumRefStringPath  (required)
   * @return A ApiResponse that wraps the response boyd and the http headers.
   */
  @RequestLine("GET /path/string/{pathString}/integer/{pathInteger}/{enumNonrefStringPath}/{enumRefStringPath}")
  @Headers({
    "Accept: text/plain",
  })
  ApiResponse<String> testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPathWithHttpInfo(@Param("pathString") String pathString, @Param("pathInteger") Integer pathInteger, @Param("enumNonrefStringPath") String enumNonrefStringPath, @Param("enumRefStringPath") StringEnumRef enumRefStringPath);


}
