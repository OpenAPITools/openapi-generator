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
public interface HeaderApi extends ApiClient.Api {


  /**
   * Test header parameter(s)
   * Test header parameter(s)
   * @param integerHeader  (optional)
   * @param booleanHeader  (optional)
   * @param stringHeader  (optional)
   * @return String
   */
  @RequestLine("GET /header/integer/boolean/string")
  @Headers({
    "Accept: text/plain",
    "integer_header: {integerHeader}",
    
    "boolean_header: {booleanHeader}",
    
    "string_header: {stringHeader}"
  })
  String testHeaderIntegerBooleanString(@Param("integerHeader") Integer integerHeader, @Param("booleanHeader") Boolean booleanHeader, @Param("stringHeader") String stringHeader);

  /**
   * Test header parameter(s)
   * Similar to <code>testHeaderIntegerBooleanString</code> but it also returns the http response headers .
   * Test header parameter(s)
   * @param integerHeader  (optional)
   * @param booleanHeader  (optional)
   * @param stringHeader  (optional)
   * @return A ApiResponse that wraps the response boyd and the http headers.
   */
  @RequestLine("GET /header/integer/boolean/string")
  @Headers({
    "Accept: text/plain",
    "integer_header: {integerHeader}",
    
    "boolean_header: {booleanHeader}",
    
    "string_header: {stringHeader}"
  })
  ApiResponse<String> testHeaderIntegerBooleanStringWithHttpInfo(@Param("integerHeader") Integer integerHeader, @Param("booleanHeader") Boolean booleanHeader, @Param("stringHeader") String stringHeader);


}
