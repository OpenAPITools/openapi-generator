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
public interface FormApi extends ApiClient.Api {


  /**
   * Test form parameter(s)
   * Test form parameter(s)
   * @param integerForm  (optional)
   * @param booleanForm  (optional)
   * @param stringForm  (optional)
   * @return String
   */
  @RequestLine("POST /form/integer/boolean/string")
  @Headers({
    "Content-Type: application/x-www-form-urlencoded",
    "Accept: text/plain",
  })
  String testFormIntegerBooleanString(@Param("integer_form") Integer integerForm, @Param("boolean_form") Boolean booleanForm, @Param("string_form") String stringForm);

  /**
   * Test form parameter(s)
   * Similar to <code>testFormIntegerBooleanString</code> but it also returns the http response headers .
   * Test form parameter(s)
   * @param integerForm  (optional)
   * @param booleanForm  (optional)
   * @param stringForm  (optional)
   * @return A ApiResponse that wraps the response boyd and the http headers.
   */
  @RequestLine("POST /form/integer/boolean/string")
  @Headers({
    "Content-Type: application/x-www-form-urlencoded",
    "Accept: text/plain",
  })
  ApiResponse<String> testFormIntegerBooleanStringWithHttpInfo(@Param("integer_form") Integer integerForm, @Param("boolean_form") Boolean booleanForm, @Param("string_form") String stringForm);


}
