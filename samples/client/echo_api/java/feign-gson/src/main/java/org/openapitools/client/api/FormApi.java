package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import org.openapitools.client.EncodingUtils;
import org.openapitools.client.model.ApiResponse;

import org.openapitools.client.model.TestFormObjectMultipartRequestMarker;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import feign.*;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")
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
  String testFormIntegerBooleanString(@Param("integer_form") @javax.annotation.Nullable Integer integerForm, @Param("boolean_form") @javax.annotation.Nullable Boolean booleanForm, @Param("string_form") @javax.annotation.Nullable String stringForm);

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
  ApiResponse<String> testFormIntegerBooleanStringWithHttpInfo(@Param("integer_form") @javax.annotation.Nullable Integer integerForm, @Param("boolean_form") @javax.annotation.Nullable Boolean booleanForm, @Param("string_form") @javax.annotation.Nullable String stringForm);



  /**
   * Test form parameter(s) for multipart schema
   * Test form parameter(s) for multipart schema
   * @param marker  (required)
   * @return String
   */
  @RequestLine("POST /form/object/multipart")
  @Headers({
    "Content-Type: multipart/form-data",
    "Accept: text/plain",
  })
  String testFormObjectMultipart(@Param("marker") @javax.annotation.Nonnull TestFormObjectMultipartRequestMarker marker);

  /**
   * Test form parameter(s) for multipart schema
   * Similar to <code>testFormObjectMultipart</code> but it also returns the http response headers .
   * Test form parameter(s) for multipart schema
   * @param marker  (required)
   * @return A ApiResponse that wraps the response boyd and the http headers.
   */
  @RequestLine("POST /form/object/multipart")
  @Headers({
    "Content-Type: multipart/form-data",
    "Accept: text/plain",
  })
  ApiResponse<String> testFormObjectMultipartWithHttpInfo(@Param("marker") @javax.annotation.Nonnull TestFormObjectMultipartRequestMarker marker);



  /**
   * Test form parameter(s) for oneOf schema
   * Test form parameter(s) for oneOf schema
   * @param form1  (optional)
   * @param form2  (optional)
   * @param form3  (optional)
   * @param form4  (optional)
   * @param id  (optional)
   * @param name  (optional)
   * @return String
   */
  @RequestLine("POST /form/oneof")
  @Headers({
    "Content-Type: application/x-www-form-urlencoded",
    "Accept: text/plain",
  })
  String testFormOneof(@Param("form1") @javax.annotation.Nullable String form1, @Param("form2") @javax.annotation.Nullable Integer form2, @Param("form3") @javax.annotation.Nullable String form3, @Param("form4") @javax.annotation.Nullable Boolean form4, @Param("id") @javax.annotation.Nullable Long id, @Param("name") @javax.annotation.Nullable String name);

  /**
   * Test form parameter(s) for oneOf schema
   * Similar to <code>testFormOneof</code> but it also returns the http response headers .
   * Test form parameter(s) for oneOf schema
   * @param form1  (optional)
   * @param form2  (optional)
   * @param form3  (optional)
   * @param form4  (optional)
   * @param id  (optional)
   * @param name  (optional)
   * @return A ApiResponse that wraps the response boyd and the http headers.
   */
  @RequestLine("POST /form/oneof")
  @Headers({
    "Content-Type: application/x-www-form-urlencoded",
    "Accept: text/plain",
  })
  ApiResponse<String> testFormOneofWithHttpInfo(@Param("form1") @javax.annotation.Nullable String form1, @Param("form2") @javax.annotation.Nullable Integer form2, @Param("form3") @javax.annotation.Nullable String form3, @Param("form4") @javax.annotation.Nullable Boolean form4, @Param("id") @javax.annotation.Nullable Long id, @Param("name") @javax.annotation.Nullable String name);


}
