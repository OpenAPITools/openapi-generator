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
public interface HeaderApi extends ApiClient.Api {


  /**
   * Test header parameter(s)
   * Test header parameter(s)
   * @param integerHeader  (optional)
   * @param booleanHeader  (optional)
   * @param stringHeader  (optional)
   * @param enumNonrefStringHeader  (optional)
   * @param enumRefStringHeader  (optional)
   * @return String
   */
  @RequestLine("GET /header/integer/boolean/string/enums")
  @Headers({
    "Accept: text/plain",
    "integer_header: {integerHeader}",
    
    "boolean_header: {booleanHeader}",
    
    "string_header: {stringHeader}",
    
    "enum_nonref_string_header: {enumNonrefStringHeader}",
    
    "enum_ref_string_header: {enumRefStringHeader}"
  })
  String testHeaderIntegerBooleanStringEnums(@Param("integerHeader") Integer integerHeader, @Param("booleanHeader") Boolean booleanHeader, @Param("stringHeader") String stringHeader, @Param("enumNonrefStringHeader") String enumNonrefStringHeader, @Param("enumRefStringHeader") StringEnumRef enumRefStringHeader);

  /**
   * Test header parameter(s)
   * Similar to <code>testHeaderIntegerBooleanStringEnums</code> but it also returns the http response headers .
   * Test header parameter(s)
   * @param integerHeader  (optional)
   * @param booleanHeader  (optional)
   * @param stringHeader  (optional)
   * @param enumNonrefStringHeader  (optional)
   * @param enumRefStringHeader  (optional)
   * @return A ApiResponse that wraps the response boyd and the http headers.
   */
  @RequestLine("GET /header/integer/boolean/string/enums")
  @Headers({
    "Accept: text/plain",
    "integer_header: {integerHeader}",
    
    "boolean_header: {booleanHeader}",
    
    "string_header: {stringHeader}",
    
    "enum_nonref_string_header: {enumNonrefStringHeader}",
    
    "enum_ref_string_header: {enumRefStringHeader}"
  })
  ApiResponse<String> testHeaderIntegerBooleanStringEnumsWithHttpInfo(@Param("integerHeader") Integer integerHeader, @Param("booleanHeader") Boolean booleanHeader, @Param("stringHeader") String stringHeader, @Param("enumNonrefStringHeader") String enumNonrefStringHeader, @Param("enumRefStringHeader") StringEnumRef enumRefStringHeader);


}
