package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import org.openapitools.client.EncodingUtils;
import org.openapitools.client.model.ApiResponse;

import java.math.BigDecimal;
import org.openapitools.client.model.Client;
import java.io.File;
import org.openapitools.client.model.FileSchemaTestClass;
import java.time.LocalDate;
import java.time.OffsetDateTime;
import org.openapitools.client.model.OuterComposite;
import org.openapitools.client.model.User;
import org.openapitools.client.model.XmlItem;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import feign.*;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")
public interface FakeApi extends ApiClient.Api {


  /**
   * creates an XmlItem
   * this route creates an XmlItem
   * @param xmlItem XmlItem Body (required)
   */
  @RequestLine("POST /fake/create_xml_item")
  @Headers({
    "Content-Type: application/xml",
    "Accept: application/json",
  })
  void createXmlItem(@javax.annotation.Nonnull XmlItem xmlItem);

  /**
   * creates an XmlItem
   * Similar to <code>createXmlItem</code> but it also returns the http response headers .
   * this route creates an XmlItem
   * @param xmlItem XmlItem Body (required)
   */
  @RequestLine("POST /fake/create_xml_item")
  @Headers({
    "Content-Type: application/xml",
    "Accept: application/json",
  })
  ApiResponse<Void> createXmlItemWithHttpInfo(@javax.annotation.Nonnull XmlItem xmlItem);



  /**
   * 
   * Test serialization of outer boolean types
   * @param body Input boolean as post body (optional)
   * @return Boolean
   */
  @RequestLine("POST /fake/outer/boolean")
  @Headers({
    "Content-Type: */*",
    "Accept: */*",
  })
  Boolean fakeOuterBooleanSerialize(@javax.annotation.Nullable Boolean body);

  /**
   * 
   * Similar to <code>fakeOuterBooleanSerialize</code> but it also returns the http response headers .
   * Test serialization of outer boolean types
   * @param body Input boolean as post body (optional)
   * @return A ApiResponse that wraps the response boyd and the http headers.
   */
  @RequestLine("POST /fake/outer/boolean")
  @Headers({
    "Content-Type: */*",
    "Accept: */*",
  })
  ApiResponse<Boolean> fakeOuterBooleanSerializeWithHttpInfo(@javax.annotation.Nullable Boolean body);



  /**
   * 
   * Test serialization of object with outer number type
   * @param body Input composite as post body (optional)
   * @return OuterComposite
   */
  @RequestLine("POST /fake/outer/composite")
  @Headers({
    "Content-Type: */*",
    "Accept: */*",
  })
  OuterComposite fakeOuterCompositeSerialize(@javax.annotation.Nullable OuterComposite body);

  /**
   * 
   * Similar to <code>fakeOuterCompositeSerialize</code> but it also returns the http response headers .
   * Test serialization of object with outer number type
   * @param body Input composite as post body (optional)
   * @return A ApiResponse that wraps the response boyd and the http headers.
   */
  @RequestLine("POST /fake/outer/composite")
  @Headers({
    "Content-Type: */*",
    "Accept: */*",
  })
  ApiResponse<OuterComposite> fakeOuterCompositeSerializeWithHttpInfo(@javax.annotation.Nullable OuterComposite body);



  /**
   * 
   * Test serialization of outer number types
   * @param body Input number as post body (optional)
   * @return BigDecimal
   */
  @RequestLine("POST /fake/outer/number")
  @Headers({
    "Content-Type: */*",
    "Accept: */*",
  })
  BigDecimal fakeOuterNumberSerialize(@javax.annotation.Nullable BigDecimal body);

  /**
   * 
   * Similar to <code>fakeOuterNumberSerialize</code> but it also returns the http response headers .
   * Test serialization of outer number types
   * @param body Input number as post body (optional)
   * @return A ApiResponse that wraps the response boyd and the http headers.
   */
  @RequestLine("POST /fake/outer/number")
  @Headers({
    "Content-Type: */*",
    "Accept: */*",
  })
  ApiResponse<BigDecimal> fakeOuterNumberSerializeWithHttpInfo(@javax.annotation.Nullable BigDecimal body);



  /**
   * 
   * Test serialization of outer string types
   * @param body Input string as post body (optional)
   * @return String
   */
  @RequestLine("POST /fake/outer/string")
  @Headers({
    "Content-Type: */*",
    "Accept: */*",
  })
  String fakeOuterStringSerialize(@javax.annotation.Nullable String body);

  /**
   * 
   * Similar to <code>fakeOuterStringSerialize</code> but it also returns the http response headers .
   * Test serialization of outer string types
   * @param body Input string as post body (optional)
   * @return A ApiResponse that wraps the response boyd and the http headers.
   */
  @RequestLine("POST /fake/outer/string")
  @Headers({
    "Content-Type: */*",
    "Accept: */*",
  })
  ApiResponse<String> fakeOuterStringSerializeWithHttpInfo(@javax.annotation.Nullable String body);



  /**
   * 
   * For this test, the body for this request much reference a schema named &#x60;File&#x60;.
   * @param body  (required)
   */
  @RequestLine("PUT /fake/body-with-file-schema")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  void testBodyWithFileSchema(@javax.annotation.Nonnull FileSchemaTestClass body);

  /**
   * 
   * Similar to <code>testBodyWithFileSchema</code> but it also returns the http response headers .
   * For this test, the body for this request much reference a schema named &#x60;File&#x60;.
   * @param body  (required)
   */
  @RequestLine("PUT /fake/body-with-file-schema")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  ApiResponse<Void> testBodyWithFileSchemaWithHttpInfo(@javax.annotation.Nonnull FileSchemaTestClass body);



  /**
   * 
   * 
   * @param query  (required)
   * @param body  (required)
   */
  @RequestLine("PUT /fake/body-with-query-params?query={query}")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  void testBodyWithQueryParams(@Param("query") @javax.annotation.Nonnull String query, @javax.annotation.Nonnull User body);

  /**
   * 
   * Similar to <code>testBodyWithQueryParams</code> but it also returns the http response headers .
   * 
   * @param query  (required)
   * @param body  (required)
   */
  @RequestLine("PUT /fake/body-with-query-params?query={query}")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  ApiResponse<Void> testBodyWithQueryParamsWithHttpInfo(@Param("query") @javax.annotation.Nonnull String query, @javax.annotation.Nonnull User body);


  /**
   * 
   * 
   * Note, this is equivalent to the other <code>testBodyWithQueryParams</code> method,
   * but with the query parameters collected into a single Map parameter. This
   * is convenient for services with optional query parameters, especially when
   * used with the {@link TestBodyWithQueryParamsQueryParams} class that allows for
   * building up this map in a fluent style.
   * @param body  (required)
   * @param queryParams Map of query parameters as name-value pairs
   *   <p>The following elements may be specified in the query map:</p>
   *   <ul>
   *   <li>query -  (required)</li>
   *   </ul>
   */
  @RequestLine("PUT /fake/body-with-query-params?query={query}")
  @Headers({
  "Content-Type: application/json",
  "Accept: application/json",
  })
  void testBodyWithQueryParams(@javax.annotation.Nonnull User body, @QueryMap(encoded=true) TestBodyWithQueryParamsQueryParams queryParams);

  /**
  * 
  * 
  * Note, this is equivalent to the other <code>testBodyWithQueryParams</code> that receives the query parameters as a map,
  * but this one also exposes the Http response headers
              * @param body  (required)
      * @param queryParams Map of query parameters as name-value pairs
      *   <p>The following elements may be specified in the query map:</p>
      *   <ul>
          *   <li>query -  (required)</li>
      *   </ul>
      */
      @RequestLine("PUT /fake/body-with-query-params?query={query}")
      @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
      })
   ApiResponse<Void> testBodyWithQueryParamsWithHttpInfo(@javax.annotation.Nonnull User body, @QueryMap(encoded=true) TestBodyWithQueryParamsQueryParams queryParams);


   /**
   * A convenience class for generating query parameters for the
   * <code>testBodyWithQueryParams</code> method in a fluent style.
   */
  public static class TestBodyWithQueryParamsQueryParams extends HashMap<String, Object> {
    public TestBodyWithQueryParamsQueryParams query(@javax.annotation.Nonnull final String value) {
      put("query", EncodingUtils.encode(value));
      return this;
    }
  }

  /**
   * To test \&quot;client\&quot; model
   * To test \&quot;client\&quot; model
   * @param body client model (required)
   * @return Client
   */
  @RequestLine("PATCH /fake")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  Client testClientModel(@javax.annotation.Nonnull Client body);

  /**
   * To test \&quot;client\&quot; model
   * Similar to <code>testClientModel</code> but it also returns the http response headers .
   * To test \&quot;client\&quot; model
   * @param body client model (required)
   * @return A ApiResponse that wraps the response boyd and the http headers.
   */
  @RequestLine("PATCH /fake")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  ApiResponse<Client> testClientModelWithHttpInfo(@javax.annotation.Nonnull Client body);



  /**
   * Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
   * Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
   * @param number None (required)
   * @param _double None (required)
   * @param patternWithoutDelimiter None (required)
   * @param _byte None (required)
   * @param integer None (optional)
   * @param int32 None (optional)
   * @param int64 None (optional)
   * @param _float None (optional)
   * @param string None (optional)
   * @param binary None (optional)
   * @param date None (optional)
   * @param dateTime None (optional)
   * @param password None (optional)
   * @param paramCallback None (optional)
   */
  @RequestLine("POST /fake")
  @Headers({
    "Content-Type: application/x-www-form-urlencoded",
    "Accept: application/json",
  })
  void testEndpointParameters(@Param("number") @javax.annotation.Nonnull BigDecimal number, @Param("double") @javax.annotation.Nonnull Double _double, @Param("pattern_without_delimiter") @javax.annotation.Nonnull String patternWithoutDelimiter, @Param("byte") @javax.annotation.Nonnull byte[] _byte, @Param("integer") @javax.annotation.Nullable Integer integer, @Param("int32") @javax.annotation.Nullable Integer int32, @Param("int64") @javax.annotation.Nullable Long int64, @Param("float") @javax.annotation.Nullable Float _float, @Param("string") @javax.annotation.Nullable String string, @Param("binary") @javax.annotation.Nullable File binary, @Param("date") @javax.annotation.Nullable LocalDate date, @Param("dateTime") @javax.annotation.Nullable OffsetDateTime dateTime, @Param("password") @javax.annotation.Nullable String password, @Param("callback") @javax.annotation.Nullable String paramCallback);

  /**
   * Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
   * Similar to <code>testEndpointParameters</code> but it also returns the http response headers .
   * Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
   * @param number None (required)
   * @param _double None (required)
   * @param patternWithoutDelimiter None (required)
   * @param _byte None (required)
   * @param integer None (optional)
   * @param int32 None (optional)
   * @param int64 None (optional)
   * @param _float None (optional)
   * @param string None (optional)
   * @param binary None (optional)
   * @param date None (optional)
   * @param dateTime None (optional)
   * @param password None (optional)
   * @param paramCallback None (optional)
   */
  @RequestLine("POST /fake")
  @Headers({
    "Content-Type: application/x-www-form-urlencoded",
    "Accept: application/json",
  })
  ApiResponse<Void> testEndpointParametersWithHttpInfo(@Param("number") @javax.annotation.Nonnull BigDecimal number, @Param("double") @javax.annotation.Nonnull Double _double, @Param("pattern_without_delimiter") @javax.annotation.Nonnull String patternWithoutDelimiter, @Param("byte") @javax.annotation.Nonnull byte[] _byte, @Param("integer") @javax.annotation.Nullable Integer integer, @Param("int32") @javax.annotation.Nullable Integer int32, @Param("int64") @javax.annotation.Nullable Long int64, @Param("float") @javax.annotation.Nullable Float _float, @Param("string") @javax.annotation.Nullable String string, @Param("binary") @javax.annotation.Nullable File binary, @Param("date") @javax.annotation.Nullable LocalDate date, @Param("dateTime") @javax.annotation.Nullable OffsetDateTime dateTime, @Param("password") @javax.annotation.Nullable String password, @Param("callback") @javax.annotation.Nullable String paramCallback);



  /**
   * To test enum parameters
   * To test enum parameters
   * @param enumHeaderStringArray Header parameter enum test (string array) (optional)
   * @param enumHeaderString Header parameter enum test (string) (optional, default to -efg)
   * @param enumQueryStringArray Query parameter enum test (string array) (optional)
   * @param enumQueryString Query parameter enum test (string) (optional, default to -efg)
   * @param enumQueryInteger Query parameter enum test (double) (optional)
   * @param enumQueryDouble Query parameter enum test (double) (optional)
   * @param enumFormStringArray Form parameter enum test (string array) (optional)
   * @param enumFormString Form parameter enum test (string) (optional, default to -efg)
   */
  @RequestLine("GET /fake?enum_query_string_array={enumQueryStringArray}&enum_query_string={enumQueryString}&enum_query_integer={enumQueryInteger}&enum_query_double={enumQueryDouble}")
  @Headers({
    "Content-Type: application/x-www-form-urlencoded",
    "Accept: application/json",
    "enum_header_string_array: {enumHeaderStringArray}",
    
    "enum_header_string: {enumHeaderString}"
  })
  void testEnumParameters(@Param("enumHeaderStringArray") @javax.annotation.Nullable List<String> enumHeaderStringArray, @Param("enumHeaderString") @javax.annotation.Nullable String enumHeaderString, @Param("enumQueryStringArray") @javax.annotation.Nullable List<String> enumQueryStringArray, @Param("enumQueryString") @javax.annotation.Nullable String enumQueryString, @Param("enumQueryInteger") @javax.annotation.Nullable Integer enumQueryInteger, @Param("enumQueryDouble") @javax.annotation.Nullable Double enumQueryDouble, @Param("enum_form_string_array") @javax.annotation.Nullable List<String> enumFormStringArray, @Param("enum_form_string") @javax.annotation.Nullable String enumFormString);

  /**
   * To test enum parameters
   * Similar to <code>testEnumParameters</code> but it also returns the http response headers .
   * To test enum parameters
   * @param enumHeaderStringArray Header parameter enum test (string array) (optional)
   * @param enumHeaderString Header parameter enum test (string) (optional, default to -efg)
   * @param enumQueryStringArray Query parameter enum test (string array) (optional)
   * @param enumQueryString Query parameter enum test (string) (optional, default to -efg)
   * @param enumQueryInteger Query parameter enum test (double) (optional)
   * @param enumQueryDouble Query parameter enum test (double) (optional)
   * @param enumFormStringArray Form parameter enum test (string array) (optional)
   * @param enumFormString Form parameter enum test (string) (optional, default to -efg)
   */
  @RequestLine("GET /fake?enum_query_string_array={enumQueryStringArray}&enum_query_string={enumQueryString}&enum_query_integer={enumQueryInteger}&enum_query_double={enumQueryDouble}")
  @Headers({
    "Content-Type: application/x-www-form-urlencoded",
    "Accept: application/json",
    "enum_header_string_array: {enumHeaderStringArray}",
    
    "enum_header_string: {enumHeaderString}"
  })
  ApiResponse<Void> testEnumParametersWithHttpInfo(@Param("enumHeaderStringArray") @javax.annotation.Nullable List<String> enumHeaderStringArray, @Param("enumHeaderString") @javax.annotation.Nullable String enumHeaderString, @Param("enumQueryStringArray") @javax.annotation.Nullable List<String> enumQueryStringArray, @Param("enumQueryString") @javax.annotation.Nullable String enumQueryString, @Param("enumQueryInteger") @javax.annotation.Nullable Integer enumQueryInteger, @Param("enumQueryDouble") @javax.annotation.Nullable Double enumQueryDouble, @Param("enum_form_string_array") @javax.annotation.Nullable List<String> enumFormStringArray, @Param("enum_form_string") @javax.annotation.Nullable String enumFormString);


  /**
   * To test enum parameters
   * To test enum parameters
   * Note, this is equivalent to the other <code>testEnumParameters</code> method,
   * but with the query parameters collected into a single Map parameter. This
   * is convenient for services with optional query parameters, especially when
   * used with the {@link TestEnumParametersQueryParams} class that allows for
   * building up this map in a fluent style.
   * @param enumHeaderStringArray Header parameter enum test (string array) (optional)
   * @param enumHeaderString Header parameter enum test (string) (optional, default to -efg)
   * @param enumFormStringArray Form parameter enum test (string array) (optional)
   * @param enumFormString Form parameter enum test (string) (optional, default to -efg)
   * @param queryParams Map of query parameters as name-value pairs
   *   <p>The following elements may be specified in the query map:</p>
   *   <ul>
   *   <li>enumQueryStringArray - Query parameter enum test (string array) (optional)</li>
   *   <li>enumQueryString - Query parameter enum test (string) (optional, default to -efg)</li>
   *   <li>enumQueryInteger - Query parameter enum test (double) (optional)</li>
   *   <li>enumQueryDouble - Query parameter enum test (double) (optional)</li>
   *   </ul>
   */
  @RequestLine("GET /fake?enum_query_string_array={enumQueryStringArray}&enum_query_string={enumQueryString}&enum_query_integer={enumQueryInteger}&enum_query_double={enumQueryDouble}")
  @Headers({
  "Content-Type: application/x-www-form-urlencoded",
  "Accept: application/json",
      "enum_header_string_array: {enumHeaderStringArray}",
      
      "enum_header_string: {enumHeaderString}"
  })
  void testEnumParameters(@Param("enumHeaderStringArray") @javax.annotation.Nullable List<String> enumHeaderStringArray, @Param("enumHeaderString") @javax.annotation.Nullable String enumHeaderString, @Param("enum_form_string_array") @javax.annotation.Nullable List<String> enumFormStringArray, @Param("enum_form_string") @javax.annotation.Nullable String enumFormString, @QueryMap(encoded=true) TestEnumParametersQueryParams queryParams);

  /**
  * To test enum parameters
  * To test enum parameters
  * Note, this is equivalent to the other <code>testEnumParameters</code> that receives the query parameters as a map,
  * but this one also exposes the Http response headers
              * @param enumHeaderStringArray Header parameter enum test (string array) (optional)
              * @param enumHeaderString Header parameter enum test (string) (optional, default to -efg)
              * @param enumFormStringArray Form parameter enum test (string array) (optional)
              * @param enumFormString Form parameter enum test (string) (optional, default to -efg)
      * @param queryParams Map of query parameters as name-value pairs
      *   <p>The following elements may be specified in the query map:</p>
      *   <ul>
          *   <li>enumQueryStringArray - Query parameter enum test (string array) (optional)</li>
          *   <li>enumQueryString - Query parameter enum test (string) (optional, default to -efg)</li>
          *   <li>enumQueryInteger - Query parameter enum test (double) (optional)</li>
          *   <li>enumQueryDouble - Query parameter enum test (double) (optional)</li>
      *   </ul>
      */
      @RequestLine("GET /fake?enum_query_string_array={enumQueryStringArray}&enum_query_string={enumQueryString}&enum_query_integer={enumQueryInteger}&enum_query_double={enumQueryDouble}")
      @Headers({
    "Content-Type: application/x-www-form-urlencoded",
    "Accept: application/json",
          "enum_header_string_array: {enumHeaderStringArray}",
      
          "enum_header_string: {enumHeaderString}"
      })
   ApiResponse<Void> testEnumParametersWithHttpInfo(@Param("enumHeaderStringArray") @javax.annotation.Nullable List<String> enumHeaderStringArray, @Param("enumHeaderString") @javax.annotation.Nullable String enumHeaderString, @Param("enum_form_string_array") @javax.annotation.Nullable List<String> enumFormStringArray, @Param("enum_form_string") @javax.annotation.Nullable String enumFormString, @QueryMap(encoded=true) TestEnumParametersQueryParams queryParams);


   /**
   * A convenience class for generating query parameters for the
   * <code>testEnumParameters</code> method in a fluent style.
   */
  public static class TestEnumParametersQueryParams extends HashMap<String, Object> {
    public TestEnumParametersQueryParams enumQueryStringArray(@javax.annotation.Nullable final List<String> value) {
      put("enum_query_string_array", EncodingUtils.encodeCollection(value, "csv"));
      return this;
    }
    public TestEnumParametersQueryParams enumQueryString(@javax.annotation.Nullable final String value) {
      put("enum_query_string", EncodingUtils.encode(value));
      return this;
    }
    public TestEnumParametersQueryParams enumQueryInteger(@javax.annotation.Nullable final Integer value) {
      put("enum_query_integer", EncodingUtils.encode(value));
      return this;
    }
    public TestEnumParametersQueryParams enumQueryDouble(@javax.annotation.Nullable final Double value) {
      put("enum_query_double", EncodingUtils.encode(value));
      return this;
    }
  }

  /**
   * Fake endpoint to test group parameters (optional)
   * Fake endpoint to test group parameters (optional)
   * @param requiredStringGroup Required String in group parameters (required)
   * @param requiredBooleanGroup Required Boolean in group parameters (required)
   * @param requiredInt64Group Required Integer in group parameters (required)
   * @param stringGroup String in group parameters (optional)
   * @param booleanGroup Boolean in group parameters (optional)
   * @param int64Group Integer in group parameters (optional)
   */
  @RequestLine("DELETE /fake?required_string_group={requiredStringGroup}&required_int64_group={requiredInt64Group}&string_group={stringGroup}&int64_group={int64Group}")
  @Headers({
    "Accept: application/json",
    "required_boolean_group: {requiredBooleanGroup}",
    
    "boolean_group: {booleanGroup}"
  })
  void testGroupParameters(@Param("requiredStringGroup") @javax.annotation.Nonnull Integer requiredStringGroup, @Param("requiredBooleanGroup") @javax.annotation.Nonnull Boolean requiredBooleanGroup, @Param("requiredInt64Group") @javax.annotation.Nonnull Long requiredInt64Group, @Param("stringGroup") @javax.annotation.Nullable Integer stringGroup, @Param("booleanGroup") @javax.annotation.Nullable Boolean booleanGroup, @Param("int64Group") @javax.annotation.Nullable Long int64Group);

  /**
   * Fake endpoint to test group parameters (optional)
   * Similar to <code>testGroupParameters</code> but it also returns the http response headers .
   * Fake endpoint to test group parameters (optional)
   * @param requiredStringGroup Required String in group parameters (required)
   * @param requiredBooleanGroup Required Boolean in group parameters (required)
   * @param requiredInt64Group Required Integer in group parameters (required)
   * @param stringGroup String in group parameters (optional)
   * @param booleanGroup Boolean in group parameters (optional)
   * @param int64Group Integer in group parameters (optional)
   */
  @RequestLine("DELETE /fake?required_string_group={requiredStringGroup}&required_int64_group={requiredInt64Group}&string_group={stringGroup}&int64_group={int64Group}")
  @Headers({
    "Accept: application/json",
    "required_boolean_group: {requiredBooleanGroup}",
    
    "boolean_group: {booleanGroup}"
  })
  ApiResponse<Void> testGroupParametersWithHttpInfo(@Param("requiredStringGroup") @javax.annotation.Nonnull Integer requiredStringGroup, @Param("requiredBooleanGroup") @javax.annotation.Nonnull Boolean requiredBooleanGroup, @Param("requiredInt64Group") @javax.annotation.Nonnull Long requiredInt64Group, @Param("stringGroup") @javax.annotation.Nullable Integer stringGroup, @Param("booleanGroup") @javax.annotation.Nullable Boolean booleanGroup, @Param("int64Group") @javax.annotation.Nullable Long int64Group);


  /**
   * Fake endpoint to test group parameters (optional)
   * Fake endpoint to test group parameters (optional)
   * Note, this is equivalent to the other <code>testGroupParameters</code> method,
   * but with the query parameters collected into a single Map parameter. This
   * is convenient for services with optional query parameters, especially when
   * used with the {@link TestGroupParametersQueryParams} class that allows for
   * building up this map in a fluent style.
   * @param requiredBooleanGroup Required Boolean in group parameters (required)
   * @param booleanGroup Boolean in group parameters (optional)
   * @param queryParams Map of query parameters as name-value pairs
   *   <p>The following elements may be specified in the query map:</p>
   *   <ul>
   *   <li>requiredStringGroup - Required String in group parameters (required)</li>
   *   <li>requiredInt64Group - Required Integer in group parameters (required)</li>
   *   <li>stringGroup - String in group parameters (optional)</li>
   *   <li>int64Group - Integer in group parameters (optional)</li>
   *   </ul>
   */
  @RequestLine("DELETE /fake?required_string_group={requiredStringGroup}&required_int64_group={requiredInt64Group}&string_group={stringGroup}&int64_group={int64Group}")
  @Headers({
  "Accept: application/json",
      "required_boolean_group: {requiredBooleanGroup}",
      
      "boolean_group: {booleanGroup}"
  })
  void testGroupParameters(@Param("requiredBooleanGroup") @javax.annotation.Nonnull Boolean requiredBooleanGroup, @Param("booleanGroup") @javax.annotation.Nullable Boolean booleanGroup, @QueryMap(encoded=true) TestGroupParametersQueryParams queryParams);

  /**
  * Fake endpoint to test group parameters (optional)
  * Fake endpoint to test group parameters (optional)
  * Note, this is equivalent to the other <code>testGroupParameters</code> that receives the query parameters as a map,
  * but this one also exposes the Http response headers
              * @param requiredBooleanGroup Required Boolean in group parameters (required)
              * @param booleanGroup Boolean in group parameters (optional)
      * @param queryParams Map of query parameters as name-value pairs
      *   <p>The following elements may be specified in the query map:</p>
      *   <ul>
          *   <li>requiredStringGroup - Required String in group parameters (required)</li>
          *   <li>requiredInt64Group - Required Integer in group parameters (required)</li>
          *   <li>stringGroup - String in group parameters (optional)</li>
          *   <li>int64Group - Integer in group parameters (optional)</li>
      *   </ul>
      */
      @RequestLine("DELETE /fake?required_string_group={requiredStringGroup}&required_int64_group={requiredInt64Group}&string_group={stringGroup}&int64_group={int64Group}")
      @Headers({
    "Accept: application/json",
          "required_boolean_group: {requiredBooleanGroup}",
      
          "boolean_group: {booleanGroup}"
      })
   ApiResponse<Void> testGroupParametersWithHttpInfo(@Param("requiredBooleanGroup") @javax.annotation.Nonnull Boolean requiredBooleanGroup, @Param("booleanGroup") @javax.annotation.Nullable Boolean booleanGroup, @QueryMap(encoded=true) TestGroupParametersQueryParams queryParams);


   /**
   * A convenience class for generating query parameters for the
   * <code>testGroupParameters</code> method in a fluent style.
   */
  public static class TestGroupParametersQueryParams extends HashMap<String, Object> {
    public TestGroupParametersQueryParams requiredStringGroup(@javax.annotation.Nonnull final Integer value) {
      put("required_string_group", EncodingUtils.encode(value));
      return this;
    }
    public TestGroupParametersQueryParams requiredInt64Group(@javax.annotation.Nonnull final Long value) {
      put("required_int64_group", EncodingUtils.encode(value));
      return this;
    }
    public TestGroupParametersQueryParams stringGroup(@javax.annotation.Nullable final Integer value) {
      put("string_group", EncodingUtils.encode(value));
      return this;
    }
    public TestGroupParametersQueryParams int64Group(@javax.annotation.Nullable final Long value) {
      put("int64_group", EncodingUtils.encode(value));
      return this;
    }
  }

  /**
   * test inline additionalProperties
   * 
   * @param param request body (required)
   */
  @RequestLine("POST /fake/inline-additionalProperties")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  void testInlineAdditionalProperties(@javax.annotation.Nonnull Map<String, String> param);

  /**
   * test inline additionalProperties
   * Similar to <code>testInlineAdditionalProperties</code> but it also returns the http response headers .
   * 
   * @param param request body (required)
   */
  @RequestLine("POST /fake/inline-additionalProperties")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  ApiResponse<Void> testInlineAdditionalPropertiesWithHttpInfo(@javax.annotation.Nonnull Map<String, String> param);



  /**
   * test json serialization of form data
   * 
   * @param param field1 (required)
   * @param param2 field2 (required)
   */
  @RequestLine("GET /fake/jsonFormData")
  @Headers({
    "Content-Type: application/x-www-form-urlencoded",
    "Accept: application/json",
  })
  void testJsonFormData(@Param("param") @javax.annotation.Nonnull String param, @Param("param2") @javax.annotation.Nonnull String param2);

  /**
   * test json serialization of form data
   * Similar to <code>testJsonFormData</code> but it also returns the http response headers .
   * 
   * @param param field1 (required)
   * @param param2 field2 (required)
   */
  @RequestLine("GET /fake/jsonFormData")
  @Headers({
    "Content-Type: application/x-www-form-urlencoded",
    "Accept: application/json",
  })
  ApiResponse<Void> testJsonFormDataWithHttpInfo(@Param("param") @javax.annotation.Nonnull String param, @Param("param2") @javax.annotation.Nonnull String param2);



  /**
   * 
   * To test the collection format in query parameters
   * @param pipe  (required)
   * @param ioutil  (required)
   * @param http  (required)
   * @param url  (required)
   * @param context  (required)
   */
  @RequestLine("PUT /fake/test-query-parameters?pipe={pipe}&ioutil={ioutil}&http={http}&url={url}&context={context}")
  @Headers({
    "Accept: application/json",
  })
  void testQueryParameterCollectionFormat(@Param("pipe") @javax.annotation.Nonnull List<String> pipe, @Param("ioutil") @javax.annotation.Nonnull List<String> ioutil, @Param("http") @javax.annotation.Nonnull List<String> http, @Param("url") @javax.annotation.Nonnull List<String> url, @Param("context") @javax.annotation.Nonnull List<String> context);

  /**
   * 
   * Similar to <code>testQueryParameterCollectionFormat</code> but it also returns the http response headers .
   * To test the collection format in query parameters
   * @param pipe  (required)
   * @param ioutil  (required)
   * @param http  (required)
   * @param url  (required)
   * @param context  (required)
   */
  @RequestLine("PUT /fake/test-query-parameters?pipe={pipe}&ioutil={ioutil}&http={http}&url={url}&context={context}")
  @Headers({
    "Accept: application/json",
  })
  ApiResponse<Void> testQueryParameterCollectionFormatWithHttpInfo(@Param("pipe") @javax.annotation.Nonnull List<String> pipe, @Param("ioutil") @javax.annotation.Nonnull List<String> ioutil, @Param("http") @javax.annotation.Nonnull List<String> http, @Param("url") @javax.annotation.Nonnull List<String> url, @Param("context") @javax.annotation.Nonnull List<String> context);


  /**
   * 
   * To test the collection format in query parameters
   * Note, this is equivalent to the other <code>testQueryParameterCollectionFormat</code> method,
   * but with the query parameters collected into a single Map parameter. This
   * is convenient for services with optional query parameters, especially when
   * used with the {@link TestQueryParameterCollectionFormatQueryParams} class that allows for
   * building up this map in a fluent style.
   * @param queryParams Map of query parameters as name-value pairs
   *   <p>The following elements may be specified in the query map:</p>
   *   <ul>
   *   <li>pipe -  (required)</li>
   *   <li>ioutil -  (required)</li>
   *   <li>http -  (required)</li>
   *   <li>url -  (required)</li>
   *   <li>context -  (required)</li>
   *   </ul>
   */
  @RequestLine("PUT /fake/test-query-parameters?pipe={pipe}&ioutil={ioutil}&http={http}&url={url}&context={context}")
  @Headers({
  "Accept: application/json",
  })
  void testQueryParameterCollectionFormat(@QueryMap(encoded=true) TestQueryParameterCollectionFormatQueryParams queryParams);

  /**
  * 
  * To test the collection format in query parameters
  * Note, this is equivalent to the other <code>testQueryParameterCollectionFormat</code> that receives the query parameters as a map,
  * but this one also exposes the Http response headers
      * @param queryParams Map of query parameters as name-value pairs
      *   <p>The following elements may be specified in the query map:</p>
      *   <ul>
          *   <li>pipe -  (required)</li>
          *   <li>ioutil -  (required)</li>
          *   <li>http -  (required)</li>
          *   <li>url -  (required)</li>
          *   <li>context -  (required)</li>
      *   </ul>
      */
      @RequestLine("PUT /fake/test-query-parameters?pipe={pipe}&ioutil={ioutil}&http={http}&url={url}&context={context}")
      @Headers({
    "Accept: application/json",
      })
   ApiResponse<Void> testQueryParameterCollectionFormatWithHttpInfo(@QueryMap(encoded=true) TestQueryParameterCollectionFormatQueryParams queryParams);


   /**
   * A convenience class for generating query parameters for the
   * <code>testQueryParameterCollectionFormat</code> method in a fluent style.
   */
  public static class TestQueryParameterCollectionFormatQueryParams extends HashMap<String, Object> {
    public TestQueryParameterCollectionFormatQueryParams pipe(@javax.annotation.Nonnull final List<String> value) {
      put("pipe", EncodingUtils.encodeCollection(value, "csv"));
      return this;
    }
    public TestQueryParameterCollectionFormatQueryParams ioutil(@javax.annotation.Nonnull final List<String> value) {
      put("ioutil", EncodingUtils.encodeCollection(value, "csv"));
      return this;
    }
    public TestQueryParameterCollectionFormatQueryParams http(@javax.annotation.Nonnull final List<String> value) {
      put("http", EncodingUtils.encodeCollection(value, "ssv"));
      return this;
    }
    public TestQueryParameterCollectionFormatQueryParams url(@javax.annotation.Nonnull final List<String> value) {
      put("url", EncodingUtils.encodeCollection(value, "csv"));
      return this;
    }
    public TestQueryParameterCollectionFormatQueryParams context(@javax.annotation.Nonnull final List<String> value) {
      put("context", EncodingUtils.encodeCollection(value, "multi"));
      return this;
    }
  }
}
