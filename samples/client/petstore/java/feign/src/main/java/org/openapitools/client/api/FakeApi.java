package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import org.openapitools.client.EncodingUtils;
import org.openapitools.client.model.ApiResponse;

import java.math.BigDecimal;
import org.openapitools.client.model.ChildWithNullable;
import org.openapitools.client.model.Client;
import org.openapitools.client.model.EnumClass;
import org.openapitools.client.model.FakeBigDecimalMap200Response;
import java.io.File;
import org.openapitools.client.model.FileSchemaTestClass;
import org.openapitools.client.model.HealthCheckResult;
import java.time.LocalDate;
import java.time.OffsetDateTime;
import org.openapitools.client.model.OuterComposite;
import org.openapitools.client.model.OuterObjectWithEnumProperty;
import org.openapitools.client.model.Pet;
import org.openapitools.client.model.TestInlineFreeformAdditionalPropertiesRequest;
import org.openapitools.client.model.User;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import feign.*;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen", comments = "Generator version: 7.14.0-SNAPSHOT")
public interface FakeApi extends ApiClient.Api {


  /**
   * 
   * for Java apache and Java native, test toUrlQueryString for maps with BegDecimal keys
   * @return FakeBigDecimalMap200Response
   */
  @RequestLine("GET /fake/BigDecimalMap")
  @Headers({
    "Accept: */*",
  })
  FakeBigDecimalMap200Response fakeBigDecimalMap();

  /**
   * 
   * Similar to <code>fakeBigDecimalMap</code> but it also returns the http response headers .
   * for Java apache and Java native, test toUrlQueryString for maps with BegDecimal keys
   * @return A ApiResponse that wraps the response boyd and the http headers.
   */
  @RequestLine("GET /fake/BigDecimalMap")
  @Headers({
    "Accept: */*",
  })
  ApiResponse<FakeBigDecimalMap200Response> fakeBigDecimalMapWithHttpInfo();



  /**
   * Health check endpoint
   * 
   * @return HealthCheckResult
   */
  @RequestLine("GET /fake/health")
  @Headers({
    "Accept: application/json",
  })
  HealthCheckResult fakeHealthGet();

  /**
   * Health check endpoint
   * Similar to <code>fakeHealthGet</code> but it also returns the http response headers .
   * 
   * @return A ApiResponse that wraps the response boyd and the http headers.
   */
  @RequestLine("GET /fake/health")
  @Headers({
    "Accept: application/json",
  })
  ApiResponse<HealthCheckResult> fakeHealthGetWithHttpInfo();



  /**
   * test http signature authentication
   * 
   * @param pet Pet object that needs to be added to the store (required)
   * @param query1 query parameter (optional)
   * @param header1 header parameter (optional)
   */
  @RequestLine("GET /fake/http-signature-test?query_1={query1}")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
    "header_1: {header1}"
  })
  void fakeHttpSignatureTest(@javax.annotation.Nonnull Pet pet, @Param("query1") @javax.annotation.Nullable String query1, @Param("header1") @javax.annotation.Nullable String header1);

  /**
   * test http signature authentication
   * Similar to <code>fakeHttpSignatureTest</code> but it also returns the http response headers .
   * 
   * @param pet Pet object that needs to be added to the store (required)
   * @param query1 query parameter (optional)
   * @param header1 header parameter (optional)
   */
  @RequestLine("GET /fake/http-signature-test?query_1={query1}")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
    "header_1: {header1}"
  })
  ApiResponse<Void> fakeHttpSignatureTestWithHttpInfo(@javax.annotation.Nonnull Pet pet, @Param("query1") @javax.annotation.Nullable String query1, @Param("header1") @javax.annotation.Nullable String header1);


  /**
   * test http signature authentication
   * 
   * Note, this is equivalent to the other <code>fakeHttpSignatureTest</code> method,
   * but with the query parameters collected into a single Map parameter. This
   * is convenient for services with optional query parameters, especially when
   * used with the {@link FakeHttpSignatureTestQueryParams} class that allows for
   * building up this map in a fluent style.
   * @param pet Pet object that needs to be added to the store (required)
   * @param header1 header parameter (optional)
   * @param queryParams Map of query parameters as name-value pairs
   *   <p>The following elements may be specified in the query map:</p>
   *   <ul>
   *   <li>query1 - query parameter (optional)</li>
   *   </ul>
   */
  @RequestLine("GET /fake/http-signature-test?query_1={query1}")
  @Headers({
  "Content-Type: application/json",
  "Accept: application/json",
      "header_1: {header1}"
  })
  void fakeHttpSignatureTest(@javax.annotation.Nonnull Pet pet, @Param("header1") @javax.annotation.Nullable String header1, @QueryMap(encoded=true) FakeHttpSignatureTestQueryParams queryParams);

  /**
  * test http signature authentication
  * 
  * Note, this is equivalent to the other <code>fakeHttpSignatureTest</code> that receives the query parameters as a map,
  * but this one also exposes the Http response headers
              * @param pet Pet object that needs to be added to the store (required)
              * @param header1 header parameter (optional)
      * @param queryParams Map of query parameters as name-value pairs
      *   <p>The following elements may be specified in the query map:</p>
      *   <ul>
          *   <li>query1 - query parameter (optional)</li>
      *   </ul>
      */
      @RequestLine("GET /fake/http-signature-test?query_1={query1}")
      @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
          "header_1: {header1}"
      })
   ApiResponse<Void> fakeHttpSignatureTestWithHttpInfo(@javax.annotation.Nonnull Pet pet, @Param("header1") @javax.annotation.Nullable String header1, @QueryMap(encoded=true) FakeHttpSignatureTestQueryParams queryParams);


   /**
   * A convenience class for generating query parameters for the
   * <code>fakeHttpSignatureTest</code> method in a fluent style.
   */
  public static class FakeHttpSignatureTestQueryParams extends HashMap<String, Object> {
    public FakeHttpSignatureTestQueryParams query1(@javax.annotation.Nullable final String value) {
      put("query_1", EncodingUtils.encode(value));
      return this;
    }
  }

  /**
   * 
   * Test serialization of outer boolean types
   * @param body Input boolean as post body (optional)
   * @return Boolean
   */
  @RequestLine("POST /fake/outer/boolean")
  @Headers({
    "Content-Type: application/json",
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
    "Content-Type: application/json",
    "Accept: */*",
  })
  ApiResponse<Boolean> fakeOuterBooleanSerializeWithHttpInfo(@javax.annotation.Nullable Boolean body);



  /**
   * 
   * Test serialization of object with outer number type
   * @param outerComposite Input composite as post body (optional)
   * @return OuterComposite
   */
  @RequestLine("POST /fake/outer/composite")
  @Headers({
    "Content-Type: application/json",
    "Accept: */*",
  })
  OuterComposite fakeOuterCompositeSerialize(@javax.annotation.Nullable OuterComposite outerComposite);

  /**
   * 
   * Similar to <code>fakeOuterCompositeSerialize</code> but it also returns the http response headers .
   * Test serialization of object with outer number type
   * @param outerComposite Input composite as post body (optional)
   * @return A ApiResponse that wraps the response boyd and the http headers.
   */
  @RequestLine("POST /fake/outer/composite")
  @Headers({
    "Content-Type: application/json",
    "Accept: */*",
  })
  ApiResponse<OuterComposite> fakeOuterCompositeSerializeWithHttpInfo(@javax.annotation.Nullable OuterComposite outerComposite);



  /**
   * 
   * Test serialization of outer number types
   * @param body Input number as post body (optional)
   * @return BigDecimal
   */
  @RequestLine("POST /fake/outer/number")
  @Headers({
    "Content-Type: application/json",
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
    "Content-Type: application/json",
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
    "Content-Type: application/json",
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
    "Content-Type: application/json",
    "Accept: */*",
  })
  ApiResponse<String> fakeOuterStringSerializeWithHttpInfo(@javax.annotation.Nullable String body);



  /**
   * 
   * Test serialization of enum (int) properties with examples
   * @param outerObjectWithEnumProperty Input enum (int) as post body (required)
   * @return OuterObjectWithEnumProperty
   */
  @RequestLine("POST /fake/property/enum-int")
  @Headers({
    "Content-Type: application/json",
    "Accept: */*",
  })
  OuterObjectWithEnumProperty fakePropertyEnumIntegerSerialize(@javax.annotation.Nonnull OuterObjectWithEnumProperty outerObjectWithEnumProperty);

  /**
   * 
   * Similar to <code>fakePropertyEnumIntegerSerialize</code> but it also returns the http response headers .
   * Test serialization of enum (int) properties with examples
   * @param outerObjectWithEnumProperty Input enum (int) as post body (required)
   * @return A ApiResponse that wraps the response boyd and the http headers.
   */
  @RequestLine("POST /fake/property/enum-int")
  @Headers({
    "Content-Type: application/json",
    "Accept: */*",
  })
  ApiResponse<OuterObjectWithEnumProperty> fakePropertyEnumIntegerSerializeWithHttpInfo(@javax.annotation.Nonnull OuterObjectWithEnumProperty outerObjectWithEnumProperty);



  /**
   * test referenced additionalProperties
   * 
   * @param requestBody request body (required)
   */
  @RequestLine("POST /fake/additionalProperties-reference")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  void testAdditionalPropertiesReference(@javax.annotation.Nonnull Map<String, Object> requestBody);

  /**
   * test referenced additionalProperties
   * Similar to <code>testAdditionalPropertiesReference</code> but it also returns the http response headers .
   * 
   * @param requestBody request body (required)
   */
  @RequestLine("POST /fake/additionalProperties-reference")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  ApiResponse<Void> testAdditionalPropertiesReferenceWithHttpInfo(@javax.annotation.Nonnull Map<String, Object> requestBody);



  /**
   * 
   * For this test, the body has to be a binary file.
   * @param body image to upload (required)
   */
  @RequestLine("PUT /fake/body-with-binary")
  @Headers({
    "Content-Type: image/png",
    "Accept: application/json",
  })
  void testBodyWithBinary(@javax.annotation.Nullable File body);

  /**
   * 
   * Similar to <code>testBodyWithBinary</code> but it also returns the http response headers .
   * For this test, the body has to be a binary file.
   * @param body image to upload (required)
   */
  @RequestLine("PUT /fake/body-with-binary")
  @Headers({
    "Content-Type: image/png",
    "Accept: application/json",
  })
  ApiResponse<Void> testBodyWithBinaryWithHttpInfo(@javax.annotation.Nullable File body);



  /**
   * 
   * For this test, the body for this request must reference a schema named &#x60;File&#x60;.
   * @param fileSchemaTestClass  (required)
   */
  @RequestLine("PUT /fake/body-with-file-schema")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  void testBodyWithFileSchema(@javax.annotation.Nonnull FileSchemaTestClass fileSchemaTestClass);

  /**
   * 
   * Similar to <code>testBodyWithFileSchema</code> but it also returns the http response headers .
   * For this test, the body for this request must reference a schema named &#x60;File&#x60;.
   * @param fileSchemaTestClass  (required)
   */
  @RequestLine("PUT /fake/body-with-file-schema")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  ApiResponse<Void> testBodyWithFileSchemaWithHttpInfo(@javax.annotation.Nonnull FileSchemaTestClass fileSchemaTestClass);



  /**
   * 
   * 
   * @param query  (required)
   * @param user  (required)
   */
  @RequestLine("PUT /fake/body-with-query-params?query={query}")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  void testBodyWithQueryParams(@Param("query") @javax.annotation.Nonnull String query, @javax.annotation.Nonnull User user);

  /**
   * 
   * Similar to <code>testBodyWithQueryParams</code> but it also returns the http response headers .
   * 
   * @param query  (required)
   * @param user  (required)
   */
  @RequestLine("PUT /fake/body-with-query-params?query={query}")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  ApiResponse<Void> testBodyWithQueryParamsWithHttpInfo(@Param("query") @javax.annotation.Nonnull String query, @javax.annotation.Nonnull User user);


  /**
   * 
   * 
   * Note, this is equivalent to the other <code>testBodyWithQueryParams</code> method,
   * but with the query parameters collected into a single Map parameter. This
   * is convenient for services with optional query parameters, especially when
   * used with the {@link TestBodyWithQueryParamsQueryParams} class that allows for
   * building up this map in a fluent style.
   * @param user  (required)
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
  void testBodyWithQueryParams(@javax.annotation.Nonnull User user, @QueryMap(encoded=true) TestBodyWithQueryParamsQueryParams queryParams);

  /**
  * 
  * 
  * Note, this is equivalent to the other <code>testBodyWithQueryParams</code> that receives the query parameters as a map,
  * but this one also exposes the Http response headers
              * @param user  (required)
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
   ApiResponse<Void> testBodyWithQueryParamsWithHttpInfo(@javax.annotation.Nonnull User user, @QueryMap(encoded=true) TestBodyWithQueryParamsQueryParams queryParams);


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
   * @param client client model (required)
   * @return Client
   */
  @RequestLine("PATCH /fake")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  Client testClientModel(@javax.annotation.Nonnull Client client);

  /**
   * To test \&quot;client\&quot; model
   * Similar to <code>testClientModel</code> but it also returns the http response headers .
   * To test \&quot;client\&quot; model
   * @param client client model (required)
   * @return A ApiResponse that wraps the response boyd and the http headers.
   */
  @RequestLine("PATCH /fake")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  ApiResponse<Client> testClientModelWithHttpInfo(@javax.annotation.Nonnull Client client);



  /**
   * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
   * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
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
   * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
   * Similar to <code>testEndpointParameters</code> but it also returns the http response headers .
   * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
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
   * @param enumQueryModelArray  (optional)
   * @param enumFormStringArray Form parameter enum test (string array) (optional)
   * @param enumFormString Form parameter enum test (string) (optional, default to -efg)
   */
  @RequestLine("GET /fake?enum_query_string_array={enumQueryStringArray}&enum_query_string={enumQueryString}&enum_query_integer={enumQueryInteger}&enum_query_double={enumQueryDouble}&enum_query_model_array={enumQueryModelArray}")
  @Headers({
    "Content-Type: application/x-www-form-urlencoded",
    "Accept: application/json",
    "enum_header_string_array: {enumHeaderStringArray}",
    
    "enum_header_string: {enumHeaderString}"
  })
  void testEnumParameters(@Param("enumHeaderStringArray") @javax.annotation.Nullable List<String> enumHeaderStringArray, @Param("enumHeaderString") @javax.annotation.Nullable String enumHeaderString, @Param("enumQueryStringArray") @javax.annotation.Nullable List<String> enumQueryStringArray, @Param("enumQueryString") @javax.annotation.Nullable String enumQueryString, @Param("enumQueryInteger") @javax.annotation.Nullable Integer enumQueryInteger, @Param("enumQueryDouble") @javax.annotation.Nullable Double enumQueryDouble, @Param("enumQueryModelArray") @javax.annotation.Nullable List<EnumClass> enumQueryModelArray, @Param("enum_form_string_array") @javax.annotation.Nullable List<String> enumFormStringArray, @Param("enum_form_string") @javax.annotation.Nullable String enumFormString);

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
   * @param enumQueryModelArray  (optional)
   * @param enumFormStringArray Form parameter enum test (string array) (optional)
   * @param enumFormString Form parameter enum test (string) (optional, default to -efg)
   */
  @RequestLine("GET /fake?enum_query_string_array={enumQueryStringArray}&enum_query_string={enumQueryString}&enum_query_integer={enumQueryInteger}&enum_query_double={enumQueryDouble}&enum_query_model_array={enumQueryModelArray}")
  @Headers({
    "Content-Type: application/x-www-form-urlencoded",
    "Accept: application/json",
    "enum_header_string_array: {enumHeaderStringArray}",
    
    "enum_header_string: {enumHeaderString}"
  })
  ApiResponse<Void> testEnumParametersWithHttpInfo(@Param("enumHeaderStringArray") @javax.annotation.Nullable List<String> enumHeaderStringArray, @Param("enumHeaderString") @javax.annotation.Nullable String enumHeaderString, @Param("enumQueryStringArray") @javax.annotation.Nullable List<String> enumQueryStringArray, @Param("enumQueryString") @javax.annotation.Nullable String enumQueryString, @Param("enumQueryInteger") @javax.annotation.Nullable Integer enumQueryInteger, @Param("enumQueryDouble") @javax.annotation.Nullable Double enumQueryDouble, @Param("enumQueryModelArray") @javax.annotation.Nullable List<EnumClass> enumQueryModelArray, @Param("enum_form_string_array") @javax.annotation.Nullable List<String> enumFormStringArray, @Param("enum_form_string") @javax.annotation.Nullable String enumFormString);


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
   *   <li>enumQueryModelArray -  (optional)</li>
   *   </ul>
   */
  @RequestLine("GET /fake?enum_query_string_array={enumQueryStringArray}&enum_query_string={enumQueryString}&enum_query_integer={enumQueryInteger}&enum_query_double={enumQueryDouble}&enum_query_model_array={enumQueryModelArray}")
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
          *   <li>enumQueryModelArray -  (optional)</li>
      *   </ul>
      */
      @RequestLine("GET /fake?enum_query_string_array={enumQueryStringArray}&enum_query_string={enumQueryString}&enum_query_integer={enumQueryInteger}&enum_query_double={enumQueryDouble}&enum_query_model_array={enumQueryModelArray}")
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
      put("enum_query_string_array", EncodingUtils.encodeCollection(value, "multi"));
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
    public TestEnumParametersQueryParams enumQueryModelArray(@javax.annotation.Nullable final List<EnumClass> value) {
      put("enum_query_model_array", EncodingUtils.encodeCollection(value, "multi"));
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
   * @param requestBody request body (required)
   */
  @RequestLine("POST /fake/inline-additionalProperties")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  void testInlineAdditionalProperties(@javax.annotation.Nonnull Map<String, String> requestBody);

  /**
   * test inline additionalProperties
   * Similar to <code>testInlineAdditionalProperties</code> but it also returns the http response headers .
   * 
   * @param requestBody request body (required)
   */
  @RequestLine("POST /fake/inline-additionalProperties")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  ApiResponse<Void> testInlineAdditionalPropertiesWithHttpInfo(@javax.annotation.Nonnull Map<String, String> requestBody);



  /**
   * test inline free-form additionalProperties
   * 
   * @param testInlineFreeformAdditionalPropertiesRequest request body (required)
   */
  @RequestLine("POST /fake/inline-freeform-additionalProperties")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  void testInlineFreeformAdditionalProperties(@javax.annotation.Nonnull TestInlineFreeformAdditionalPropertiesRequest testInlineFreeformAdditionalPropertiesRequest);

  /**
   * test inline free-form additionalProperties
   * Similar to <code>testInlineFreeformAdditionalProperties</code> but it also returns the http response headers .
   * 
   * @param testInlineFreeformAdditionalPropertiesRequest request body (required)
   */
  @RequestLine("POST /fake/inline-freeform-additionalProperties")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  ApiResponse<Void> testInlineFreeformAdditionalPropertiesWithHttpInfo(@javax.annotation.Nonnull TestInlineFreeformAdditionalPropertiesRequest testInlineFreeformAdditionalPropertiesRequest);



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
   * test nullable parent property
   * 
   * @param childWithNullable request body (required)
   */
  @RequestLine("POST /fake/nullable")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  void testNullable(@javax.annotation.Nonnull ChildWithNullable childWithNullable);

  /**
   * test nullable parent property
   * Similar to <code>testNullable</code> but it also returns the http response headers .
   * 
   * @param childWithNullable request body (required)
   */
  @RequestLine("POST /fake/nullable")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  ApiResponse<Void> testNullableWithHttpInfo(@javax.annotation.Nonnull ChildWithNullable childWithNullable);



  /**
   * 
   * To test the collection format in query parameters
   * @param pipe  (required)
   * @param ioutil  (required)
   * @param http  (required)
   * @param url  (required)
   * @param context  (required)
   * @param allowEmpty  (required)
   * @param language  (optional)
   */
  @RequestLine("PUT /fake/test-query-parameters?pipe={pipe}&ioutil={ioutil}&http={http}&url={url}&context={context}&language={language}&allowEmpty={allowEmpty}")
  @Headers({
    "Accept: application/json",
  })
  void testQueryParameterCollectionFormat(@Param("pipe") @javax.annotation.Nonnull List<String> pipe, @Param("ioutil") @javax.annotation.Nonnull List<String> ioutil, @Param("http") @javax.annotation.Nonnull List<String> http, @Param("url") @javax.annotation.Nonnull List<String> url, @Param("context") @javax.annotation.Nonnull List<String> context, @Param("allowEmpty") @javax.annotation.Nonnull String allowEmpty, @Param("language") @javax.annotation.Nullable Map<String, String> language);

  /**
   * 
   * Similar to <code>testQueryParameterCollectionFormat</code> but it also returns the http response headers .
   * To test the collection format in query parameters
   * @param pipe  (required)
   * @param ioutil  (required)
   * @param http  (required)
   * @param url  (required)
   * @param context  (required)
   * @param allowEmpty  (required)
   * @param language  (optional)
   */
  @RequestLine("PUT /fake/test-query-parameters?pipe={pipe}&ioutil={ioutil}&http={http}&url={url}&context={context}&language={language}&allowEmpty={allowEmpty}")
  @Headers({
    "Accept: application/json",
  })
  ApiResponse<Void> testQueryParameterCollectionFormatWithHttpInfo(@Param("pipe") @javax.annotation.Nonnull List<String> pipe, @Param("ioutil") @javax.annotation.Nonnull List<String> ioutil, @Param("http") @javax.annotation.Nonnull List<String> http, @Param("url") @javax.annotation.Nonnull List<String> url, @Param("context") @javax.annotation.Nonnull List<String> context, @Param("allowEmpty") @javax.annotation.Nonnull String allowEmpty, @Param("language") @javax.annotation.Nullable Map<String, String> language);


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
   *   <li>language -  (optional)</li>
   *   <li>allowEmpty -  (required)</li>
   *   </ul>
   */
  @RequestLine("PUT /fake/test-query-parameters?pipe={pipe}&ioutil={ioutil}&http={http}&url={url}&context={context}&language={language}&allowEmpty={allowEmpty}")
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
          *   <li>language -  (optional)</li>
          *   <li>allowEmpty -  (required)</li>
      *   </ul>
      */
      @RequestLine("PUT /fake/test-query-parameters?pipe={pipe}&ioutil={ioutil}&http={http}&url={url}&context={context}&language={language}&allowEmpty={allowEmpty}")
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
      put("pipe", EncodingUtils.encodeCollection(value, "pipes"));
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
    public TestQueryParameterCollectionFormatQueryParams language(@javax.annotation.Nullable final Map<String, String> value) {
      put("language", EncodingUtils.encode(value));
      return this;
    }
    public TestQueryParameterCollectionFormatQueryParams allowEmpty(@javax.annotation.Nonnull final String value) {
      put("allowEmpty", EncodingUtils.encode(value));
      return this;
    }
  }

  /**
   * test referenced string map
   * 
   * @param requestBody request body (required)
   */
  @RequestLine("POST /fake/stringMap-reference")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  void testStringMapReference(@javax.annotation.Nonnull Map<String, String> requestBody);

  /**
   * test referenced string map
   * Similar to <code>testStringMapReference</code> but it also returns the http response headers .
   * 
   * @param requestBody request body (required)
   */
  @RequestLine("POST /fake/stringMap-reference")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  ApiResponse<Void> testStringMapReferenceWithHttpInfo(@javax.annotation.Nonnull Map<String, String> requestBody);


}
