package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import org.openapitools.client.EncodingUtils;

import java.math.BigDecimal;
import org.openapitools.client.model.Client;
import java.io.File;
import org.openapitools.client.model.FileSchemaTestClass;
import org.threeten.bp.LocalDate;
import org.threeten.bp.OffsetDateTime;
import org.openapitools.client.model.OuterComposite;
import org.openapitools.client.model.User;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import feign.*;


public interface FakeApi extends ApiClient.Api {


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
  Boolean fakeOuterBooleanSerialize(Boolean body);

  /**
   * 
   * Test serialization of object with outer number type
    * @param outerComposite Input composite as post body (optional)
   * @return OuterComposite
   */
  @RequestLine("POST /fake/outer/composite")
  @Headers({
    "Content-Type: */*",
    "Accept: */*",
  })
  OuterComposite fakeOuterCompositeSerialize(OuterComposite outerComposite);

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
  BigDecimal fakeOuterNumberSerialize(BigDecimal body);

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
  String fakeOuterStringSerialize(String body);

  /**
   * 
   * For this test, the body for this request much reference a schema named &#x60;File&#x60;.
    * @param fileSchemaTestClass  (required)
   */
  @RequestLine("PUT /fake/body-with-file-schema")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  void testBodyWithFileSchema(FileSchemaTestClass fileSchemaTestClass);

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
  void testBodyWithQueryParams(@Param("query") String query, User user);

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
  void testBodyWithQueryParams(User user, @QueryMap(encoded=true) Map<String, Object> queryParams);

  /**
   * A convenience class for generating query parameters for the
   * <code>testBodyWithQueryParams</code> method in a fluent style.
   */
  public static class TestBodyWithQueryParamsQueryParams extends HashMap<String, Object> {
    public TestBodyWithQueryParamsQueryParams query(final String value) {
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
  Client testClientModel(Client client);

  /**
   * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
   * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
    * @param number None (required)
    * @param _double None (required)
    * @param patternWithoutDelimiter None (required)
    * @param _byte None (required)
    * @param integer None (optional, default to null)
    * @param int32 None (optional, default to null)
    * @param int64 None (optional, default to null)
    * @param _float None (optional, default to null)
    * @param string None (optional, default to null)
    * @param binary None (optional, default to null)
    * @param date None (optional, default to null)
    * @param dateTime None (optional, default to null)
    * @param password None (optional, default to null)
    * @param paramCallback None (optional, default to null)
   */
  @RequestLine("POST /fake")
  @Headers({
    "Content-Type: application/x-www-form-urlencoded",
    "Accept: application/json",
  })
  void testEndpointParameters(@Param("number") BigDecimal number, @Param("_double") Double _double, @Param("patternWithoutDelimiter") String patternWithoutDelimiter, @Param("_byte") byte[] _byte, @Param("integer") Integer integer, @Param("int32") Integer int32, @Param("int64") Long int64, @Param("_float") Float _float, @Param("string") String string, @Param("binary") File binary, @Param("date") LocalDate date, @Param("dateTime") OffsetDateTime dateTime, @Param("password") String password, @Param("paramCallback") String paramCallback);

  /**
   * To test enum parameters
   * To test enum parameters
    * @param enumHeaderStringArray Header parameter enum test (string array) (optional)
    * @param enumHeaderString Header parameter enum test (string) (optional, default to -efg)
    * @param enumQueryStringArray Query parameter enum test (string array) (optional)
    * @param enumQueryString Query parameter enum test (string) (optional, default to -efg)
    * @param enumQueryInteger Query parameter enum test (double) (optional)
    * @param enumQueryDouble Query parameter enum test (double) (optional)
    * @param enumFormStringArray Form parameter enum test (string array) (optional, default to $)
    * @param enumFormString Form parameter enum test (string) (optional, default to -efg)
   */
  @RequestLine("GET /fake?enum_query_string_array={enumQueryStringArray}&enum_query_string={enumQueryString}&enum_query_integer={enumQueryInteger}&enum_query_double={enumQueryDouble}")
  @Headers({
    "Content-Type: application/x-www-form-urlencoded",
    "Accept: application/json",
    "enum_header_string_array: {enumHeaderStringArray}",
    
    "enum_header_string: {enumHeaderString}"
  })
  void testEnumParameters(@Param("enumHeaderStringArray") List<String> enumHeaderStringArray, @Param("enumHeaderString") String enumHeaderString, @Param("enumQueryStringArray") List<String> enumQueryStringArray, @Param("enumQueryString") String enumQueryString, @Param("enumQueryInteger") Integer enumQueryInteger, @Param("enumQueryDouble") Double enumQueryDouble, @Param("enumFormStringArray") List<String> enumFormStringArray, @Param("enumFormString") String enumFormString);

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
   * @param enumFormStringArray Form parameter enum test (string array) (optional, default to $)
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
  void testEnumParameters(@Param("enumHeaderStringArray") List<String> enumHeaderStringArray, @Param("enumHeaderString") String enumHeaderString, @Param("enumFormStringArray") List<String> enumFormStringArray, @Param("enumFormString") String enumFormString, @QueryMap(encoded=true) Map<String, Object> queryParams);

  /**
   * A convenience class for generating query parameters for the
   * <code>testEnumParameters</code> method in a fluent style.
   */
  public static class TestEnumParametersQueryParams extends HashMap<String, Object> {
    public TestEnumParametersQueryParams enumQueryStringArray(final List<String> value) {
      put("enum_query_string_array", EncodingUtils.encodeCollection(value, "csv"));
      return this;
    }
    public TestEnumParametersQueryParams enumQueryString(final String value) {
      put("enum_query_string", EncodingUtils.encode(value));
      return this;
    }
    public TestEnumParametersQueryParams enumQueryInteger(final Integer value) {
      put("enum_query_integer", EncodingUtils.encode(value));
      return this;
    }
    public TestEnumParametersQueryParams enumQueryDouble(final Double value) {
      put("enum_query_double", EncodingUtils.encode(value));
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
  void testInlineAdditionalProperties(Map<String, String> requestBody);

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
  void testJsonFormData(@Param("param") String param, @Param("param2") String param2);
}
