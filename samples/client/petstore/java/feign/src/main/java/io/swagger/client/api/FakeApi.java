package io.swagger.client.api;

import io.swagger.client.ApiClient;
import io.swagger.client.EncodingUtils;

import java.math.BigDecimal;
import io.swagger.client.model.Client;
import org.joda.time.DateTime;
import org.joda.time.LocalDate;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import feign.*;


public interface FakeApi extends ApiClient.Api {


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
  Client testClientModel(Client body);

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
    "Content-Type: application/xml; charset&#x3D;utf-8",
    "Accept: application/xml; charset&#x3D;utf-8,application/json; charset&#x3D;utf-8",
  })
  void testEndpointParameters(@Param("number") BigDecimal number, @Param("_double") Double _double, @Param("patternWithoutDelimiter") String patternWithoutDelimiter, @Param("_byte") byte[] _byte, @Param("integer") Integer integer, @Param("int32") Integer int32, @Param("int64") Long int64, @Param("_float") Float _float, @Param("string") String string, @Param("binary") byte[] binary, @Param("date") LocalDate date, @Param("dateTime") DateTime dateTime, @Param("password") String password, @Param("paramCallback") String paramCallback);

  /**
   * To test enum parameters
   * To test enum parameters
    * @param enumFormStringArray Form parameter enum test (string array) (optional)
    * @param enumFormString Form parameter enum test (string) (optional, default to -efg)
    * @param enumHeaderStringArray Header parameter enum test (string array) (optional)
    * @param enumHeaderString Header parameter enum test (string) (optional, default to -efg)
    * @param enumQueryStringArray Query parameter enum test (string array) (optional)
    * @param enumQueryString Query parameter enum test (string) (optional, default to -efg)
    * @param enumQueryInteger Query parameter enum test (double) (optional)
    * @param enumQueryDouble Query parameter enum test (double) (optional)
   */
  @RequestLine("GET /fake?enum_query_string_array={enumQueryStringArray}&enum_query_string={enumQueryString}&enum_query_integer={enumQueryInteger}")
  @Headers({
    "Content-Type: */*",
    "Accept: */*",
    "enum_header_string_array: {enumHeaderStringArray}",
    
    "enum_header_string: {enumHeaderString}"
  })
  void testEnumParameters(@Param("enumFormStringArray") List<String> enumFormStringArray, @Param("enumFormString") String enumFormString, @Param("enumHeaderStringArray") List<String> enumHeaderStringArray, @Param("enumHeaderString") String enumHeaderString, @Param("enumQueryStringArray") List<String> enumQueryStringArray, @Param("enumQueryString") String enumQueryString, @Param("enumQueryInteger") Integer enumQueryInteger, @Param("enumQueryDouble") Double enumQueryDouble);

  /**
   * To test enum parameters
   * To test enum parameters
   * Note, this is equivalent to the other <code>testEnumParameters</code> method,
   * but with the query parameters collected into a single Map parameter. This
   * is convenient for services with optional query parameters, especially when
   * used with the {@link TestEnumParametersQueryParams} class that allows for
   * building up this map in a fluent style.
   * @param enumFormStringArray Form parameter enum test (string array) (optional)
   * @param enumFormString Form parameter enum test (string) (optional, default to -efg)
   * @param enumHeaderStringArray Header parameter enum test (string array) (optional)
   * @param enumHeaderString Header parameter enum test (string) (optional, default to -efg)
   * @param enumQueryDouble Query parameter enum test (double) (optional)
   * @param queryParams Map of query parameters as name-value pairs
   *   <p>The following elements may be specified in the query map:</p>
   *   <ul>
   *   <li>enumQueryStringArray - Query parameter enum test (string array) (optional)</li>
   *   <li>enumQueryString - Query parameter enum test (string) (optional, default to -efg)</li>
   *   <li>enumQueryInteger - Query parameter enum test (double) (optional)</li>
   *   </ul>
   */
  @RequestLine("GET /fake?enum_query_string_array={enumQueryStringArray}&enum_query_string={enumQueryString}&enum_query_integer={enumQueryInteger}")
  @Headers({
  "Content-Type: */*",
  "Accept: */*",
      "enum_header_string_array: {enumHeaderStringArray}",
      
      "enum_header_string: {enumHeaderString}"
  })
  void testEnumParameters(@Param("enumFormStringArray") List<String> enumFormStringArray, @Param("enumFormString") String enumFormString, @Param("enumHeaderStringArray") List<String> enumHeaderStringArray, @Param("enumHeaderString") String enumHeaderString, @Param("enumQueryDouble") Double enumQueryDouble, @QueryMap(encoded=true) Map<String, Object> queryParams);

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
  }
}
