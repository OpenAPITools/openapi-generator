package org.openapitools.client.api;

import org.openapitools.client.CollectionFormats.*;

import retrofit.Callback;
import retrofit.http.*;
import retrofit.mime.*;

import java.math.BigDecimal;
import org.openapitools.client.model.Client;
import org.joda.time.DateTime;
import java.io.File;
import org.openapitools.client.model.FileSchemaTestClass;
import org.joda.time.LocalDate;
import org.openapitools.client.model.OuterComposite;
import org.openapitools.client.model.User;
import org.openapitools.client.model.XmlItem;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public interface FakeApi {
  /**
   * creates an XmlItem
   * Sync method
   * this route creates an XmlItem
   * @param xmlItem XmlItem Body (required)
   * @return Void
   */
  
  @POST("/fake/create_xml_item")
  Void createXmlItem(
    @retrofit.http.Body XmlItem xmlItem
  );

  /**
   * creates an XmlItem
   * Async method
   * @param xmlItem XmlItem Body (required)
   * @param cb callback method
   */
  
  @POST("/fake/create_xml_item")
  void createXmlItem(
    @retrofit.http.Body XmlItem xmlItem, Callback<Void> cb
  );
  /**
   * 
   * Sync method
   * Test serialization of outer boolean types
   * @param body Input boolean as post body (optional)
   * @return Boolean
   */
  
  @POST("/fake/outer/boolean")
  Boolean fakeOuterBooleanSerialize(
    @retrofit.http.Body Boolean body
  );

  /**
   * 
   * Async method
   * @param body Input boolean as post body (optional)
   * @param cb callback method
   */
  
  @POST("/fake/outer/boolean")
  void fakeOuterBooleanSerialize(
    @retrofit.http.Body Boolean body, Callback<Boolean> cb
  );
  /**
   * 
   * Sync method
   * Test serialization of object with outer number type
   * @param body Input composite as post body (optional)
   * @return OuterComposite
   */
  
  @POST("/fake/outer/composite")
  OuterComposite fakeOuterCompositeSerialize(
    @retrofit.http.Body OuterComposite body
  );

  /**
   * 
   * Async method
   * @param body Input composite as post body (optional)
   * @param cb callback method
   */
  
  @POST("/fake/outer/composite")
  void fakeOuterCompositeSerialize(
    @retrofit.http.Body OuterComposite body, Callback<OuterComposite> cb
  );
  /**
   * 
   * Sync method
   * Test serialization of outer number types
   * @param body Input number as post body (optional)
   * @return BigDecimal
   */
  
  @POST("/fake/outer/number")
  BigDecimal fakeOuterNumberSerialize(
    @retrofit.http.Body BigDecimal body
  );

  /**
   * 
   * Async method
   * @param body Input number as post body (optional)
   * @param cb callback method
   */
  
  @POST("/fake/outer/number")
  void fakeOuterNumberSerialize(
    @retrofit.http.Body BigDecimal body, Callback<BigDecimal> cb
  );
  /**
   * 
   * Sync method
   * Test serialization of outer string types
   * @param body Input string as post body (optional)
   * @return String
   */
  
  @POST("/fake/outer/string")
  String fakeOuterStringSerialize(
    @retrofit.http.Body String body
  );

  /**
   * 
   * Async method
   * @param body Input string as post body (optional)
   * @param cb callback method
   */
  
  @POST("/fake/outer/string")
  void fakeOuterStringSerialize(
    @retrofit.http.Body String body, Callback<String> cb
  );
  /**
   * 
   * Sync method
   * For this test, the body for this request much reference a schema named &#x60;File&#x60;.
   * @param body  (required)
   * @return Void
   */
  
  @PUT("/fake/body-with-file-schema")
  Void testBodyWithFileSchema(
    @retrofit.http.Body FileSchemaTestClass body
  );

  /**
   * 
   * Async method
   * @param body  (required)
   * @param cb callback method
   */
  
  @PUT("/fake/body-with-file-schema")
  void testBodyWithFileSchema(
    @retrofit.http.Body FileSchemaTestClass body, Callback<Void> cb
  );
  /**
   * 
   * Sync method
   * 
   * @param query  (required)
   * @param body  (required)
   * @return Void
   */
  
  @PUT("/fake/body-with-query-params")
  Void testBodyWithQueryParams(
    @retrofit.http.Query("query") String query, @retrofit.http.Body User body
  );

  /**
   * 
   * Async method
   * @param query  (required)
   * @param body  (required)
   * @param cb callback method
   */
  
  @PUT("/fake/body-with-query-params")
  void testBodyWithQueryParams(
    @retrofit.http.Query("query") String query, @retrofit.http.Body User body, Callback<Void> cb
  );
  /**
   * To test \&quot;client\&quot; model
   * Sync method
   * To test \&quot;client\&quot; model
   * @param body client model (required)
   * @return Client
   */
  
  @PATCH("/fake")
  Client testClientModel(
    @retrofit.http.Body Client body
  );

  /**
   * To test \&quot;client\&quot; model
   * Async method
   * @param body client model (required)
   * @param cb callback method
   */
  
  @PATCH("/fake")
  void testClientModel(
    @retrofit.http.Body Client body, Callback<Client> cb
  );
  /**
   * Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
   * Sync method
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
   * @return Void
   */
  
  @retrofit.http.FormUrlEncoded
  @POST("/fake")
  Void testEndpointParameters(
    @retrofit.http.Field("number") BigDecimal number, @retrofit.http.Field("double") Double _double, @retrofit.http.Field("pattern_without_delimiter") String patternWithoutDelimiter, @retrofit.http.Field("byte") byte[] _byte, @retrofit.http.Field("integer") Integer integer, @retrofit.http.Field("int32") Integer int32, @retrofit.http.Field("int64") Long int64, @retrofit.http.Field("float") Float _float, @retrofit.http.Field("string") String string, @retrofit.http.Field("binary") TypedFile binary, @retrofit.http.Field("date") LocalDate date, @retrofit.http.Field("dateTime") DateTime dateTime, @retrofit.http.Field("password") String password, @retrofit.http.Field("callback") String paramCallback
  );

  /**
   * Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
   * Async method
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
   * @param cb callback method
   */
  
  @retrofit.http.FormUrlEncoded
  @POST("/fake")
  void testEndpointParameters(
    @retrofit.http.Field("number") BigDecimal number, @retrofit.http.Field("double") Double _double, @retrofit.http.Field("pattern_without_delimiter") String patternWithoutDelimiter, @retrofit.http.Field("byte") byte[] _byte, @retrofit.http.Field("integer") Integer integer, @retrofit.http.Field("int32") Integer int32, @retrofit.http.Field("int64") Long int64, @retrofit.http.Field("float") Float _float, @retrofit.http.Field("string") String string, @retrofit.http.Field("binary") TypedFile binary, @retrofit.http.Field("date") LocalDate date, @retrofit.http.Field("dateTime") DateTime dateTime, @retrofit.http.Field("password") String password, @retrofit.http.Field("callback") String paramCallback, Callback<Void> cb
  );
  /**
   * To test enum parameters
   * Sync method
   * To test enum parameters
   * @param enumHeaderStringArray Header parameter enum test (string array) (optional, default to new ArrayList&lt;String&gt;())
   * @param enumHeaderString Header parameter enum test (string) (optional, default to -efg)
   * @param enumQueryStringArray Query parameter enum test (string array) (optional, default to new ArrayList&lt;String&gt;())
   * @param enumQueryString Query parameter enum test (string) (optional, default to -efg)
   * @param enumQueryInteger Query parameter enum test (double) (optional)
   * @param enumQueryDouble Query parameter enum test (double) (optional)
   * @param enumFormStringArray Form parameter enum test (string array) (optional, default to $)
   * @param enumFormString Form parameter enum test (string) (optional, default to -efg)
   * @return Void
   */
  
  @retrofit.http.FormUrlEncoded
  @GET("/fake")
  Void testEnumParameters(
    @retrofit.http.Header("enum_header_string_array") List<String> enumHeaderStringArray, @retrofit.http.Header("enum_header_string") String enumHeaderString, @retrofit.http.Query("enum_query_string_array") CSVParams enumQueryStringArray, @retrofit.http.Query("enum_query_string") String enumQueryString, @retrofit.http.Query("enum_query_integer") Integer enumQueryInteger, @retrofit.http.Query("enum_query_double") Double enumQueryDouble, @retrofit.http.Field("enum_form_string_array") List<String> enumFormStringArray, @retrofit.http.Field("enum_form_string") String enumFormString
  );

  /**
   * To test enum parameters
   * Async method
   * @param enumHeaderStringArray Header parameter enum test (string array) (optional, default to new ArrayList&lt;String&gt;())
   * @param enumHeaderString Header parameter enum test (string) (optional, default to -efg)
   * @param enumQueryStringArray Query parameter enum test (string array) (optional, default to new ArrayList&lt;String&gt;())
   * @param enumQueryString Query parameter enum test (string) (optional, default to -efg)
   * @param enumQueryInteger Query parameter enum test (double) (optional)
   * @param enumQueryDouble Query parameter enum test (double) (optional)
   * @param enumFormStringArray Form parameter enum test (string array) (optional, default to $)
   * @param enumFormString Form parameter enum test (string) (optional, default to -efg)
   * @param cb callback method
   */
  
  @retrofit.http.FormUrlEncoded
  @GET("/fake")
  void testEnumParameters(
    @retrofit.http.Header("enum_header_string_array") List<String> enumHeaderStringArray, @retrofit.http.Header("enum_header_string") String enumHeaderString, @retrofit.http.Query("enum_query_string_array") CSVParams enumQueryStringArray, @retrofit.http.Query("enum_query_string") String enumQueryString, @retrofit.http.Query("enum_query_integer") Integer enumQueryInteger, @retrofit.http.Query("enum_query_double") Double enumQueryDouble, @retrofit.http.Field("enum_form_string_array") List<String> enumFormStringArray, @retrofit.http.Field("enum_form_string") String enumFormString, Callback<Void> cb
  );
  /**
   * Fake endpoint to test group parameters (optional)
   * Sync method
   * Fake endpoint to test group parameters (optional)
   * @param requiredStringGroup Required String in group parameters (required)
   * @param requiredBooleanGroup Required Boolean in group parameters (required)
   * @param requiredInt64Group Required Integer in group parameters (required)
   * @param stringGroup String in group parameters (optional)
   * @param booleanGroup Boolean in group parameters (optional)
   * @param int64Group Integer in group parameters (optional)
   * @return Void
   */
  
  @DELETE("/fake")
  Void testGroupParameters(
    @retrofit.http.Query("required_string_group") Integer requiredStringGroup, @retrofit.http.Header("required_boolean_group") Boolean requiredBooleanGroup, @retrofit.http.Query("required_int64_group") Long requiredInt64Group, @retrofit.http.Query("string_group") Integer stringGroup, @retrofit.http.Header("boolean_group") Boolean booleanGroup, @retrofit.http.Query("int64_group") Long int64Group
  );

  /**
   * Fake endpoint to test group parameters (optional)
   * Async method
   * @param requiredStringGroup Required String in group parameters (required)
   * @param requiredBooleanGroup Required Boolean in group parameters (required)
   * @param requiredInt64Group Required Integer in group parameters (required)
   * @param stringGroup String in group parameters (optional)
   * @param booleanGroup Boolean in group parameters (optional)
   * @param int64Group Integer in group parameters (optional)
   * @param cb callback method
   */
  
  @DELETE("/fake")
  void testGroupParameters(
    @retrofit.http.Query("required_string_group") Integer requiredStringGroup, @retrofit.http.Header("required_boolean_group") Boolean requiredBooleanGroup, @retrofit.http.Query("required_int64_group") Long requiredInt64Group, @retrofit.http.Query("string_group") Integer stringGroup, @retrofit.http.Header("boolean_group") Boolean booleanGroup, @retrofit.http.Query("int64_group") Long int64Group, Callback<Void> cb
  );
  /**
   * test inline additionalProperties
   * Sync method
   * 
   * @param param request body (required)
   * @return Void
   */
  
  @POST("/fake/inline-additionalProperties")
  Void testInlineAdditionalProperties(
    @retrofit.http.Body Map<String, String> param
  );

  /**
   * test inline additionalProperties
   * Async method
   * @param param request body (required)
   * @param cb callback method
   */
  
  @POST("/fake/inline-additionalProperties")
  void testInlineAdditionalProperties(
    @retrofit.http.Body Map<String, String> param, Callback<Void> cb
  );
  /**
   * test json serialization of form data
   * Sync method
   * 
   * @param param field1 (required)
   * @param param2 field2 (required)
   * @return Void
   */
  
  @retrofit.http.FormUrlEncoded
  @GET("/fake/jsonFormData")
  Void testJsonFormData(
    @retrofit.http.Field("param") String param, @retrofit.http.Field("param2") String param2
  );

  /**
   * test json serialization of form data
   * Async method
   * @param param field1 (required)
   * @param param2 field2 (required)
   * @param cb callback method
   */
  
  @retrofit.http.FormUrlEncoded
  @GET("/fake/jsonFormData")
  void testJsonFormData(
    @retrofit.http.Field("param") String param, @retrofit.http.Field("param2") String param2, Callback<Void> cb
  );
  /**
   * 
   * Sync method
   * To test the collection format in query parameters
   * @param pipe  (required)
   * @param ioutil  (required)
   * @param http  (required)
   * @param url  (required)
   * @param context  (required)
   * @return Void
   */
  
  @PUT("/fake/test-query-paramters")
  Void testQueryParameterCollectionFormat(
    @retrofit.http.Query("pipe") CSVParams pipe, @retrofit.http.Query("ioutil") CSVParams ioutil, @retrofit.http.Query("http") SPACEParams http, @retrofit.http.Query("url") CSVParams url, @retrofit.http.Query("context") List<String> context
  );

  /**
   * 
   * Async method
   * @param pipe  (required)
   * @param ioutil  (required)
   * @param http  (required)
   * @param url  (required)
   * @param context  (required)
   * @param cb callback method
   */
  
  @PUT("/fake/test-query-paramters")
  void testQueryParameterCollectionFormat(
    @retrofit.http.Query("pipe") CSVParams pipe, @retrofit.http.Query("ioutil") CSVParams ioutil, @retrofit.http.Query("http") SPACEParams http, @retrofit.http.Query("url") CSVParams url, @retrofit.http.Query("context") List<String> context, Callback<Void> cb
  );
}
