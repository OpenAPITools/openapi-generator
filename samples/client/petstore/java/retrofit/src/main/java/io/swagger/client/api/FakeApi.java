package io.swagger.client.api;

import io.swagger.client.CollectionFormats.*;

import retrofit.Callback;
import retrofit.http.*;
import retrofit.mime.*;

import io.swagger.client.model.Client;
import org.joda.time.LocalDate;
import org.joda.time.DateTime;
import java.math.BigDecimal;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public interface FakeApi {
  /**
   * To test \&quot;client\&quot; model
   * Sync method
   * 
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
   * @return void
   */
  
  @PATCH("/fake")
  void testClientModel(
    @retrofit.http.Body Client body, Callback<Client> cb
  );
  /**
   * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
   * Sync method
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
   * @return Void
   */
  
  @retrofit.http.FormUrlEncoded
  @POST("/fake")
  Void testEndpointParameters(
    @retrofit.http.Field("number") BigDecimal number, @retrofit.http.Field("double") Double _double, @retrofit.http.Field("pattern_without_delimiter") String patternWithoutDelimiter, @retrofit.http.Field("byte") byte[] _byte, @retrofit.http.Field("integer") Integer integer, @retrofit.http.Field("int32") Integer int32, @retrofit.http.Field("int64") Long int64, @retrofit.http.Field("float") Float _float, @retrofit.http.Field("string") String string, @retrofit.http.Field("binary") byte[] binary, @retrofit.http.Field("date") LocalDate date, @retrofit.http.Field("dateTime") DateTime dateTime, @retrofit.http.Field("password") String password, @retrofit.http.Field("callback") String paramCallback
  );

  /**
   * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
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
   * @return void
   */
  
  @retrofit.http.FormUrlEncoded
  @POST("/fake")
  void testEndpointParameters(
    @retrofit.http.Field("number") BigDecimal number, @retrofit.http.Field("double") Double _double, @retrofit.http.Field("pattern_without_delimiter") String patternWithoutDelimiter, @retrofit.http.Field("byte") byte[] _byte, @retrofit.http.Field("integer") Integer integer, @retrofit.http.Field("int32") Integer int32, @retrofit.http.Field("int64") Long int64, @retrofit.http.Field("float") Float _float, @retrofit.http.Field("string") String string, @retrofit.http.Field("binary") byte[] binary, @retrofit.http.Field("date") LocalDate date, @retrofit.http.Field("dateTime") DateTime dateTime, @retrofit.http.Field("password") String password, @retrofit.http.Field("callback") String paramCallback, Callback<Void> cb
  );
  /**
   * To test enum parameters
   * Sync method
   * 
   * @param enumFormStringArray Form parameter enum test (string array) (optional)
   * @param enumFormString Form parameter enum test (string) (optional, default to -efg)
   * @param enumHeaderStringArray Header parameter enum test (string array) (optional)
   * @param enumHeaderString Header parameter enum test (string) (optional, default to -efg)
   * @param enumQueryStringArray Query parameter enum test (string array) (optional)
   * @param enumQueryString Query parameter enum test (string) (optional, default to -efg)
   * @param enumQueryInteger Query parameter enum test (double) (optional)
   * @param enumQueryDouble Query parameter enum test (double) (optional)
   * @return Void
   */
  
  @retrofit.http.FormUrlEncoded
  @GET("/fake")
  Void testEnumParameters(
    @retrofit.http.Field("enum_form_string_array") List<String> enumFormStringArray, @retrofit.http.Field("enum_form_string") String enumFormString, @retrofit.http.Header("enum_header_string_array") List<String> enumHeaderStringArray, @retrofit.http.Header("enum_header_string") String enumHeaderString, @retrofit.http.Query("enum_query_string_array") CSVParams enumQueryStringArray, @retrofit.http.Query("enum_query_string") String enumQueryString, @retrofit.http.Query("enum_query_integer") BigDecimal enumQueryInteger, @retrofit.http.Field("enum_query_double") Double enumQueryDouble
  );

  /**
   * To test enum parameters
   * Async method
   * @param enumFormStringArray Form parameter enum test (string array) (optional)
   * @param enumFormString Form parameter enum test (string) (optional, default to -efg)
   * @param enumHeaderStringArray Header parameter enum test (string array) (optional)
   * @param enumHeaderString Header parameter enum test (string) (optional, default to -efg)
   * @param enumQueryStringArray Query parameter enum test (string array) (optional)
   * @param enumQueryString Query parameter enum test (string) (optional, default to -efg)
   * @param enumQueryInteger Query parameter enum test (double) (optional)
   * @param enumQueryDouble Query parameter enum test (double) (optional)
   * @param cb callback method
   * @return void
   */
  
  @retrofit.http.FormUrlEncoded
  @GET("/fake")
  void testEnumParameters(
    @retrofit.http.Field("enum_form_string_array") List<String> enumFormStringArray, @retrofit.http.Field("enum_form_string") String enumFormString, @retrofit.http.Header("enum_header_string_array") List<String> enumHeaderStringArray, @retrofit.http.Header("enum_header_string") String enumHeaderString, @retrofit.http.Query("enum_query_string_array") CSVParams enumQueryStringArray, @retrofit.http.Query("enum_query_string") String enumQueryString, @retrofit.http.Query("enum_query_integer") BigDecimal enumQueryInteger, @retrofit.http.Field("enum_query_double") Double enumQueryDouble, Callback<Void> cb
  );
}
