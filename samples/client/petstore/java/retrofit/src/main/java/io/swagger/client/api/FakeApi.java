package io.swagger.client.api;

import io.swagger.client.CollectionFormats.*;

import retrofit.Callback;
import retrofit.http.*;
import retrofit.mime.*;

import io.swagger.client.model.Client;
import org.joda.time.LocalDate;
import java.math.BigDecimal;
import org.joda.time.DateTime;

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
    @Body Client body
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
    @Body Client body, Callback<Client> cb
  );
  /**
   * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
   * Sync method
   * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
   * @param number None (required)
   * @param _double None (required)
   * @param string None (required)
   * @param _byte None (required)
   * @param integer None (optional)
   * @param int32 None (optional)
   * @param int64 None (optional)
   * @param _float None (optional)
   * @param binary None (optional)
   * @param date None (optional)
   * @param dateTime None (optional)
   * @param password None (optional)
   * @return Void
   */
  
  @FormUrlEncoded
  @POST("/fake")
  Void testEndpointParameters(
    @Field("number") BigDecimal number, @Field("double") Double _double, @Field("string") String string, @Field("byte") byte[] _byte, @Field("integer") Integer integer, @Field("int32") Integer int32, @Field("int64") Long int64, @Field("float") Float _float, @Field("binary") byte[] binary, @Field("date") LocalDate date, @Field("dateTime") DateTime dateTime, @Field("password") String password
  );

  /**
   * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
   * Async method
   * @param number None (required)
   * @param _double None (required)
   * @param string None (required)
   * @param _byte None (required)
   * @param integer None (optional)
   * @param int32 None (optional)
   * @param int64 None (optional)
   * @param _float None (optional)
   * @param binary None (optional)
   * @param date None (optional)
   * @param dateTime None (optional)
   * @param password None (optional)
   * @param cb callback method
   * @return void
   */
  
  @FormUrlEncoded
  @POST("/fake")
  void testEndpointParameters(
    @Field("number") BigDecimal number, @Field("double") Double _double, @Field("string") String string, @Field("byte") byte[] _byte, @Field("integer") Integer integer, @Field("int32") Integer int32, @Field("int64") Long int64, @Field("float") Float _float, @Field("binary") byte[] binary, @Field("date") LocalDate date, @Field("dateTime") DateTime dateTime, @Field("password") String password, Callback<Void> cb
  );
  /**
   * To test enum query parameters
   * Sync method
   * 
   * @param enumQueryString Query parameter enum test (string) (optional, default to -efg)
   * @param enumQueryInteger Query parameter enum test (double) (optional)
   * @param enumQueryDouble Query parameter enum test (double) (optional)
   * @return Void
   */
  
  @FormUrlEncoded
  @GET("/fake")
  Void testEnumQueryParameters(
    @Field("enum_query_string") String enumQueryString, @Query("enum_query_integer") BigDecimal enumQueryInteger, @Field("enum_query_double") Double enumQueryDouble
  );

  /**
   * To test enum query parameters
   * Async method
   * @param enumQueryString Query parameter enum test (string) (optional, default to -efg)
   * @param enumQueryInteger Query parameter enum test (double) (optional)
   * @param enumQueryDouble Query parameter enum test (double) (optional)
   * @param cb callback method
   * @return void
   */
  
  @FormUrlEncoded
  @GET("/fake")
  void testEnumQueryParameters(
    @Field("enum_query_string") String enumQueryString, @Query("enum_query_integer") BigDecimal enumQueryInteger, @Field("enum_query_double") Double enumQueryDouble, Callback<Void> cb
  );
}
