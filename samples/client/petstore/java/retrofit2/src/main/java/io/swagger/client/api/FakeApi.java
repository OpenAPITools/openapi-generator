package io.swagger.client.api;

import io.swagger.client.CollectionFormats.*;


import retrofit2.Call;
import retrofit2.http.*;

import okhttp3.RequestBody;

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
   * 
   * @param body client model (required)
   * @return Call&lt;Client&gt;
   */
  
  @PATCH("fake")
  Call<Client> testClientModel(
    @Body Client body
  );

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
   * @return Call&lt;Void&gt;
   */
  
  @FormUrlEncoded
  @POST("fake")
  Call<Void> testEndpointParameters(
    @Field("number") BigDecimal number, @Field("double") Double _double, @Field("pattern_without_delimiter") String patternWithoutDelimiter, @Field("byte") byte[] _byte, @Field("integer") Integer integer, @Field("int32") Integer int32, @Field("int64") Long int64, @Field("float") Float _float, @Field("string") String string, @Field("binary") byte[] binary, @Field("date") LocalDate date, @Field("dateTime") DateTime dateTime, @Field("password") String password
  );

  /**
   * To test enum parameters
   * 
   * @param enumFormStringArray Form parameter enum test (string array) (optional)
   * @param enumFormString Form parameter enum test (string) (optional, default to -efg)
   * @param enumHeaderStringArray Header parameter enum test (string array) (optional)
   * @param enumHeaderString Header parameter enum test (string) (optional, default to -efg)
   * @param enumQueryStringArray Query parameter enum test (string array) (optional)
   * @param enumQueryString Query parameter enum test (string) (optional, default to -efg)
   * @param enumQueryInteger Query parameter enum test (double) (optional)
   * @param enumQueryDouble Query parameter enum test (double) (optional)
   * @return Call&lt;Void&gt;
   */
  
  @FormUrlEncoded
  @GET("fake")
  Call<Void> testEnumParameters(
    @Field("enum_form_string_array") List<String> enumFormStringArray, @Field("enum_form_string") String enumFormString, @Header("enum_header_string_array") List<String> enumHeaderStringArray, @Header("enum_header_string") String enumHeaderString, @Query("enum_query_string_array") CSVParams enumQueryStringArray, @Query("enum_query_string") String enumQueryString, @Query("enum_query_integer") BigDecimal enumQueryInteger, @Field("enum_query_double") Double enumQueryDouble
  );

}
