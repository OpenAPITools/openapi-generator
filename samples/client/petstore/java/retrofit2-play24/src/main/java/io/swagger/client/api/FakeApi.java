package io.swagger.client.api;

import io.swagger.client.CollectionFormats.*;



import retrofit2.Call;
import retrofit2.http.*;

import okhttp3.RequestBody;

import java.math.BigDecimal;
import io.swagger.client.model.Client;
import org.joda.time.DateTime;
import org.joda.time.LocalDate;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import play.libs.F;
import retrofit2.Response;

public interface FakeApi {
  /**
   * To test \&quot;client\&quot; model
   * To test \&quot;client\&quot; model
   * @param body client model (required)
   * @return Call&lt;Client&gt;
   */
  
  @Headers({
  	"Content-Type:application/json" 
  })
  @PATCH("fake")
  F.Promise<Response<Client>> testClientModel(
    @retrofit2.http.Body Client body
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
   * @param paramCallback None (optional)
   * @return Call&lt;Void&gt;
   */
  
  @retrofit2.http.FormUrlEncoded
  @POST("fake")
  F.Promise<Response<Void>> testEndpointParameters(
    @retrofit2.http.Field("number") BigDecimal number, @retrofit2.http.Field("double") Double _double, @retrofit2.http.Field("pattern_without_delimiter") String patternWithoutDelimiter, @retrofit2.http.Field("byte") byte[] _byte, @retrofit2.http.Field("integer") Integer integer, @retrofit2.http.Field("int32") Integer int32, @retrofit2.http.Field("int64") Long int64, @retrofit2.http.Field("float") Float _float, @retrofit2.http.Field("string") String string, @retrofit2.http.Field("binary") byte[] binary, @retrofit2.http.Field("date") LocalDate date, @retrofit2.http.Field("dateTime") DateTime dateTime, @retrofit2.http.Field("password") String password, @retrofit2.http.Field("callback") String paramCallback
  );

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
   * @return Call&lt;Void&gt;
   */
  
  @retrofit2.http.FormUrlEncoded
  @GET("fake")
  F.Promise<Response<Void>> testEnumParameters(
    @retrofit2.http.Field("enum_form_string_array") List<String> enumFormStringArray, @retrofit2.http.Field("enum_form_string") String enumFormString, @retrofit2.http.Header("enum_header_string_array") List<String> enumHeaderStringArray, @retrofit2.http.Header("enum_header_string") String enumHeaderString, @retrofit2.http.Query("enum_query_string_array") CSVParams enumQueryStringArray, @retrofit2.http.Query("enum_query_string") String enumQueryString, @retrofit2.http.Query("enum_query_integer") Integer enumQueryInteger, @retrofit2.http.Field("enum_query_double") Double enumQueryDouble
  );

}
