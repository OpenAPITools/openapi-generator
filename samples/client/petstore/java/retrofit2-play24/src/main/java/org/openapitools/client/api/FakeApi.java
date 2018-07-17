package org.openapitools.client.api;

import org.openapitools.client.CollectionFormats.*;



import retrofit2.Call;
import retrofit2.http.*;

import okhttp3.RequestBody;
import okhttp3.ResponseBody;
import okhttp3.MultipartBody;

import java.math.BigDecimal;
import org.openapitools.client.model.Client;
import java.io.File;
import org.openapitools.client.model.FileSchemaTestClass;
import java.time.LocalDate;
import java.time.OffsetDateTime;
import org.openapitools.client.model.OuterComposite;
import org.openapitools.client.model.User;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import play.libs.F;
import retrofit2.Response;

public interface FakeApi {
  /**
   * 
   * Test serialization of outer boolean types
   * @param body Input boolean as post body (optional)
   * @return Call&lt;Boolean&gt;
   */
  @POST("fake/outer/boolean")
  F.Promise<Response<Boolean>> fakeOuterBooleanSerialize(
    @retrofit2.http.Body Boolean body
  );

  /**
   * 
   * Test serialization of object with outer number type
   * @param outerComposite Input composite as post body (optional)
   * @return Call&lt;OuterComposite&gt;
   */
  @POST("fake/outer/composite")
  F.Promise<Response<OuterComposite>> fakeOuterCompositeSerialize(
    @retrofit2.http.Body OuterComposite outerComposite
  );

  /**
   * 
   * Test serialization of outer number types
   * @param body Input number as post body (optional)
   * @return Call&lt;BigDecimal&gt;
   */
  @POST("fake/outer/number")
  F.Promise<Response<BigDecimal>> fakeOuterNumberSerialize(
    @retrofit2.http.Body BigDecimal body
  );

  /**
   * 
   * Test serialization of outer string types
   * @param body Input string as post body (optional)
   * @return Call&lt;String&gt;
   */
  @POST("fake/outer/string")
  F.Promise<Response<String>> fakeOuterStringSerialize(
    @retrofit2.http.Body String body
  );

  /**
   * 
   * For this test, the body for this request much reference a schema named &#x60;File&#x60;.
   * @param fileSchemaTestClass  (required)
   * @return Call&lt;Void&gt;
   */
  @Headers({
    "Content-Type:application/json"
  })
  @PUT("fake/body-with-file-schema")
  F.Promise<Response<Void>> testBodyWithFileSchema(
    @retrofit2.http.Body FileSchemaTestClass fileSchemaTestClass
  );

  /**
   * 
   * 
   * @param query  (required)
   * @param user  (required)
   * @return Call&lt;Void&gt;
   */
  @Headers({
    "Content-Type:application/json"
  })
  @PUT("fake/body-with-query-params")
  F.Promise<Response<Void>> testBodyWithQueryParams(
    @retrofit2.http.Query("query") String query, @retrofit2.http.Body User user
  );

  /**
   * To test \&quot;client\&quot; model
   * To test \&quot;client\&quot; model
   * @param client client model (required)
   * @return Call&lt;Client&gt;
   */
  @Headers({
    "Content-Type:application/json"
  })
  @PATCH("fake")
  F.Promise<Response<Client>> testClientModel(
    @retrofit2.http.Body Client client
  );

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
   * @return Call&lt;Void&gt;
   */
  @retrofit2.http.FormUrlEncoded
  @POST("fake")
  F.Promise<Response<Void>> testEndpointParameters(
    @retrofit2.http.Field("number") BigDecimal number, @retrofit2.http.Field("double") Double _double, @retrofit2.http.Field("pattern_without_delimiter") String patternWithoutDelimiter, @retrofit2.http.Field("byte") byte[] _byte, @retrofit2.http.Field("integer") Integer integer, @retrofit2.http.Field("int32") Integer int32, @retrofit2.http.Field("int64") Long int64, @retrofit2.http.Field("float") Float _float, @retrofit2.http.Field("string") String string, @retrofit2.http.Field("binary") MultipartBody.Part binary, @retrofit2.http.Field("date") LocalDate date, @retrofit2.http.Field("dateTime") OffsetDateTime dateTime, @retrofit2.http.Field("password") String password, @retrofit2.http.Field("callback") String paramCallback
  );

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
   * @return Call&lt;Void&gt;
   */
  @retrofit2.http.FormUrlEncoded
  @GET("fake")
  F.Promise<Response<Void>> testEnumParameters(
    @retrofit2.http.Header("enum_header_string_array") List<String> enumHeaderStringArray, @retrofit2.http.Header("enum_header_string") String enumHeaderString, @retrofit2.http.Query("enum_query_string_array") CSVParams enumQueryStringArray, @retrofit2.http.Query("enum_query_string") String enumQueryString, @retrofit2.http.Query("enum_query_integer") Integer enumQueryInteger, @retrofit2.http.Query("enum_query_double") Double enumQueryDouble, @retrofit2.http.Field("enum_form_string_array") List<String> enumFormStringArray, @retrofit2.http.Field("enum_form_string") String enumFormString
  );

  /**
   * test inline additionalProperties
   * 
   * @param requestBody request body (required)
   * @return Call&lt;Void&gt;
   */
  @Headers({
    "Content-Type:application/json"
  })
  @POST("fake/inline-additionalProperties")
  F.Promise<Response<Void>> testInlineAdditionalProperties(
    @retrofit2.http.Body Map<String, String> requestBody
  );

  /**
   * test json serialization of form data
   * 
   * @param param field1 (required)
   * @param param2 field2 (required)
   * @return Call&lt;Void&gt;
   */
  @retrofit2.http.FormUrlEncoded
  @GET("fake/jsonFormData")
  F.Promise<Response<Void>> testJsonFormData(
    @retrofit2.http.Field("param") String param, @retrofit2.http.Field("param2") String param2
  );

}
