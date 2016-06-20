package io.swagger.client.api;

import io.swagger.client.CollectionFormats.*;

import rx.Observable;

import retrofit2.http.*;

import okhttp3.RequestBody;

import org.joda.time.LocalDate;
import org.joda.time.DateTime;
import java.math.BigDecimal;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public interface FakeApi {
  /**
   * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
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
   * @return Call<Void>
   */
  
  @FormUrlEncoded
  @POST("fake")
  Observable<Void> testEndpointParameters(
    @Field("number") BigDecimal number, @Field("double") Double _double, @Field("string") String string, @Field("byte") byte[] _byte, @Field("integer") Integer integer, @Field("int32") Integer int32, @Field("int64") Long int64, @Field("float") Float _float, @Field("binary") byte[] binary, @Field("date") LocalDate date, @Field("dateTime") DateTime dateTime, @Field("password") String password
  );

}
