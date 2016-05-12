package io.swagger.client.api;

import io.swagger.client.ApiClient;

import java.math.BigDecimal;
import java.util.Date;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import feign.*;

@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaClientCodegen", date = "2016-05-06T18:35:05.435+08:00")
public interface FakeApi extends ApiClient.Api {


  /**
   * Fake endpoint for testing various parameters
   * Fake endpoint for testing various parameters
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
   * @return void
   */
  @RequestLine("POST /fake")
  @Headers({
    "Content-type: application/x-www-form-urlencoded",
    "Accepts: application/json",
  })
  void testEndpointParameters(@Param("number") BigDecimal number, @Param("_double") Double _double, @Param("string") String string, @Param("_byte") byte[] _byte, @Param("integer") Integer integer, @Param("int32") Integer int32, @Param("int64") Long int64, @Param("_float") Float _float, @Param("binary") byte[] binary, @Param("date") Date date, @Param("dateTime") Date dateTime, @Param("password") String password);
}
