package io.swagger.client.api;

import io.swagger.client.ApiCallback;
import io.swagger.client.ApiClient;
import io.swagger.client.ApiException;
import io.swagger.client.ApiResponse;
import io.swagger.client.Configuration;
import io.swagger.client.Pair;
import io.swagger.client.ProgressRequestBody;
import io.swagger.client.ProgressResponseBody;

import com.google.gson.reflect.TypeToken;

import com.squareup.okhttp.Call;
import com.squareup.okhttp.Interceptor;
import com.squareup.okhttp.Response;

import java.io.IOException;

import java.math.BigDecimal;
import java.util.Date;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class FakeApi {
  private ApiClient apiClient;

  public FakeApi() {
    this(Configuration.getDefaultApiClient());
  }

  public FakeApi(ApiClient apiClient) {
    this.apiClient = apiClient;
  }

  public ApiClient getApiClient() {
    return apiClient;
  }

  public void setApiClient(ApiClient apiClient) {
    this.apiClient = apiClient;
  }

  /* Build call for testEndpointParameters */
  private Call testEndpointParametersCall(BigDecimal number, Double _double, String string, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, byte[] binary, Date date, Date dateTime, String password, final ProgressResponseBody.ProgressListener progressListener, final ProgressRequestBody.ProgressRequestListener progressRequestListener) throws ApiException {
    Object localVarPostBody = null;
    
    // verify the required parameter 'number' is set
    if (number == null) {
       throw new ApiException("Missing the required parameter 'number' when calling testEndpointParameters(Async)");
    }
    
    // verify the required parameter '_double' is set
    if (_double == null) {
       throw new ApiException("Missing the required parameter '_double' when calling testEndpointParameters(Async)");
    }
    
    // verify the required parameter 'string' is set
    if (string == null) {
       throw new ApiException("Missing the required parameter 'string' when calling testEndpointParameters(Async)");
    }
    
    // verify the required parameter '_byte' is set
    if (_byte == null) {
       throw new ApiException("Missing the required parameter '_byte' when calling testEndpointParameters(Async)");
    }
    

    // create path and map variables
    String localVarPath = "/fake".replaceAll("\\{format\\}","json");

    List<Pair> localVarQueryParams = new ArrayList<Pair>();

    Map<String, String> localVarHeaderParams = new HashMap<String, String>();

    Map<String, Object> localVarFormParams = new HashMap<String, Object>();
    if (integer != null)
      localVarFormParams.put("integer", integer);
    if (int32 != null)
      localVarFormParams.put("int32", int32);
    if (int64 != null)
      localVarFormParams.put("int64", int64);
    if (number != null)
      localVarFormParams.put("number", number);
    if (_float != null)
      localVarFormParams.put("float", _float);
    if (_double != null)
      localVarFormParams.put("double", _double);
    if (string != null)
      localVarFormParams.put("string", string);
    if (_byte != null)
      localVarFormParams.put("byte", _byte);
    if (binary != null)
      localVarFormParams.put("binary", binary);
    if (date != null)
      localVarFormParams.put("date", date);
    if (dateTime != null)
      localVarFormParams.put("dateTime", dateTime);
    if (password != null)
      localVarFormParams.put("password", password);

    final String[] localVarAccepts = {
      "application/xml", "application/json"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
    if (localVarAccept != null) localVarHeaderParams.put("Accept", localVarAccept);

    final String[] localVarContentTypes = {
      
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);
    localVarHeaderParams.put("Content-Type", localVarContentType);

    if(progressListener != null) {
      apiClient.getHttpClient().networkInterceptors().add(new Interceptor() {
      @Override
      public Response intercept(Interceptor.Chain chain) throws IOException {
        Response originalResponse = chain.proceed(chain.request());
        return originalResponse.newBuilder()
                .body(new ProgressResponseBody(originalResponse.body(), progressListener))
                .build();
        }
      });
    }

    String[] localVarAuthNames = new String[] {  };
    return apiClient.buildCall(localVarPath, "POST", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarAuthNames, progressRequestListener);
  }

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
   * @throws ApiException If fail to call the API, e.g. server error or cannot deserialize the response body
   */
  public void testEndpointParameters(BigDecimal number, Double _double, String string, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, byte[] binary, Date date, Date dateTime, String password) throws ApiException {
    testEndpointParametersWithHttpInfo(number, _double, string, _byte, integer, int32, int64, _float, binary, date, dateTime, password);
  }

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
   * @return ApiResponse<Void>
   * @throws ApiException If fail to call the API, e.g. server error or cannot deserialize the response body
   */
  public ApiResponse<Void> testEndpointParametersWithHttpInfo(BigDecimal number, Double _double, String string, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, byte[] binary, Date date, Date dateTime, String password) throws ApiException {
    Call call = testEndpointParametersCall(number, _double, string, _byte, integer, int32, int64, _float, binary, date, dateTime, password, null, null);
    return apiClient.execute(call);
  }

  /**
   * Fake endpoint for testing various parameters (asynchronously)
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
   * @param callback The callback to be executed when the API call finishes
   * @return The request call
   * @throws ApiException If fail to process the API call, e.g. serializing the request body object
   */
  public Call testEndpointParametersAsync(BigDecimal number, Double _double, String string, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, byte[] binary, Date date, Date dateTime, String password, final ApiCallback<Void> callback) throws ApiException {

    ProgressResponseBody.ProgressListener progressListener = null;
    ProgressRequestBody.ProgressRequestListener progressRequestListener = null;

    if (callback != null) {
      progressListener = new ProgressResponseBody.ProgressListener() {
        @Override
        public void update(long bytesRead, long contentLength, boolean done) {
          callback.onDownloadProgress(bytesRead, contentLength, done);
        }
      };

      progressRequestListener = new ProgressRequestBody.ProgressRequestListener() {
        @Override
        public void onRequestProgress(long bytesWritten, long contentLength, boolean done) {
          callback.onUploadProgress(bytesWritten, contentLength, done);
        }
      };
    }

    Call call = testEndpointParametersCall(number, _double, string, _byte, integer, int32, int64, _float, binary, date, dateTime, password, progressListener, progressRequestListener);
    apiClient.executeAsync(call, callback);
    return call;
  }
}
