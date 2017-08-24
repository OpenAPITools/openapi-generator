package io.swagger.client.api;

import java.math.BigDecimal;
import io.swagger.client.model.Client;
import org.threeten.bp.LocalDate;
import org.threeten.bp.OffsetDateTime;
import io.swagger.client.model.OuterComposite;

import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.MultiMap;
import io.vertx.core.json.JsonObject;

import com.fasterxml.jackson.core.type.TypeReference;

import java.util.*;

import io.swagger.client.ApiClient;
import io.swagger.client.ApiException;
import io.swagger.client.Configuration;
import io.swagger.client.Pair;


public class FakeApiImpl implements FakeApi {

    private ApiClient apiClient;

    public FakeApiImpl() {
        this(null);
    }

    public FakeApiImpl(ApiClient apiClient) {
        this.apiClient = apiClient != null ? apiClient : Configuration.getDefaultApiClient();
    }

    public ApiClient getApiClient() {
        return apiClient;
    }

    public void setApiClient(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    /**
     * 
     * Test serialization of outer boolean types
     * @param body Input boolean as post body (optional)
     * @param resultHandler Asynchronous result handler
     */
    public void fakeOuterBooleanSerialize(Boolean body, Handler<AsyncResult<Boolean>> resultHandler) {
        Object localVarBody = body;
        
        // create path and map variables
        String localVarPath = "/fake/outer/boolean";

        // query params
        List<Pair> localVarQueryParams = new ArrayList<>();

        // header params
        MultiMap localVarHeaderParams = MultiMap.caseInsensitiveMultiMap();
        
        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        
        String[] localVarAccepts = {  };
        String[] localVarContentTypes = {  };
        String[] localVarAuthNames = new String[] {  };
        TypeReference<Boolean> localVarReturnType = new TypeReference<Boolean>() {};
        apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarBody, localVarHeaderParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, localVarReturnType, resultHandler);
    }
    /**
     * 
     * Test serialization of object with outer number type
     * @param body Input composite as post body (optional)
     * @param resultHandler Asynchronous result handler
     */
    public void fakeOuterCompositeSerialize(OuterComposite body, Handler<AsyncResult<OuterComposite>> resultHandler) {
        Object localVarBody = body;
        
        // create path and map variables
        String localVarPath = "/fake/outer/composite";

        // query params
        List<Pair> localVarQueryParams = new ArrayList<>();

        // header params
        MultiMap localVarHeaderParams = MultiMap.caseInsensitiveMultiMap();
        
        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        
        String[] localVarAccepts = {  };
        String[] localVarContentTypes = {  };
        String[] localVarAuthNames = new String[] {  };
        TypeReference<OuterComposite> localVarReturnType = new TypeReference<OuterComposite>() {};
        apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarBody, localVarHeaderParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, localVarReturnType, resultHandler);
    }
    /**
     * 
     * Test serialization of outer number types
     * @param body Input number as post body (optional)
     * @param resultHandler Asynchronous result handler
     */
    public void fakeOuterNumberSerialize(BigDecimal body, Handler<AsyncResult<BigDecimal>> resultHandler) {
        Object localVarBody = body;
        
        // create path and map variables
        String localVarPath = "/fake/outer/number";

        // query params
        List<Pair> localVarQueryParams = new ArrayList<>();

        // header params
        MultiMap localVarHeaderParams = MultiMap.caseInsensitiveMultiMap();
        
        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        
        String[] localVarAccepts = {  };
        String[] localVarContentTypes = {  };
        String[] localVarAuthNames = new String[] {  };
        TypeReference<BigDecimal> localVarReturnType = new TypeReference<BigDecimal>() {};
        apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarBody, localVarHeaderParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, localVarReturnType, resultHandler);
    }
    /**
     * 
     * Test serialization of outer string types
     * @param body Input string as post body (optional)
     * @param resultHandler Asynchronous result handler
     */
    public void fakeOuterStringSerialize(String body, Handler<AsyncResult<String>> resultHandler) {
        Object localVarBody = body;
        
        // create path and map variables
        String localVarPath = "/fake/outer/string";

        // query params
        List<Pair> localVarQueryParams = new ArrayList<>();

        // header params
        MultiMap localVarHeaderParams = MultiMap.caseInsensitiveMultiMap();
        
        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        
        String[] localVarAccepts = {  };
        String[] localVarContentTypes = {  };
        String[] localVarAuthNames = new String[] {  };
        TypeReference<String> localVarReturnType = new TypeReference<String>() {};
        apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarBody, localVarHeaderParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, localVarReturnType, resultHandler);
    }
    /**
     * To test \&quot;client\&quot; model
     * To test \&quot;client\&quot; model
     * @param body client model (required)
     * @param resultHandler Asynchronous result handler
     */
    public void testClientModel(Client body, Handler<AsyncResult<Client>> resultHandler) {
        Object localVarBody = body;
        
        // verify the required parameter 'body' is set
        if (body == null) {
            resultHandler.handle(ApiException.fail(400, "Missing the required parameter 'body' when calling testClientModel"));
            return;
        }
        
        // create path and map variables
        String localVarPath = "/fake";

        // query params
        List<Pair> localVarQueryParams = new ArrayList<>();

        // header params
        MultiMap localVarHeaderParams = MultiMap.caseInsensitiveMultiMap();
        
        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        
        String[] localVarAccepts = { "application/json" };
        String[] localVarContentTypes = { "application/json" };
        String[] localVarAuthNames = new String[] {  };
        TypeReference<Client> localVarReturnType = new TypeReference<Client>() {};
        apiClient.invokeAPI(localVarPath, "PATCH", localVarQueryParams, localVarBody, localVarHeaderParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, localVarReturnType, resultHandler);
    }
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
     * @param resultHandler Asynchronous result handler
     */
    public void testEndpointParameters(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, String string, byte[] binary, LocalDate date, OffsetDateTime dateTime, String password, String paramCallback, Handler<AsyncResult<Void>> resultHandler) {
        Object localVarBody = null;
        
        // verify the required parameter 'number' is set
        if (number == null) {
            resultHandler.handle(ApiException.fail(400, "Missing the required parameter 'number' when calling testEndpointParameters"));
            return;
        }
        
        // verify the required parameter '_double' is set
        if (_double == null) {
            resultHandler.handle(ApiException.fail(400, "Missing the required parameter '_double' when calling testEndpointParameters"));
            return;
        }
        
        // verify the required parameter 'patternWithoutDelimiter' is set
        if (patternWithoutDelimiter == null) {
            resultHandler.handle(ApiException.fail(400, "Missing the required parameter 'patternWithoutDelimiter' when calling testEndpointParameters"));
            return;
        }
        
        // verify the required parameter '_byte' is set
        if (_byte == null) {
            resultHandler.handle(ApiException.fail(400, "Missing the required parameter '_byte' when calling testEndpointParameters"));
            return;
        }
        
        // create path and map variables
        String localVarPath = "/fake";

        // query params
        List<Pair> localVarQueryParams = new ArrayList<>();

        // header params
        MultiMap localVarHeaderParams = MultiMap.caseInsensitiveMultiMap();
        
        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        if (integer != null) localVarFormParams.put("integer", integer);
if (int32 != null) localVarFormParams.put("int32", int32);
if (int64 != null) localVarFormParams.put("int64", int64);
if (number != null) localVarFormParams.put("number", number);
if (_float != null) localVarFormParams.put("float", _float);
if (_double != null) localVarFormParams.put("double", _double);
if (string != null) localVarFormParams.put("string", string);
if (patternWithoutDelimiter != null) localVarFormParams.put("pattern_without_delimiter", patternWithoutDelimiter);
if (_byte != null) localVarFormParams.put("byte", _byte);
if (binary != null) localVarFormParams.put("binary", binary);
if (date != null) localVarFormParams.put("date", date);
if (dateTime != null) localVarFormParams.put("dateTime", dateTime);
if (password != null) localVarFormParams.put("password", password);
if (paramCallback != null) localVarFormParams.put("callback", paramCallback);

        String[] localVarAccepts = { "application/xml; charset=utf-8", "application/json; charset=utf-8" };
        String[] localVarContentTypes = { "application/xml; charset=utf-8", "application/json; charset=utf-8" };
        String[] localVarAuthNames = new String[] { "http_basic_test" };

        apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarBody, localVarHeaderParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, null, resultHandler);
    }
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
     * @param resultHandler Asynchronous result handler
     */
    public void testEnumParameters(List<String> enumFormStringArray, String enumFormString, List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Double enumQueryDouble, Handler<AsyncResult<Void>> resultHandler) {
        Object localVarBody = null;
        
        // create path and map variables
        String localVarPath = "/fake";

        // query params
        List<Pair> localVarQueryParams = new ArrayList<>();
        localVarQueryParams.addAll(apiClient.parameterToPairs("csv", "enum_query_string_array", enumQueryStringArray));
        localVarQueryParams.addAll(apiClient.parameterToPairs("", "enum_query_string", enumQueryString));
        localVarQueryParams.addAll(apiClient.parameterToPairs("", "enum_query_integer", enumQueryInteger));

        // header params
        MultiMap localVarHeaderParams = MultiMap.caseInsensitiveMultiMap();
        if (enumHeaderStringArray != null)
        localVarHeaderParams.add("enum_header_string_array", apiClient.parameterToString(enumHeaderStringArray));
if (enumHeaderString != null)
        localVarHeaderParams.add("enum_header_string", apiClient.parameterToString(enumHeaderString));

        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        if (enumFormStringArray != null) localVarFormParams.put("enum_form_string_array", enumFormStringArray);
if (enumFormString != null) localVarFormParams.put("enum_form_string", enumFormString);
if (enumQueryDouble != null) localVarFormParams.put("enum_query_double", enumQueryDouble);

        String[] localVarAccepts = { "*/*" };
        String[] localVarContentTypes = { "*/*" };
        String[] localVarAuthNames = new String[] {  };

        apiClient.invokeAPI(localVarPath, "GET", localVarQueryParams, localVarBody, localVarHeaderParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, null, resultHandler);
    }
    /**
     * test json serialization of form data
     * 
     * @param param field1 (required)
     * @param param2 field2 (required)
     * @param resultHandler Asynchronous result handler
     */
    public void testJsonFormData(String param, String param2, Handler<AsyncResult<Void>> resultHandler) {
        Object localVarBody = null;
        
        // verify the required parameter 'param' is set
        if (param == null) {
            resultHandler.handle(ApiException.fail(400, "Missing the required parameter 'param' when calling testJsonFormData"));
            return;
        }
        
        // verify the required parameter 'param2' is set
        if (param2 == null) {
            resultHandler.handle(ApiException.fail(400, "Missing the required parameter 'param2' when calling testJsonFormData"));
            return;
        }
        
        // create path and map variables
        String localVarPath = "/fake/jsonFormData";

        // query params
        List<Pair> localVarQueryParams = new ArrayList<>();

        // header params
        MultiMap localVarHeaderParams = MultiMap.caseInsensitiveMultiMap();
        
        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        if (param != null) localVarFormParams.put("param", param);
if (param2 != null) localVarFormParams.put("param2", param2);

        String[] localVarAccepts = {  };
        String[] localVarContentTypes = { "application/json" };
        String[] localVarAuthNames = new String[] {  };

        apiClient.invokeAPI(localVarPath, "GET", localVarQueryParams, localVarBody, localVarHeaderParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, null, resultHandler);
    }
}
