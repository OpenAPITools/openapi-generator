package org.openapitools.client.api;

import io.vertx.core.file.AsyncFile;
import java.math.BigDecimal;
import org.openapitools.client.model.ChildWithNullable;
import org.openapitools.client.model.Client;
import org.openapitools.client.model.EnumClass;
import org.openapitools.client.model.FakeBigDecimalMap200Response;
import org.openapitools.client.model.FileSchemaTestClass;
import org.openapitools.client.model.HealthCheckResult;
import java.time.LocalDate;
import java.time.OffsetDateTime;
import org.openapitools.client.model.OuterComposite;
import org.openapitools.client.model.OuterObjectWithEnumProperty;
import org.openapitools.client.model.Pet;
import org.openapitools.client.model.User;

import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.MultiMap;
import io.vertx.core.json.JsonObject;

import com.fasterxml.jackson.core.type.TypeReference;

import java.util.*;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;

import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.Pair;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen")
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
    * for Java apache and Java native, test toUrlQueryString for maps with BegDecimal keys
    * @param resultHandler Asynchronous result handler
    */
    public void fakeBigDecimalMap(Handler<AsyncResult<FakeBigDecimalMap200Response>> resultHandler) {
        fakeBigDecimalMap(null, resultHandler);
    }

    /**
    * 
    * for Java apache and Java native, test toUrlQueryString for maps with BegDecimal keys
    * @param authInfo per call authentication override.
    * @param resultHandler Asynchronous result handler
    */
    public void fakeBigDecimalMap(ApiClient.AuthInfo authInfo, Handler<AsyncResult<FakeBigDecimalMap200Response>> resultHandler) {
        Object localVarBody = null;
        
        // create path and map variables
        String localVarPath = "/fake/BigDecimalMap";

        // query params
        List<Pair> localVarQueryParams = new ArrayList<>();

        // header params
        MultiMap localVarHeaderParams = MultiMap.caseInsensitiveMultiMap();
        
        // cookie params
        MultiMap localVarCookieParams = MultiMap.caseInsensitiveMultiMap();
        
        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        
        String[] localVarAccepts = { "*/*" };
        String[] localVarContentTypes = {  };
        String[] localVarAuthNames = new String[] {  };
        TypeReference<FakeBigDecimalMap200Response> localVarReturnType = new TypeReference<FakeBigDecimalMap200Response>() {};
        apiClient.invokeAPI(localVarPath, "GET", localVarQueryParams, localVarBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, authInfo, localVarReturnType, resultHandler);
    }
    /**
    * Health check endpoint
    * 
    * @param resultHandler Asynchronous result handler
    */
    public void fakeHealthGet(Handler<AsyncResult<HealthCheckResult>> resultHandler) {
        fakeHealthGet(null, resultHandler);
    }

    /**
    * Health check endpoint
    * 
    * @param authInfo per call authentication override.
    * @param resultHandler Asynchronous result handler
    */
    public void fakeHealthGet(ApiClient.AuthInfo authInfo, Handler<AsyncResult<HealthCheckResult>> resultHandler) {
        Object localVarBody = null;
        
        // create path and map variables
        String localVarPath = "/fake/health";

        // query params
        List<Pair> localVarQueryParams = new ArrayList<>();

        // header params
        MultiMap localVarHeaderParams = MultiMap.caseInsensitiveMultiMap();
        
        // cookie params
        MultiMap localVarCookieParams = MultiMap.caseInsensitiveMultiMap();
        
        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        
        String[] localVarAccepts = { "application/json" };
        String[] localVarContentTypes = {  };
        String[] localVarAuthNames = new String[] {  };
        TypeReference<HealthCheckResult> localVarReturnType = new TypeReference<HealthCheckResult>() {};
        apiClient.invokeAPI(localVarPath, "GET", localVarQueryParams, localVarBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, authInfo, localVarReturnType, resultHandler);
    }
    /**
    * test http signature authentication
    * 
        * @param pet Pet object that needs to be added to the store (required)
        * @param query1 query parameter (optional)
        * @param header1 header parameter (optional)
    * @param resultHandler Asynchronous result handler
    */
    public void fakeHttpSignatureTest(Pet pet, String query1, String header1, Handler<AsyncResult<Void>> resultHandler) {
        fakeHttpSignatureTest(pet, query1, header1, null, resultHandler);
    }

    /**
    * test http signature authentication
    * 
    * @param pet Pet object that needs to be added to the store (required)
    * @param query1 query parameter (optional)
    * @param header1 header parameter (optional)
    * @param authInfo per call authentication override.
    * @param resultHandler Asynchronous result handler
    */
    public void fakeHttpSignatureTest(Pet pet, String query1, String header1, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> resultHandler) {
        Object localVarBody = pet;
        
        // verify the required parameter 'pet' is set
        if (pet == null) {
            resultHandler.handle(ApiException.fail(400, "Missing the required parameter 'pet' when calling fakeHttpSignatureTest"));
            return;
        }
        
        // create path and map variables
        String localVarPath = "/fake/http-signature-test";

        // query params
        List<Pair> localVarQueryParams = new ArrayList<>();
        localVarQueryParams.addAll(apiClient.parameterToPairs("", "query_1", query1));

        // header params
        MultiMap localVarHeaderParams = MultiMap.caseInsensitiveMultiMap();
        if (header1 != null)
        localVarHeaderParams.add("header_1", apiClient.parameterToString(header1));

        // cookie params
        MultiMap localVarCookieParams = MultiMap.caseInsensitiveMultiMap();
        
        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        
        String[] localVarAccepts = {  };
        String[] localVarContentTypes = { "application/json", "application/xml" };
        String[] localVarAuthNames = new String[] { "http_signature_test" };

        apiClient.invokeAPI(localVarPath, "GET", localVarQueryParams, localVarBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, authInfo, null, resultHandler);
    }
    /**
    * 
    * Test serialization of outer boolean types
        * @param body Input boolean as post body (optional)
    * @param resultHandler Asynchronous result handler
    */
    public void fakeOuterBooleanSerialize(Boolean body, Handler<AsyncResult<Boolean>> resultHandler) {
        fakeOuterBooleanSerialize(body, null, resultHandler);
    }

    /**
    * 
    * Test serialization of outer boolean types
    * @param body Input boolean as post body (optional)
    * @param authInfo per call authentication override.
    * @param resultHandler Asynchronous result handler
    */
    public void fakeOuterBooleanSerialize(Boolean body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Boolean>> resultHandler) {
        Object localVarBody = body;
        
        // create path and map variables
        String localVarPath = "/fake/outer/boolean";

        // query params
        List<Pair> localVarQueryParams = new ArrayList<>();

        // header params
        MultiMap localVarHeaderParams = MultiMap.caseInsensitiveMultiMap();
        
        // cookie params
        MultiMap localVarCookieParams = MultiMap.caseInsensitiveMultiMap();
        
        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        
        String[] localVarAccepts = { "*/*" };
        String[] localVarContentTypes = { "application/json" };
        String[] localVarAuthNames = new String[] {  };
        TypeReference<Boolean> localVarReturnType = new TypeReference<Boolean>() {};
        apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, authInfo, localVarReturnType, resultHandler);
    }
    /**
    * 
    * Test serialization of object with outer number type
        * @param outerComposite Input composite as post body (optional)
    * @param resultHandler Asynchronous result handler
    */
    public void fakeOuterCompositeSerialize(OuterComposite outerComposite, Handler<AsyncResult<OuterComposite>> resultHandler) {
        fakeOuterCompositeSerialize(outerComposite, null, resultHandler);
    }

    /**
    * 
    * Test serialization of object with outer number type
    * @param outerComposite Input composite as post body (optional)
    * @param authInfo per call authentication override.
    * @param resultHandler Asynchronous result handler
    */
    public void fakeOuterCompositeSerialize(OuterComposite outerComposite, ApiClient.AuthInfo authInfo, Handler<AsyncResult<OuterComposite>> resultHandler) {
        Object localVarBody = outerComposite;
        
        // create path and map variables
        String localVarPath = "/fake/outer/composite";

        // query params
        List<Pair> localVarQueryParams = new ArrayList<>();

        // header params
        MultiMap localVarHeaderParams = MultiMap.caseInsensitiveMultiMap();
        
        // cookie params
        MultiMap localVarCookieParams = MultiMap.caseInsensitiveMultiMap();
        
        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        
        String[] localVarAccepts = { "*/*" };
        String[] localVarContentTypes = { "application/json" };
        String[] localVarAuthNames = new String[] {  };
        TypeReference<OuterComposite> localVarReturnType = new TypeReference<OuterComposite>() {};
        apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, authInfo, localVarReturnType, resultHandler);
    }
    /**
    * 
    * Test serialization of outer number types
        * @param body Input number as post body (optional)
    * @param resultHandler Asynchronous result handler
    */
    public void fakeOuterNumberSerialize(BigDecimal body, Handler<AsyncResult<BigDecimal>> resultHandler) {
        fakeOuterNumberSerialize(body, null, resultHandler);
    }

    /**
    * 
    * Test serialization of outer number types
    * @param body Input number as post body (optional)
    * @param authInfo per call authentication override.
    * @param resultHandler Asynchronous result handler
    */
    public void fakeOuterNumberSerialize(BigDecimal body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<BigDecimal>> resultHandler) {
        Object localVarBody = body;
        
        // create path and map variables
        String localVarPath = "/fake/outer/number";

        // query params
        List<Pair> localVarQueryParams = new ArrayList<>();

        // header params
        MultiMap localVarHeaderParams = MultiMap.caseInsensitiveMultiMap();
        
        // cookie params
        MultiMap localVarCookieParams = MultiMap.caseInsensitiveMultiMap();
        
        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        
        String[] localVarAccepts = { "*/*" };
        String[] localVarContentTypes = { "application/json" };
        String[] localVarAuthNames = new String[] {  };
        TypeReference<BigDecimal> localVarReturnType = new TypeReference<BigDecimal>() {};
        apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, authInfo, localVarReturnType, resultHandler);
    }
    /**
    * 
    * Test serialization of outer string types
        * @param body Input string as post body (optional)
    * @param resultHandler Asynchronous result handler
    */
    public void fakeOuterStringSerialize(String body, Handler<AsyncResult<String>> resultHandler) {
        fakeOuterStringSerialize(body, null, resultHandler);
    }

    /**
    * 
    * Test serialization of outer string types
    * @param body Input string as post body (optional)
    * @param authInfo per call authentication override.
    * @param resultHandler Asynchronous result handler
    */
    public void fakeOuterStringSerialize(String body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<String>> resultHandler) {
        Object localVarBody = body;
        
        // create path and map variables
        String localVarPath = "/fake/outer/string";

        // query params
        List<Pair> localVarQueryParams = new ArrayList<>();

        // header params
        MultiMap localVarHeaderParams = MultiMap.caseInsensitiveMultiMap();
        
        // cookie params
        MultiMap localVarCookieParams = MultiMap.caseInsensitiveMultiMap();
        
        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        
        String[] localVarAccepts = { "*/*" };
        String[] localVarContentTypes = { "application/json" };
        String[] localVarAuthNames = new String[] {  };
        TypeReference<String> localVarReturnType = new TypeReference<String>() {};
        apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, authInfo, localVarReturnType, resultHandler);
    }
    /**
    * 
    * Test serialization of enum (int) properties with examples
        * @param outerObjectWithEnumProperty Input enum (int) as post body (required)
    * @param resultHandler Asynchronous result handler
    */
    public void fakePropertyEnumIntegerSerialize(OuterObjectWithEnumProperty outerObjectWithEnumProperty, Handler<AsyncResult<OuterObjectWithEnumProperty>> resultHandler) {
        fakePropertyEnumIntegerSerialize(outerObjectWithEnumProperty, null, resultHandler);
    }

    /**
    * 
    * Test serialization of enum (int) properties with examples
    * @param outerObjectWithEnumProperty Input enum (int) as post body (required)
    * @param authInfo per call authentication override.
    * @param resultHandler Asynchronous result handler
    */
    public void fakePropertyEnumIntegerSerialize(OuterObjectWithEnumProperty outerObjectWithEnumProperty, ApiClient.AuthInfo authInfo, Handler<AsyncResult<OuterObjectWithEnumProperty>> resultHandler) {
        Object localVarBody = outerObjectWithEnumProperty;
        
        // verify the required parameter 'outerObjectWithEnumProperty' is set
        if (outerObjectWithEnumProperty == null) {
            resultHandler.handle(ApiException.fail(400, "Missing the required parameter 'outerObjectWithEnumProperty' when calling fakePropertyEnumIntegerSerialize"));
            return;
        }
        
        // create path and map variables
        String localVarPath = "/fake/property/enum-int";

        // query params
        List<Pair> localVarQueryParams = new ArrayList<>();

        // header params
        MultiMap localVarHeaderParams = MultiMap.caseInsensitiveMultiMap();
        
        // cookie params
        MultiMap localVarCookieParams = MultiMap.caseInsensitiveMultiMap();
        
        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        
        String[] localVarAccepts = { "*/*" };
        String[] localVarContentTypes = { "application/json" };
        String[] localVarAuthNames = new String[] {  };
        TypeReference<OuterObjectWithEnumProperty> localVarReturnType = new TypeReference<OuterObjectWithEnumProperty>() {};
        apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, authInfo, localVarReturnType, resultHandler);
    }
    /**
    * 
    * For this test, the body has to be a binary file.
        * @param body image to upload (required)
    * @param resultHandler Asynchronous result handler
    */
    public void testBodyWithBinary(AsyncFile body, Handler<AsyncResult<Void>> resultHandler) {
        testBodyWithBinary(body, null, resultHandler);
    }

    /**
    * 
    * For this test, the body has to be a binary file.
    * @param body image to upload (required)
    * @param authInfo per call authentication override.
    * @param resultHandler Asynchronous result handler
    */
    public void testBodyWithBinary(AsyncFile body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> resultHandler) {
        Object localVarBody = body;
        
        // verify the required parameter 'body' is set
        if (body == null) {
            resultHandler.handle(ApiException.fail(400, "Missing the required parameter 'body' when calling testBodyWithBinary"));
            return;
        }
        
        // create path and map variables
        String localVarPath = "/fake/body-with-binary";

        // query params
        List<Pair> localVarQueryParams = new ArrayList<>();

        // header params
        MultiMap localVarHeaderParams = MultiMap.caseInsensitiveMultiMap();
        
        // cookie params
        MultiMap localVarCookieParams = MultiMap.caseInsensitiveMultiMap();
        
        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        
        String[] localVarAccepts = {  };
        String[] localVarContentTypes = { "image/png" };
        String[] localVarAuthNames = new String[] {  };

        apiClient.invokeAPI(localVarPath, "PUT", localVarQueryParams, localVarBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, authInfo, null, resultHandler);
    }
    /**
    * 
    * For this test, the body for this request must reference a schema named &#x60;File&#x60;.
        * @param fileSchemaTestClass  (required)
    * @param resultHandler Asynchronous result handler
    */
    public void testBodyWithFileSchema(FileSchemaTestClass fileSchemaTestClass, Handler<AsyncResult<Void>> resultHandler) {
        testBodyWithFileSchema(fileSchemaTestClass, null, resultHandler);
    }

    /**
    * 
    * For this test, the body for this request must reference a schema named &#x60;File&#x60;.
    * @param fileSchemaTestClass  (required)
    * @param authInfo per call authentication override.
    * @param resultHandler Asynchronous result handler
    */
    public void testBodyWithFileSchema(FileSchemaTestClass fileSchemaTestClass, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> resultHandler) {
        Object localVarBody = fileSchemaTestClass;
        
        // verify the required parameter 'fileSchemaTestClass' is set
        if (fileSchemaTestClass == null) {
            resultHandler.handle(ApiException.fail(400, "Missing the required parameter 'fileSchemaTestClass' when calling testBodyWithFileSchema"));
            return;
        }
        
        // create path and map variables
        String localVarPath = "/fake/body-with-file-schema";

        // query params
        List<Pair> localVarQueryParams = new ArrayList<>();

        // header params
        MultiMap localVarHeaderParams = MultiMap.caseInsensitiveMultiMap();
        
        // cookie params
        MultiMap localVarCookieParams = MultiMap.caseInsensitiveMultiMap();
        
        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        
        String[] localVarAccepts = {  };
        String[] localVarContentTypes = { "application/json" };
        String[] localVarAuthNames = new String[] {  };

        apiClient.invokeAPI(localVarPath, "PUT", localVarQueryParams, localVarBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, authInfo, null, resultHandler);
    }
    /**
    * 
    * 
        * @param query  (required)
        * @param user  (required)
    * @param resultHandler Asynchronous result handler
    */
    public void testBodyWithQueryParams(String query, User user, Handler<AsyncResult<Void>> resultHandler) {
        testBodyWithQueryParams(query, user, null, resultHandler);
    }

    /**
    * 
    * 
    * @param query  (required)
    * @param user  (required)
    * @param authInfo per call authentication override.
    * @param resultHandler Asynchronous result handler
    */
    public void testBodyWithQueryParams(String query, User user, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> resultHandler) {
        Object localVarBody = user;
        
        // verify the required parameter 'query' is set
        if (query == null) {
            resultHandler.handle(ApiException.fail(400, "Missing the required parameter 'query' when calling testBodyWithQueryParams"));
            return;
        }
        
        // verify the required parameter 'user' is set
        if (user == null) {
            resultHandler.handle(ApiException.fail(400, "Missing the required parameter 'user' when calling testBodyWithQueryParams"));
            return;
        }
        
        // create path and map variables
        String localVarPath = "/fake/body-with-query-params";

        // query params
        List<Pair> localVarQueryParams = new ArrayList<>();
        localVarQueryParams.addAll(apiClient.parameterToPairs("", "query", query));

        // header params
        MultiMap localVarHeaderParams = MultiMap.caseInsensitiveMultiMap();
        
        // cookie params
        MultiMap localVarCookieParams = MultiMap.caseInsensitiveMultiMap();
        
        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        
        String[] localVarAccepts = {  };
        String[] localVarContentTypes = { "application/json" };
        String[] localVarAuthNames = new String[] {  };

        apiClient.invokeAPI(localVarPath, "PUT", localVarQueryParams, localVarBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, authInfo, null, resultHandler);
    }
    /**
    * To test \&quot;client\&quot; model
    * To test \&quot;client\&quot; model
        * @param client client model (required)
    * @param resultHandler Asynchronous result handler
    */
    public void testClientModel(Client client, Handler<AsyncResult<Client>> resultHandler) {
        testClientModel(client, null, resultHandler);
    }

    /**
    * To test \&quot;client\&quot; model
    * To test \&quot;client\&quot; model
    * @param client client model (required)
    * @param authInfo per call authentication override.
    * @param resultHandler Asynchronous result handler
    */
    public void testClientModel(Client client, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Client>> resultHandler) {
        Object localVarBody = client;
        
        // verify the required parameter 'client' is set
        if (client == null) {
            resultHandler.handle(ApiException.fail(400, "Missing the required parameter 'client' when calling testClientModel"));
            return;
        }
        
        // create path and map variables
        String localVarPath = "/fake";

        // query params
        List<Pair> localVarQueryParams = new ArrayList<>();

        // header params
        MultiMap localVarHeaderParams = MultiMap.caseInsensitiveMultiMap();
        
        // cookie params
        MultiMap localVarCookieParams = MultiMap.caseInsensitiveMultiMap();
        
        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        
        String[] localVarAccepts = { "application/json" };
        String[] localVarContentTypes = { "application/json" };
        String[] localVarAuthNames = new String[] {  };
        TypeReference<Client> localVarReturnType = new TypeReference<Client>() {};
        apiClient.invokeAPI(localVarPath, "PATCH", localVarQueryParams, localVarBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, authInfo, localVarReturnType, resultHandler);
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
    public void testEndpointParameters(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, String string, AsyncFile binary, LocalDate date, OffsetDateTime dateTime, String password, String paramCallback, Handler<AsyncResult<Void>> resultHandler) {
        testEndpointParameters(number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, string, binary, date, dateTime, password, paramCallback, null, resultHandler);
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
    * @param authInfo per call authentication override.
    * @param resultHandler Asynchronous result handler
    */
    public void testEndpointParameters(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, String string, AsyncFile binary, LocalDate date, OffsetDateTime dateTime, String password, String paramCallback, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> resultHandler) {
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
        
        // cookie params
        MultiMap localVarCookieParams = MultiMap.caseInsensitiveMultiMap();
        
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

        String[] localVarAccepts = {  };
        String[] localVarContentTypes = { "application/x-www-form-urlencoded" };
        String[] localVarAuthNames = new String[] { "http_basic_test" };

        apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, authInfo, null, resultHandler);
    }
    /**
    * To test enum parameters
    * To test enum parameters
        * @param enumHeaderStringArray Header parameter enum test (string array) (optional)
        * @param enumHeaderString Header parameter enum test (string) (optional, default to -efg)
        * @param enumQueryStringArray Query parameter enum test (string array) (optional)
        * @param enumQueryString Query parameter enum test (string) (optional, default to -efg)
        * @param enumQueryInteger Query parameter enum test (double) (optional)
        * @param enumQueryDouble Query parameter enum test (double) (optional)
        * @param enumQueryModelArray  (optional)
        * @param enumFormStringArray Form parameter enum test (string array) (optional, default to $)
        * @param enumFormString Form parameter enum test (string) (optional, default to -efg)
    * @param resultHandler Asynchronous result handler
    */
    public void testEnumParameters(List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Double enumQueryDouble, List<EnumClass> enumQueryModelArray, List<String> enumFormStringArray, String enumFormString, Handler<AsyncResult<Void>> resultHandler) {
        testEnumParameters(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumQueryModelArray, enumFormStringArray, enumFormString, null, resultHandler);
    }

    /**
    * To test enum parameters
    * To test enum parameters
    * @param enumHeaderStringArray Header parameter enum test (string array) (optional)
    * @param enumHeaderString Header parameter enum test (string) (optional, default to -efg)
    * @param enumQueryStringArray Query parameter enum test (string array) (optional)
    * @param enumQueryString Query parameter enum test (string) (optional, default to -efg)
    * @param enumQueryInteger Query parameter enum test (double) (optional)
    * @param enumQueryDouble Query parameter enum test (double) (optional)
    * @param enumQueryModelArray  (optional)
    * @param enumFormStringArray Form parameter enum test (string array) (optional, default to $)
    * @param enumFormString Form parameter enum test (string) (optional, default to -efg)
    * @param authInfo per call authentication override.
    * @param resultHandler Asynchronous result handler
    */
    public void testEnumParameters(List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Double enumQueryDouble, List<EnumClass> enumQueryModelArray, List<String> enumFormStringArray, String enumFormString, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> resultHandler) {
        Object localVarBody = null;
        
        // create path and map variables
        String localVarPath = "/fake";

        // query params
        List<Pair> localVarQueryParams = new ArrayList<>();
        localVarQueryParams.addAll(apiClient.parameterToPairs("multi", "enum_query_string_array", enumQueryStringArray));
        localVarQueryParams.addAll(apiClient.parameterToPairs("", "enum_query_string", enumQueryString));
        localVarQueryParams.addAll(apiClient.parameterToPairs("", "enum_query_integer", enumQueryInteger));
        localVarQueryParams.addAll(apiClient.parameterToPairs("", "enum_query_double", enumQueryDouble));
        localVarQueryParams.addAll(apiClient.parameterToPairs("multi", "enum_query_model_array", enumQueryModelArray));

        // header params
        MultiMap localVarHeaderParams = MultiMap.caseInsensitiveMultiMap();
        if (enumHeaderStringArray != null)
        localVarHeaderParams.add("enum_header_string_array", apiClient.parameterToString(enumHeaderStringArray));
if (enumHeaderString != null)
        localVarHeaderParams.add("enum_header_string", apiClient.parameterToString(enumHeaderString));

        // cookie params
        MultiMap localVarCookieParams = MultiMap.caseInsensitiveMultiMap();
        
        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        if (enumFormStringArray != null) localVarFormParams.put("enum_form_string_array", enumFormStringArray);
if (enumFormString != null) localVarFormParams.put("enum_form_string", enumFormString);

        String[] localVarAccepts = {  };
        String[] localVarContentTypes = { "application/x-www-form-urlencoded" };
        String[] localVarAuthNames = new String[] {  };

        apiClient.invokeAPI(localVarPath, "GET", localVarQueryParams, localVarBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, authInfo, null, resultHandler);
    }
    /**
    * Fake endpoint to test group parameters (optional)
    * Fake endpoint to test group parameters (optional)
        * @param requiredStringGroup Required String in group parameters (required)
        * @param requiredBooleanGroup Required Boolean in group parameters (required)
        * @param requiredInt64Group Required Integer in group parameters (required)
        * @param stringGroup String in group parameters (optional)
        * @param booleanGroup Boolean in group parameters (optional)
        * @param int64Group Integer in group parameters (optional)
    * @param resultHandler Asynchronous result handler
    */
    public void testGroupParameters(Integer requiredStringGroup, Boolean requiredBooleanGroup, Long requiredInt64Group, Integer stringGroup, Boolean booleanGroup, Long int64Group, Handler<AsyncResult<Void>> resultHandler) {
        testGroupParameters(requiredStringGroup, requiredBooleanGroup, requiredInt64Group, stringGroup, booleanGroup, int64Group, null, resultHandler);
    }

    /**
    * Fake endpoint to test group parameters (optional)
    * Fake endpoint to test group parameters (optional)
    * @param requiredStringGroup Required String in group parameters (required)
    * @param requiredBooleanGroup Required Boolean in group parameters (required)
    * @param requiredInt64Group Required Integer in group parameters (required)
    * @param stringGroup String in group parameters (optional)
    * @param booleanGroup Boolean in group parameters (optional)
    * @param int64Group Integer in group parameters (optional)
    * @param authInfo per call authentication override.
    * @param resultHandler Asynchronous result handler
    */
    public void testGroupParameters(Integer requiredStringGroup, Boolean requiredBooleanGroup, Long requiredInt64Group, Integer stringGroup, Boolean booleanGroup, Long int64Group, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> resultHandler) {
        Object localVarBody = null;
        
        // verify the required parameter 'requiredStringGroup' is set
        if (requiredStringGroup == null) {
            resultHandler.handle(ApiException.fail(400, "Missing the required parameter 'requiredStringGroup' when calling testGroupParameters"));
            return;
        }
        
        // verify the required parameter 'requiredBooleanGroup' is set
        if (requiredBooleanGroup == null) {
            resultHandler.handle(ApiException.fail(400, "Missing the required parameter 'requiredBooleanGroup' when calling testGroupParameters"));
            return;
        }
        
        // verify the required parameter 'requiredInt64Group' is set
        if (requiredInt64Group == null) {
            resultHandler.handle(ApiException.fail(400, "Missing the required parameter 'requiredInt64Group' when calling testGroupParameters"));
            return;
        }
        
        // create path and map variables
        String localVarPath = "/fake";

        // query params
        List<Pair> localVarQueryParams = new ArrayList<>();
        localVarQueryParams.addAll(apiClient.parameterToPairs("", "required_string_group", requiredStringGroup));
        localVarQueryParams.addAll(apiClient.parameterToPairs("", "required_int64_group", requiredInt64Group));
        localVarQueryParams.addAll(apiClient.parameterToPairs("", "string_group", stringGroup));
        localVarQueryParams.addAll(apiClient.parameterToPairs("", "int64_group", int64Group));

        // header params
        MultiMap localVarHeaderParams = MultiMap.caseInsensitiveMultiMap();
        if (requiredBooleanGroup != null)
        localVarHeaderParams.add("required_boolean_group", apiClient.parameterToString(requiredBooleanGroup));
if (booleanGroup != null)
        localVarHeaderParams.add("boolean_group", apiClient.parameterToString(booleanGroup));

        // cookie params
        MultiMap localVarCookieParams = MultiMap.caseInsensitiveMultiMap();
        
        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        
        String[] localVarAccepts = {  };
        String[] localVarContentTypes = {  };
        String[] localVarAuthNames = new String[] { "bearer_test" };

        apiClient.invokeAPI(localVarPath, "DELETE", localVarQueryParams, localVarBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, authInfo, null, resultHandler);
    }
    /**
    * test inline additionalProperties
    * 
        * @param requestBody request body (required)
    * @param resultHandler Asynchronous result handler
    */
    public void testInlineAdditionalProperties(Map<String, String> requestBody, Handler<AsyncResult<Void>> resultHandler) {
        testInlineAdditionalProperties(requestBody, null, resultHandler);
    }

    /**
    * test inline additionalProperties
    * 
    * @param requestBody request body (required)
    * @param authInfo per call authentication override.
    * @param resultHandler Asynchronous result handler
    */
    public void testInlineAdditionalProperties(Map<String, String> requestBody, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> resultHandler) {
        Object localVarBody = requestBody;
        
        // verify the required parameter 'requestBody' is set
        if (requestBody == null) {
            resultHandler.handle(ApiException.fail(400, "Missing the required parameter 'requestBody' when calling testInlineAdditionalProperties"));
            return;
        }
        
        // create path and map variables
        String localVarPath = "/fake/inline-additionalProperties";

        // query params
        List<Pair> localVarQueryParams = new ArrayList<>();

        // header params
        MultiMap localVarHeaderParams = MultiMap.caseInsensitiveMultiMap();
        
        // cookie params
        MultiMap localVarCookieParams = MultiMap.caseInsensitiveMultiMap();
        
        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        
        String[] localVarAccepts = {  };
        String[] localVarContentTypes = { "application/json" };
        String[] localVarAuthNames = new String[] {  };

        apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, authInfo, null, resultHandler);
    }
    /**
    * test json serialization of form data
    * 
        * @param param field1 (required)
        * @param param2 field2 (required)
    * @param resultHandler Asynchronous result handler
    */
    public void testJsonFormData(String param, String param2, Handler<AsyncResult<Void>> resultHandler) {
        testJsonFormData(param, param2, null, resultHandler);
    }

    /**
    * test json serialization of form data
    * 
    * @param param field1 (required)
    * @param param2 field2 (required)
    * @param authInfo per call authentication override.
    * @param resultHandler Asynchronous result handler
    */
    public void testJsonFormData(String param, String param2, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> resultHandler) {
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
        
        // cookie params
        MultiMap localVarCookieParams = MultiMap.caseInsensitiveMultiMap();
        
        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        if (param != null) localVarFormParams.put("param", param);
if (param2 != null) localVarFormParams.put("param2", param2);

        String[] localVarAccepts = {  };
        String[] localVarContentTypes = { "application/x-www-form-urlencoded" };
        String[] localVarAuthNames = new String[] {  };

        apiClient.invokeAPI(localVarPath, "GET", localVarQueryParams, localVarBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, authInfo, null, resultHandler);
    }
    /**
    * test nullable parent property
    * 
        * @param childWithNullable request body (required)
    * @param resultHandler Asynchronous result handler
    */
    public void testNullable(ChildWithNullable childWithNullable, Handler<AsyncResult<Void>> resultHandler) {
        testNullable(childWithNullable, null, resultHandler);
    }

    /**
    * test nullable parent property
    * 
    * @param childWithNullable request body (required)
    * @param authInfo per call authentication override.
    * @param resultHandler Asynchronous result handler
    */
    public void testNullable(ChildWithNullable childWithNullable, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> resultHandler) {
        Object localVarBody = childWithNullable;
        
        // verify the required parameter 'childWithNullable' is set
        if (childWithNullable == null) {
            resultHandler.handle(ApiException.fail(400, "Missing the required parameter 'childWithNullable' when calling testNullable"));
            return;
        }
        
        // create path and map variables
        String localVarPath = "/fake/nullable";

        // query params
        List<Pair> localVarQueryParams = new ArrayList<>();

        // header params
        MultiMap localVarHeaderParams = MultiMap.caseInsensitiveMultiMap();
        
        // cookie params
        MultiMap localVarCookieParams = MultiMap.caseInsensitiveMultiMap();
        
        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        
        String[] localVarAccepts = {  };
        String[] localVarContentTypes = { "application/json" };
        String[] localVarAuthNames = new String[] {  };

        apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, authInfo, null, resultHandler);
    }
    /**
    * 
    * To test the collection format in query parameters
        * @param pipe  (required)
        * @param ioutil  (required)
        * @param http  (required)
        * @param url  (required)
        * @param context  (required)
        * @param allowEmpty  (required)
        * @param language  (optional)
    * @param resultHandler Asynchronous result handler
    */
    public void testQueryParameterCollectionFormat(List<String> pipe, List<String> ioutil, List<String> http, List<String> url, List<String> context, String allowEmpty, Map<String, String> language, Handler<AsyncResult<Void>> resultHandler) {
        testQueryParameterCollectionFormat(pipe, ioutil, http, url, context, allowEmpty, language, null, resultHandler);
    }

    /**
    * 
    * To test the collection format in query parameters
    * @param pipe  (required)
    * @param ioutil  (required)
    * @param http  (required)
    * @param url  (required)
    * @param context  (required)
    * @param allowEmpty  (required)
    * @param language  (optional)
    * @param authInfo per call authentication override.
    * @param resultHandler Asynchronous result handler
    */
    public void testQueryParameterCollectionFormat(List<String> pipe, List<String> ioutil, List<String> http, List<String> url, List<String> context, String allowEmpty, Map<String, String> language, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> resultHandler) {
        Object localVarBody = null;
        
        // verify the required parameter 'pipe' is set
        if (pipe == null) {
            resultHandler.handle(ApiException.fail(400, "Missing the required parameter 'pipe' when calling testQueryParameterCollectionFormat"));
            return;
        }
        
        // verify the required parameter 'ioutil' is set
        if (ioutil == null) {
            resultHandler.handle(ApiException.fail(400, "Missing the required parameter 'ioutil' when calling testQueryParameterCollectionFormat"));
            return;
        }
        
        // verify the required parameter 'http' is set
        if (http == null) {
            resultHandler.handle(ApiException.fail(400, "Missing the required parameter 'http' when calling testQueryParameterCollectionFormat"));
            return;
        }
        
        // verify the required parameter 'url' is set
        if (url == null) {
            resultHandler.handle(ApiException.fail(400, "Missing the required parameter 'url' when calling testQueryParameterCollectionFormat"));
            return;
        }
        
        // verify the required parameter 'context' is set
        if (context == null) {
            resultHandler.handle(ApiException.fail(400, "Missing the required parameter 'context' when calling testQueryParameterCollectionFormat"));
            return;
        }
        
        // verify the required parameter 'allowEmpty' is set
        if (allowEmpty == null) {
            resultHandler.handle(ApiException.fail(400, "Missing the required parameter 'allowEmpty' when calling testQueryParameterCollectionFormat"));
            return;
        }
        
        // create path and map variables
        String localVarPath = "/fake/test-query-parameters";

        // query params
        List<Pair> localVarQueryParams = new ArrayList<>();
        localVarQueryParams.addAll(apiClient.parameterToPairs("pipes", "pipe", pipe));
        localVarQueryParams.addAll(apiClient.parameterToPairs("csv", "ioutil", ioutil));
        localVarQueryParams.addAll(apiClient.parameterToPairs("ssv", "http", http));
        localVarQueryParams.addAll(apiClient.parameterToPairs("csv", "url", url));
        localVarQueryParams.addAll(apiClient.parameterToPairs("multi", "context", context));
        localVarQueryParams.addAll(apiClient.parameterToPairs("", "language", language));
        localVarQueryParams.addAll(apiClient.parameterToPairs("", "allowEmpty", allowEmpty));

        // header params
        MultiMap localVarHeaderParams = MultiMap.caseInsensitiveMultiMap();
        
        // cookie params
        MultiMap localVarCookieParams = MultiMap.caseInsensitiveMultiMap();
        
        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        
        String[] localVarAccepts = {  };
        String[] localVarContentTypes = {  };
        String[] localVarAuthNames = new String[] {  };

        apiClient.invokeAPI(localVarPath, "PUT", localVarQueryParams, localVarBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, authInfo, null, resultHandler);
    }

    private String encodeParameter(String parameter) {
        try {
            return URLEncoder.encode(parameter, StandardCharsets.UTF_8.name());
        } catch (UnsupportedEncodingException e) {
            return parameter;
        }
    }
}
