package org.openapitools.client.api;

import org.openapitools.client.model.Order;

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
public class StoreApiImpl implements StoreApi {

    private ApiClient apiClient;

    public StoreApiImpl() {
        this(null);
    }

    public StoreApiImpl(ApiClient apiClient) {
        this.apiClient = apiClient != null ? apiClient : Configuration.getDefaultApiClient();
    }

    public ApiClient getApiClient() {
        return apiClient;
    }

    public void setApiClient(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    /**
    * Delete purchase order by ID
    * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
        * @param orderId ID of the order that needs to be deleted (required)
    * @param resultHandler Asynchronous result handler
    */
    public void deleteOrder(String orderId, Handler<AsyncResult<Void>> resultHandler) {
        deleteOrder(orderId, null, resultHandler);
    }

    /**
    * Delete purchase order by ID
    * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
    * @param orderId ID of the order that needs to be deleted (required)
    * @param authInfo per call authentication override.
    * @param resultHandler Asynchronous result handler
    */
    public void deleteOrder(String orderId, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> resultHandler) {
        Object localVarBody = null;
        
        // verify the required parameter 'orderId' is set
        if (orderId == null) {
            resultHandler.handle(ApiException.fail(400, "Missing the required parameter 'orderId' when calling deleteOrder"));
            return;
        }
        
        // create path and map variables
        String localVarPath = "/store/order/{order_id}".replaceAll("\\{" + "order_id" + "\\}", encodeParameter(orderId.toString()));

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
        String[] localVarContentTypes = {  };
        String[] localVarAuthNames = new String[] {  };

        apiClient.invokeAPI(localVarPath, "DELETE", localVarQueryParams, localVarBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, authInfo, null, resultHandler);
    }
    /**
    * Returns pet inventories by status
    * Returns a map of status codes to quantities
    * @param resultHandler Asynchronous result handler
    */
    public void getInventory(Handler<AsyncResult<Map<String, Integer>>> resultHandler) {
        getInventory(null, resultHandler);
    }

    /**
    * Returns pet inventories by status
    * Returns a map of status codes to quantities
    * @param authInfo per call authentication override.
    * @param resultHandler Asynchronous result handler
    */
    public void getInventory(ApiClient.AuthInfo authInfo, Handler<AsyncResult<Map<String, Integer>>> resultHandler) {
        Object localVarBody = null;
        
        // create path and map variables
        String localVarPath = "/store/inventory";

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
        String[] localVarAuthNames = new String[] { "api_key" };
        TypeReference<Map<String, Integer>> localVarReturnType = new TypeReference<Map<String, Integer>>() {};
        apiClient.invokeAPI(localVarPath, "GET", localVarQueryParams, localVarBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, authInfo, localVarReturnType, resultHandler);
    }
    /**
    * Find purchase order by ID
    * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
        * @param orderId ID of pet that needs to be fetched (required)
    * @param resultHandler Asynchronous result handler
    */
    public void getOrderById(Long orderId, Handler<AsyncResult<Order>> resultHandler) {
        getOrderById(orderId, null, resultHandler);
    }

    /**
    * Find purchase order by ID
    * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
    * @param orderId ID of pet that needs to be fetched (required)
    * @param authInfo per call authentication override.
    * @param resultHandler Asynchronous result handler
    */
    public void getOrderById(Long orderId, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Order>> resultHandler) {
        Object localVarBody = null;
        
        // verify the required parameter 'orderId' is set
        if (orderId == null) {
            resultHandler.handle(ApiException.fail(400, "Missing the required parameter 'orderId' when calling getOrderById"));
            return;
        }
        
        // create path and map variables
        String localVarPath = "/store/order/{order_id}".replaceAll("\\{" + "order_id" + "\\}", encodeParameter(orderId.toString()));

        // query params
        List<Pair> localVarQueryParams = new ArrayList<>();

        // header params
        MultiMap localVarHeaderParams = MultiMap.caseInsensitiveMultiMap();
        
        // cookie params
        MultiMap localVarCookieParams = MultiMap.caseInsensitiveMultiMap();
        
        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        
        String[] localVarAccepts = { "application/xml", "application/json" };
        String[] localVarContentTypes = {  };
        String[] localVarAuthNames = new String[] {  };
        TypeReference<Order> localVarReturnType = new TypeReference<Order>() {};
        apiClient.invokeAPI(localVarPath, "GET", localVarQueryParams, localVarBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, authInfo, localVarReturnType, resultHandler);
    }
    /**
    * Place an order for a pet
    * 
        * @param body order placed for purchasing the pet (required)
    * @param resultHandler Asynchronous result handler
    */
    public void placeOrder(Order body, Handler<AsyncResult<Order>> resultHandler) {
        placeOrder(body, null, resultHandler);
    }

    /**
    * Place an order for a pet
    * 
    * @param body order placed for purchasing the pet (required)
    * @param authInfo per call authentication override.
    * @param resultHandler Asynchronous result handler
    */
    public void placeOrder(Order body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Order>> resultHandler) {
        Object localVarBody = body;
        
        // verify the required parameter 'body' is set
        if (body == null) {
            resultHandler.handle(ApiException.fail(400, "Missing the required parameter 'body' when calling placeOrder"));
            return;
        }
        
        // create path and map variables
        String localVarPath = "/store/order";

        // query params
        List<Pair> localVarQueryParams = new ArrayList<>();

        // header params
        MultiMap localVarHeaderParams = MultiMap.caseInsensitiveMultiMap();
        
        // cookie params
        MultiMap localVarCookieParams = MultiMap.caseInsensitiveMultiMap();
        
        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        
        String[] localVarAccepts = { "application/xml", "application/json" };
        String[] localVarContentTypes = {  };
        String[] localVarAuthNames = new String[] {  };
        TypeReference<Order> localVarReturnType = new TypeReference<Order>() {};
        apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, authInfo, localVarReturnType, resultHandler);
    }

    private String encodeParameter(String parameter) {
        try {
            return URLEncoder.encode(parameter, StandardCharsets.UTF_8.name());
        } catch (UnsupportedEncodingException e) {
            return parameter;
        }
    }
}
