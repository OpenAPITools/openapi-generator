package io.swagger.client.api;

import io.swagger.client.ApiCallback;
import io.swagger.client.ApiClient;
import io.swagger.client.ApiException;
import io.swagger.client.Configuration;
import io.swagger.client.Pair;

import com.google.gson.reflect.TypeToken;

import com.squareup.okhttp.Call;

import java.util.Map;
import io.swagger.client.model.Order;

import java.lang.reflect.Type;
import java.util.*;

public class StoreApi {
  private ApiClient apiClient;

  public StoreApi() {
    this(Configuration.getDefaultApiClient());
  }

  public StoreApi(ApiClient apiClient) {
    this.apiClient = apiClient;
  }

  public ApiClient getApiClient() {
    return apiClient;
  }

  public void setApiClient(ApiClient apiClient) {
    this.apiClient = apiClient;
  }

  
  /* Build call for getInventory */
  private Call getInventoryCall() throws ApiException {
    Object postBody = null;
    

    // create path and map variables
    String path = "/store/inventory".replaceAll("\\{format\\}","json");

    List<Pair> queryParams = new ArrayList<Pair>();

    Map<String, String> headerParams = new HashMap<String, String>();

    Map<String, Object> formParams = new HashMap<String, Object>();

    final String[] accepts = {
      "application/json", "application/xml"
    };
    final String accept = apiClient.selectHeaderAccept(accepts);
    if (accept != null) headerParams.put("Accept", accept);

    final String[] contentTypes = {
      
    };
    final String contentType = apiClient.selectHeaderContentType(contentTypes);
    headerParams.put("Content-Type", contentType);

    String[] authNames = new String[] { "api_key" };
    return apiClient.buildCall(path, "GET", queryParams, postBody, headerParams, formParams, authNames);
  }

  /**
   * Returns pet inventories by status
   * Returns a map of status codes to quantities
   * @return Map<String, Integer>
   */
  public Map<String, Integer> getInventory() throws ApiException {
    Call call = getInventoryCall();
    Type returnType = new TypeToken<Map<String, Integer>>(){}.getType();
    return apiClient.execute(call, returnType);
  }

  /**
   * Returns pet inventories by status (asynchronously)
   * Returns a map of status codes to quantities
   * @param callback The callback to be executed when the API call finishes
   * @return The request call
   */
  public Call getInventoryAsync(ApiCallback<Map<String, Integer>> callback) throws ApiException {
    Call call = getInventoryCall();
    Type returnType = new TypeToken<Map<String, Integer>>(){}.getType();
    apiClient.executeAsync(call, returnType, callback);
    return call;
  }
  
  /* Build call for placeOrder */
  private Call placeOrderCall(Order body) throws ApiException {
    Object postBody = body;
    

    // create path and map variables
    String path = "/store/order".replaceAll("\\{format\\}","json");

    List<Pair> queryParams = new ArrayList<Pair>();

    Map<String, String> headerParams = new HashMap<String, String>();

    Map<String, Object> formParams = new HashMap<String, Object>();

    final String[] accepts = {
      "application/json", "application/xml"
    };
    final String accept = apiClient.selectHeaderAccept(accepts);
    if (accept != null) headerParams.put("Accept", accept);

    final String[] contentTypes = {
      
    };
    final String contentType = apiClient.selectHeaderContentType(contentTypes);
    headerParams.put("Content-Type", contentType);

    String[] authNames = new String[] {  };
    return apiClient.buildCall(path, "POST", queryParams, postBody, headerParams, formParams, authNames);
  }

  /**
   * Place an order for a pet
   * 
   * @param body order placed for purchasing the pet
   * @return Order
   */
  public Order placeOrder(Order body) throws ApiException {
    Call call = placeOrderCall(body);
    Type returnType = new TypeToken<Order>(){}.getType();
    return apiClient.execute(call, returnType);
  }

  /**
   * Place an order for a pet (asynchronously)
   * 
   * @param body order placed for purchasing the pet
   * @param callback The callback to be executed when the API call finishes
   * @return The request call
   */
  public Call placeOrderAsync(Order body, ApiCallback<Order> callback) throws ApiException {
    Call call = placeOrderCall(body);
    Type returnType = new TypeToken<Order>(){}.getType();
    apiClient.executeAsync(call, returnType, callback);
    return call;
  }
  
  /* Build call for getOrderById */
  private Call getOrderByIdCall(String orderId) throws ApiException {
    Object postBody = null;
    
    // verify the required parameter 'orderId' is set
    if (orderId == null) {
       throw new ApiException("Missing the required parameter 'orderId' when calling getOrderById(Async)");
    }
    

    // create path and map variables
    String path = "/store/order/{orderId}".replaceAll("\\{format\\}","json")
      .replaceAll("\\{" + "orderId" + "\\}", apiClient.escapeString(orderId.toString()));

    List<Pair> queryParams = new ArrayList<Pair>();

    Map<String, String> headerParams = new HashMap<String, String>();

    Map<String, Object> formParams = new HashMap<String, Object>();

    final String[] accepts = {
      "application/json", "application/xml"
    };
    final String accept = apiClient.selectHeaderAccept(accepts);
    if (accept != null) headerParams.put("Accept", accept);

    final String[] contentTypes = {
      
    };
    final String contentType = apiClient.selectHeaderContentType(contentTypes);
    headerParams.put("Content-Type", contentType);

    String[] authNames = new String[] {  };
    return apiClient.buildCall(path, "GET", queryParams, postBody, headerParams, formParams, authNames);
  }

  /**
   * Find purchase order by ID
   * For valid response try integer IDs with value &lt;= 5 or &gt; 10. Other values will generated exceptions
   * @param orderId ID of pet that needs to be fetched
   * @return Order
   */
  public Order getOrderById(String orderId) throws ApiException {
    Call call = getOrderByIdCall(orderId);
    Type returnType = new TypeToken<Order>(){}.getType();
    return apiClient.execute(call, returnType);
  }

  /**
   * Find purchase order by ID (asynchronously)
   * For valid response try integer IDs with value &lt;= 5 or &gt; 10. Other values will generated exceptions
   * @param orderId ID of pet that needs to be fetched
   * @param callback The callback to be executed when the API call finishes
   * @return The request call
   */
  public Call getOrderByIdAsync(String orderId, ApiCallback<Order> callback) throws ApiException {
    Call call = getOrderByIdCall(orderId);
    Type returnType = new TypeToken<Order>(){}.getType();
    apiClient.executeAsync(call, returnType, callback);
    return call;
  }
  
  /* Build call for deleteOrder */
  private Call deleteOrderCall(String orderId) throws ApiException {
    Object postBody = null;
    
    // verify the required parameter 'orderId' is set
    if (orderId == null) {
       throw new ApiException("Missing the required parameter 'orderId' when calling deleteOrder(Async)");
    }
    

    // create path and map variables
    String path = "/store/order/{orderId}".replaceAll("\\{format\\}","json")
      .replaceAll("\\{" + "orderId" + "\\}", apiClient.escapeString(orderId.toString()));

    List<Pair> queryParams = new ArrayList<Pair>();

    Map<String, String> headerParams = new HashMap<String, String>();

    Map<String, Object> formParams = new HashMap<String, Object>();

    final String[] accepts = {
      "application/json", "application/xml"
    };
    final String accept = apiClient.selectHeaderAccept(accepts);
    if (accept != null) headerParams.put("Accept", accept);

    final String[] contentTypes = {
      
    };
    final String contentType = apiClient.selectHeaderContentType(contentTypes);
    headerParams.put("Content-Type", contentType);

    String[] authNames = new String[] {  };
    return apiClient.buildCall(path, "DELETE", queryParams, postBody, headerParams, formParams, authNames);
  }

  /**
   * Delete purchase order by ID
   * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
   * @param orderId ID of the order that needs to be deleted
   */
  public void deleteOrder(String orderId) throws ApiException {
    Call call = deleteOrderCall(orderId);
    apiClient.execute(call);
  }

  /**
   * Delete purchase order by ID (asynchronously)
   * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
   * @param orderId ID of the order that needs to be deleted
   * @param callback The callback to be executed when the API call finishes
   * @return The request call
   */
  public Call deleteOrderAsync(String orderId, ApiCallback<Void> callback) throws ApiException {
    Call call = deleteOrderCall(orderId);
    apiClient.executeAsync(call, callback);
    return call;
  }
  
}
