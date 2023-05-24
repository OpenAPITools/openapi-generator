package org.openapitools.client.api;

import org.openapitools.client.ApiException;
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiResponse;
import org.openapitools.client.Configuration;
import org.openapitools.client.Pair;

import javax.ws.rs.core.GenericType;

import org.openapitools.client.model.Order;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen")
public class StoreApi {
  private ApiClient apiClient;

  public StoreApi() {
    this(Configuration.getDefaultApiClient());
  }

  public StoreApi(ApiClient apiClient) {
    this.apiClient = apiClient;
  }

  /**
   * Get the API client
   *
   * @return API client
   */
  public ApiClient getApiClient() {
    return apiClient;
  }

  /**
   * Set the API client
   *
   * @param apiClient an instance of API client
   */
  public void setApiClient(ApiClient apiClient) {
    this.apiClient = apiClient;
  }

  /**
   * Delete purchase order by ID
   * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
   * @param orderId ID of the order that needs to be deleted (required)
   * @throws ApiException if fails to make API call
   * @http.response.details
     <table summary="Response Details" border="1">
       <tr><td> Status Code </td><td> Description </td><td> Response Headers </td></tr>
       <tr><td> 400 </td><td> Invalid ID supplied </td><td>  -  </td></tr>
       <tr><td> 404 </td><td> Order not found </td><td>  -  </td></tr>
     </table>
   */
  public void deleteOrder(String orderId) throws ApiException {
    deleteOrderWithHttpInfo(orderId);
  }

  /**
   * Delete purchase order by ID
   * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
   * @param orderId ID of the order that needs to be deleted (required)
   * @return ApiResponse&lt;Void&gt;
   * @throws ApiException if fails to make API call
   * @http.response.details
     <table summary="Response Details" border="1">
       <tr><td> Status Code </td><td> Description </td><td> Response Headers </td></tr>
       <tr><td> 400 </td><td> Invalid ID supplied </td><td>  -  </td></tr>
       <tr><td> 404 </td><td> Order not found </td><td>  -  </td></tr>
     </table>
   */
  public ApiResponse<Void> deleteOrderWithHttpInfo(String orderId) throws ApiException {
    // Check required parameters
    if (orderId == null) {
      throw new ApiException(400, "Missing the required parameter 'orderId' when calling deleteOrder");
    }

    // Path parameters
    String localVarPath = "/store/order/{order_id}"
            .replaceAll("\\{order_id}", apiClient.escapeString(orderId));

    String localVarAccept = apiClient.selectHeaderAccept();
    String localVarContentType = apiClient.selectHeaderContentType();
    return apiClient.invokeAPI("StoreApi.deleteOrder", localVarPath, "DELETE", new ArrayList<>(), null,
                               new LinkedHashMap<>(), new LinkedHashMap<>(), new LinkedHashMap<>(), localVarAccept, localVarContentType,
                               null, null, false);
  }
  /**
   * Returns pet inventories by status
   * Returns a map of status codes to quantities
   * @return Map&lt;String, Integer&gt;
   * @throws ApiException if fails to make API call
   * @http.response.details
     <table summary="Response Details" border="1">
       <tr><td> Status Code </td><td> Description </td><td> Response Headers </td></tr>
       <tr><td> 200 </td><td> successful operation </td><td>  -  </td></tr>
     </table>
   */
  public Map<String, Integer> getInventory() throws ApiException {
    return getInventoryWithHttpInfo().getData();
  }

  /**
   * Returns pet inventories by status
   * Returns a map of status codes to quantities
   * @return ApiResponse&lt;Map&lt;String, Integer&gt;&gt;
   * @throws ApiException if fails to make API call
   * @http.response.details
     <table summary="Response Details" border="1">
       <tr><td> Status Code </td><td> Description </td><td> Response Headers </td></tr>
       <tr><td> 200 </td><td> successful operation </td><td>  -  </td></tr>
     </table>
   */
  public ApiResponse<Map<String, Integer>> getInventoryWithHttpInfo() throws ApiException {
    String localVarAccept = apiClient.selectHeaderAccept("application/json");
    String localVarContentType = apiClient.selectHeaderContentType();
    String[] localVarAuthNames = new String[] {"api_key"};
    GenericType<Map<String, Integer>> localVarReturnType = new GenericType<Map<String, Integer>>() {};
    return apiClient.invokeAPI("StoreApi.getInventory", "/store/inventory", "GET", new ArrayList<>(), null,
                               new LinkedHashMap<>(), new LinkedHashMap<>(), new LinkedHashMap<>(), localVarAccept, localVarContentType,
                               localVarAuthNames, localVarReturnType, false);
  }
  /**
   * Find purchase order by ID
   * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generate exceptions
   * @param orderId ID of pet that needs to be fetched (required)
   * @return Order
   * @throws ApiException if fails to make API call
   * @http.response.details
     <table summary="Response Details" border="1">
       <tr><td> Status Code </td><td> Description </td><td> Response Headers </td></tr>
       <tr><td> 200 </td><td> successful operation </td><td>  -  </td></tr>
       <tr><td> 400 </td><td> Invalid ID supplied </td><td>  -  </td></tr>
       <tr><td> 404 </td><td> Order not found </td><td>  -  </td></tr>
     </table>
   */
  public Order getOrderById(Long orderId) throws ApiException {
    return getOrderByIdWithHttpInfo(orderId).getData();
  }

  /**
   * Find purchase order by ID
   * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generate exceptions
   * @param orderId ID of pet that needs to be fetched (required)
   * @return ApiResponse&lt;Order&gt;
   * @throws ApiException if fails to make API call
   * @http.response.details
     <table summary="Response Details" border="1">
       <tr><td> Status Code </td><td> Description </td><td> Response Headers </td></tr>
       <tr><td> 200 </td><td> successful operation </td><td>  -  </td></tr>
       <tr><td> 400 </td><td> Invalid ID supplied </td><td>  -  </td></tr>
       <tr><td> 404 </td><td> Order not found </td><td>  -  </td></tr>
     </table>
   */
  public ApiResponse<Order> getOrderByIdWithHttpInfo(Long orderId) throws ApiException {
    // Check required parameters
    if (orderId == null) {
      throw new ApiException(400, "Missing the required parameter 'orderId' when calling getOrderById");
    }

    // Path parameters
    String localVarPath = "/store/order/{order_id}"
            .replaceAll("\\{order_id}", apiClient.escapeString(orderId.toString()));

    String localVarAccept = apiClient.selectHeaderAccept("application/xml", "application/json");
    String localVarContentType = apiClient.selectHeaderContentType();
    GenericType<Order> localVarReturnType = new GenericType<Order>() {};
    return apiClient.invokeAPI("StoreApi.getOrderById", localVarPath, "GET", new ArrayList<>(), null,
                               new LinkedHashMap<>(), new LinkedHashMap<>(), new LinkedHashMap<>(), localVarAccept, localVarContentType,
                               null, localVarReturnType, false);
  }
  /**
   * Place an order for a pet
   * 
   * @param order order placed for purchasing the pet (required)
   * @return Order
   * @throws ApiException if fails to make API call
   * @http.response.details
     <table summary="Response Details" border="1">
       <tr><td> Status Code </td><td> Description </td><td> Response Headers </td></tr>
       <tr><td> 200 </td><td> successful operation </td><td>  -  </td></tr>
       <tr><td> 400 </td><td> Invalid Order </td><td>  -  </td></tr>
     </table>
   */
  public Order placeOrder(Order order) throws ApiException {
    return placeOrderWithHttpInfo(order).getData();
  }

  /**
   * Place an order for a pet
   * 
   * @param order order placed for purchasing the pet (required)
   * @return ApiResponse&lt;Order&gt;
   * @throws ApiException if fails to make API call
   * @http.response.details
     <table summary="Response Details" border="1">
       <tr><td> Status Code </td><td> Description </td><td> Response Headers </td></tr>
       <tr><td> 200 </td><td> successful operation </td><td>  -  </td></tr>
       <tr><td> 400 </td><td> Invalid Order </td><td>  -  </td></tr>
     </table>
   */
  public ApiResponse<Order> placeOrderWithHttpInfo(Order order) throws ApiException {
    // Check required parameters
    if (order == null) {
      throw new ApiException(400, "Missing the required parameter 'order' when calling placeOrder");
    }

    String localVarAccept = apiClient.selectHeaderAccept("application/xml", "application/json");
    String localVarContentType = apiClient.selectHeaderContentType("application/json");
    GenericType<Order> localVarReturnType = new GenericType<Order>() {};
    return apiClient.invokeAPI("StoreApi.placeOrder", "/store/order", "POST", new ArrayList<>(), order,
                               new LinkedHashMap<>(), new LinkedHashMap<>(), new LinkedHashMap<>(), localVarAccept, localVarContentType,
                               null, localVarReturnType, false);
  }
}
