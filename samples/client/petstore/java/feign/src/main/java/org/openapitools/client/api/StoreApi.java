package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import org.openapitools.client.EncodingUtils;
import org.openapitools.client.model.ApiResponse;

import org.openapitools.client.model.Order;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import feign.*;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen")
public interface StoreApi extends ApiClient.Api {


  /**
   * Delete purchase order by ID
   * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
   * @param orderId ID of the order that needs to be deleted (required)
   */
  @RequestLine("DELETE /store/order/{orderId}")
  @Headers({
    "Accept: application/json",
  })
  void deleteOrder(@Param("orderId") String orderId);

  /**
   * Delete purchase order by ID
   * Similar to <code>deleteOrder</code> but it also returns the http response headers .
   * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
   * @param orderId ID of the order that needs to be deleted (required)
   */
  @RequestLine("DELETE /store/order/{orderId}")
  @Headers({
    "Accept: application/json",
  })
  ApiResponse<Void> deleteOrderWithHttpInfo(@Param("orderId") String orderId);



  /**
   * Returns pet inventories by status
   * Returns a map of status codes to quantities
   * @return Map&lt;String, Integer&gt;
   */
  @RequestLine("GET /store/inventory")
  @Headers({
    "Accept: application/json",
  })
  Map<String, Integer> getInventory();

  /**
   * Returns pet inventories by status
   * Similar to <code>getInventory</code> but it also returns the http response headers .
   * Returns a map of status codes to quantities
   * @return A ApiResponse that wraps the response boyd and the http headers.
   */
  @RequestLine("GET /store/inventory")
  @Headers({
    "Accept: application/json",
  })
  ApiResponse<Map<String, Integer>> getInventoryWithHttpInfo();



  /**
   * Find purchase order by ID
   * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
   * @param orderId ID of pet that needs to be fetched (required)
   * @return Order
   */
  @RequestLine("GET /store/order/{orderId}")
  @Headers({
    "Accept: application/json",
  })
  Order getOrderById(@Param("orderId") Long orderId);

  /**
   * Find purchase order by ID
   * Similar to <code>getOrderById</code> but it also returns the http response headers .
   * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
   * @param orderId ID of pet that needs to be fetched (required)
   * @return A ApiResponse that wraps the response boyd and the http headers.
   */
  @RequestLine("GET /store/order/{orderId}")
  @Headers({
    "Accept: application/json",
  })
  ApiResponse<Order> getOrderByIdWithHttpInfo(@Param("orderId") Long orderId);



  /**
   * Place an order for a pet
   * 
   * @param order order placed for purchasing the pet (required)
   * @return Order
   */
  @RequestLine("POST /store/order")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  Order placeOrder(Order order);

  /**
   * Place an order for a pet
   * Similar to <code>placeOrder</code> but it also returns the http response headers .
   * 
   * @param order order placed for purchasing the pet (required)
   * @return A ApiResponse that wraps the response boyd and the http headers.
   */
  @RequestLine("POST /store/order")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  ApiResponse<Order> placeOrderWithHttpInfo(Order order);


}
