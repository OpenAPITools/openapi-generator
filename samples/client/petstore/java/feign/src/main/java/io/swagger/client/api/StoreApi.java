package io.swagger.client.api;

import io.swagger.client.ApiClient;

import java.util.Map;
import io.swagger.client.model.Order;

import java.util.*;
import feign.*;

@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaClientCodegen", date = "2016-01-11T21:48:33.457Z")
public interface StoreApi extends ApiClient.Api {


  /**
   * Returns pet inventories by status
   * Returns a map of status codes to quantities
   * @return Map<String, Integer>
   */
  @RequestLine("GET /store/inventory")
  @Headers({
    "Content-type: application/json",
    "Accepts: application/json",
  })
  Map<String, Integer> getInventory();
  
  /**
   * Place an order for a pet
   * 
   * @param body order placed for purchasing the pet
   * @return Order
   */
  @RequestLine("POST /store/order")
  @Headers({
    "Content-type: application/json",
    "Accepts: application/json",
  })
  Order placeOrder(Order body);
  
  /**
   * Find purchase order by ID
   * For valid response try integer IDs with value &lt;= 5 or &gt; 10. Other values will generated exceptions
   * @param orderId ID of pet that needs to be fetched
   * @return Order
   */
  @RequestLine("GET /store/order/{orderId}")
  @Headers({
    "Content-type: application/json",
    "Accepts: application/json",
  })
  Order getOrderById(@Param("orderId") String orderId);
  
  /**
   * Delete purchase order by ID
   * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
   * @param orderId ID of the order that needs to be deleted
   * @return void
   */
  @RequestLine("DELETE /store/order/{orderId}")
  @Headers({
    "Content-type: application/json",
    "Accepts: application/json",
  })
  void deleteOrder(@Param("orderId") String orderId);
  
}
