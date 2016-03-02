package io.swagger.client.api;

import io.swagger.client.ApiClient;

import io.swagger.client.model.Order;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import feign.*;

@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaClientCodegen", date = "2016-03-02T21:04:56.888+08:00")
public interface StoreApi extends ApiClient.Api {


  /**
   * Finds orders by status
   * A single status value can be provided as a string
   * @param status Status value that needs to be considered for query
   * @return List<Order>
   */
  @RequestLine("GET /store/findByStatus?status={status}")
  @Headers({
    "Content-type: application/json",
    "Accepts: application/json",
  })
  List<Order> findOrdersByStatus(@Param("status") String status);
  
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
   * Returns pet inventories by status
   * Returns an arbitrary object which is actually a map of status codes to quantities
   * @return Object
   */
  @RequestLine("GET /store/inventory?response=arbitrary_object")
  @Headers({
    "Content-type: application/json",
    "Accepts: application/json",
  })
  Object getInventoryInObject();
  
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
