package io.swagger.client.api;

import io.swagger.client.CollectionFormats.*;

import retrofit.Callback;
import retrofit.http.*;
import retrofit.mime.*;

import java.util.Map;
import io.swagger.client.model.Order;

import java.util.*;

public interface StoreApi {
  
  /**
   * Returns pet inventories by status
   * Sync method
   * Returns a map of status codes to quantities
   * @return Map<String, Integer>
   */
  
  @GET("/store/inventory")
  Map<String, Integer> getInventory();
    

  /**
   * Returns pet inventories by status
   * Async method
   * @param cb callback method
   * @return void
   */
  
  @GET("/store/inventory")
  void getInventory(
    Callback<Map<String, Integer>> cb
  );
  
  /**
   * Place an order for a pet
   * Sync method
   * 
   * @param body order placed for purchasing the pet
   * @return Order
   */
  
  @POST("/store/order")
  Order placeOrder(
    @Body Order body
  );

  /**
   * Place an order for a pet
   * Async method
   * @param body order placed for purchasing the pet
   * @param cb callback method
   * @return void
   */
  
  @POST("/store/order")
  void placeOrder(
    @Body Order body, Callback<Order> cb
  );
  
  /**
   * Find purchase order by ID
   * Sync method
   * For valid response try integer IDs with value &lt;= 5 or &gt; 10. Other values will generated exceptions
   * @param orderId ID of pet that needs to be fetched
   * @return Order
   */
  
  @GET("/store/order/{orderId}")
  Order getOrderById(
    @Path("orderId") String orderId
  );

  /**
   * Find purchase order by ID
   * Async method
   * @param orderId ID of pet that needs to be fetched
   * @param cb callback method
   * @return void
   */
  
  @GET("/store/order/{orderId}")
  void getOrderById(
    @Path("orderId") String orderId, Callback<Order> cb
  );
  
  /**
   * Delete purchase order by ID
   * Sync method
   * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
   * @param orderId ID of the order that needs to be deleted
   * @return Void
   */
  
  @DELETE("/store/order/{orderId}")
  Void deleteOrder(
    @Path("orderId") String orderId
  );

  /**
   * Delete purchase order by ID
   * Async method
   * @param orderId ID of the order that needs to be deleted
   * @param cb callback method
   * @return void
   */
  
  @DELETE("/store/order/{orderId}")
  void deleteOrder(
    @Path("orderId") String orderId, Callback<Void> cb
  );
  
}
