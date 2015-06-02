package io.swagger.client.api;

import io.swagger.client.model.*;

import retrofit.http.*;
import retrofit.mime.*;
import java.util.*;

import java.util.Map;
import io.swagger.client.model.Order;

public interface StoreApi {
  
  /**
   * Returns pet inventories by status
   * Returns a map of status codes to quantities
   * @return Map<String, Integer>
   */
  
  @GET("/store/inventory")  
  Map<String, Integer> getInventory();
      
  
  /**
   * Place an order for a pet
   * 
   * @param body order placed for purchasing the pet
   * @return Order
   */
  
  @POST("/store/order")  
  Order placeOrder(
    @Body Order body
  );  
  
  /**
   * Find purchase order by ID
   * For valid response try integer IDs with value &lt;= 5 or &gt; 10. Other values will generated exceptions
   * @param orderId ID of pet that needs to be fetched
   * @return Order
   */
  
  @GET("/store/order/{orderId}")  
  Order getOrderById(
    @Path("orderId") String orderId
  );  
  
  /**
   * Delete purchase order by ID
   * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
   * @param orderId ID of the order that needs to be deleted
   * @return Void
   */
  
  @DELETE("/store/order/{orderId}")  
  Void deleteOrder(
    @Path("orderId") String orderId
  );  
  
}
