package io.swagger.client.api;

import io.swagger.client.CollectionFormats.*;


import retrofit2.Call;
import retrofit2.http.*;

import okhttp3.RequestBody;

import java.util.Map;
import io.swagger.client.model.Order;

import java.util.*;

public interface StoreApi {
  
  /**
   * Returns pet inventories by status
   * Returns a map of status codes to quantities
   * @return Call<Map<String, Integer>>
   */
  
  @GET("store/inventory")
  Call<Map<String, Integer>> getInventory();
    

  
  /**
   * Place an order for a pet
   * 
   * @param body order placed for purchasing the pet
   * @return Call<Order>
   */
  
  @POST("store/order")
  Call<Order> placeOrder(
    @Body Order body
  );

  
  /**
   * Find purchase order by ID
   * For valid response try integer IDs with value &lt;= 5 or &gt; 10. Other values will generated exceptions
   * @param orderId ID of pet that needs to be fetched
   * @return Call<Order>
   */
  
  @GET("store/order/{orderId}")
  Call<Order> getOrderById(
    @Path("orderId") String orderId
  );

  
  /**
   * Delete purchase order by ID
   * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
   * @param orderId ID of the order that needs to be deleted
   * @return Call<Void>
   */
  
  @DELETE("store/order/{orderId}")
  Call<Void> deleteOrder(
    @Path("orderId") String orderId
  );

  
}
