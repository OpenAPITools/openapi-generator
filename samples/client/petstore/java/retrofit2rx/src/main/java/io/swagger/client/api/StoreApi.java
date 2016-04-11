package io.swagger.client.api;

import io.swagger.client.CollectionFormats.*;

import rx.Observable;

import retrofit2.http.*;

import okhttp3.RequestBody;

import io.swagger.client.model.Order;



import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;



public interface StoreApi {
  
  /**
   * Delete purchase order by ID
   * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
   * @param orderId ID of the order that needs to be deleted (required)
   * @return Call<Void>
   */
  
  @DELETE("store/order/{orderId}")
  Observable<Void> deleteOrder(
    @Path("orderId") String orderId
  );

  
  /**
   * Finds orders by status
   * A single status value can be provided as a string
   * @param status Status value that needs to be considered for query (optional, default to placed)
   * @return Call<List<Order>>
   */
  
  @GET("store/findByStatus")
  Observable<List<Order>> findOrdersByStatus(
    @Query("status") String status
  );

  
  /**
   * Returns pet inventories by status
   * Returns a map of status codes to quantities
   * @return Call<Map<String, Integer>>
   */
  
  @GET("store/inventory")
  Observable<Map<String, Integer>> getInventory();
    

  
  /**
   * Fake endpoint to test arbitrary object return by &#39;Get inventory&#39;
   * Returns an arbitrary object which is actually a map of status codes to quantities
   * @return Call<Object>
   */
  
  @GET("store/inventory?response=arbitrary_object")
  Observable<Object> getInventoryInObject();
    

  
  /**
   * Find purchase order by ID
   * For valid response try integer IDs with value &lt;= 5 or &gt; 10. Other values will generated exceptions
   * @param orderId ID of pet that needs to be fetched (required)
   * @return Call<Order>
   */
  
  @GET("store/order/{orderId}")
  Observable<Order> getOrderById(
    @Path("orderId") String orderId
  );

  
  /**
   * Place an order for a pet
   * 
   * @param body order placed for purchasing the pet (optional)
   * @return Call<Order>
   */
  
  @POST("store/order")
  Observable<Order> placeOrder(
    @Body Order body
  );

  
}

