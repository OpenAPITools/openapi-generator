package org.openapitools.client.api;

import org.openapitools.client.CollectionFormats.*;
import org.openapitools.client.model.Order;
import retrofit2.Response;
import retrofit2.http.*;

import java.util.Map;

public interface StoreApi {
  /**
   * Delete purchase order by ID
   * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
   * @param orderId ID of the order that needs to be deleted (required)
   * @return Call&lt;Void&gt;
   */
  @DELETE("store/order/{order_id}")
  CompletionStage<Response<Void>> deleteOrder(
    @retrofit2.http.Path("order_id") String orderId
  );

  /**
   * Returns pet inventories by status
   * Returns a map of status codes to quantities
   * @return Call&lt;Map&lt;String, Integer&gt;&gt;
   */
  @GET("store/inventory")
  CompletionStage<Response<Map<String, Integer>>> getInventory();


  /**
   * Find purchase order by ID
   * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
   * @param orderId ID of pet that needs to be fetched (required)
   * @return Call&lt;Order&gt;
   */
  @GET("store/order/{order_id}")
  CompletionStage<Response<Order>> getOrderById(
    @retrofit2.http.Path("order_id") Long orderId
  );

  /**
   * Place an order for a pet
   *
   * @param order order placed for purchasing the pet (required)
   * @return Call&lt;Order&gt;
   */
  @POST("store/order")
  CompletionStage<Response<Order>> placeOrder(
    @retrofit2.http.Body Order order
  );

}
