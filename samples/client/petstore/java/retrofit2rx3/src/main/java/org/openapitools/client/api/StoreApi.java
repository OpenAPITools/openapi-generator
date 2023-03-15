package org.openapitools.client.api;

import org.openapitools.client.CollectionFormats.*;

import io.reactivex.rxjava3.core.Observable;
import io.reactivex.rxjava3.core.Completable;
import retrofit2.http.*;

import okhttp3.RequestBody;
import okhttp3.ResponseBody;
import okhttp3.MultipartBody;

import org.openapitools.client.model.Order;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public interface StoreApi {
  /**
   * Delete purchase order by ID
   * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
   * @param orderId ID of the order that needs to be deleted (required)
   * @return Completable
   */
  @DELETE("store/order/{order_id}")
  Completable deleteOrder(
    @retrofit2.http.Path("order_id") String orderId
  );

  /**
   * Returns pet inventories by status
   * Returns a map of status codes to quantities
   * @return Observable&lt;Map&lt;String, Integer&gt;&gt;
   */
  @GET("store/inventory")
  Observable<Map<String, Integer>> getInventory();
    

  /**
   * Find purchase order by ID
   * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
   * @param orderId ID of pet that needs to be fetched (required)
   * @return Observable&lt;Order&gt;
   */
  @GET("store/order/{order_id}")
  Observable<Order> getOrderById(
    @retrofit2.http.Path("order_id") Long orderId
  );

  /**
   * Place an order for a pet
   * 
   * @param body order placed for purchasing the pet (required)
   * @return Observable&lt;Order&gt;
   */
  @POST("store/order")
  Observable<Order> placeOrder(
    @retrofit2.http.Body Order body
  );

}
