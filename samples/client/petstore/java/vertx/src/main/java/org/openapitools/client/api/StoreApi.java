package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import org.openapitools.client.model.Order;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;

import java.util.*;

public interface StoreApi {

    void deleteOrder(String orderId, Handler<AsyncResult<Void>> handler);

    void deleteOrder(String orderId, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void getInventory(Handler<AsyncResult<Map<String, Integer>>> handler);

    void getInventory(ApiClient.AuthInfo authInfo, Handler<AsyncResult<Map<String, Integer>>> handler);

    void getOrderById(Long orderId, Handler<AsyncResult<Order>> handler);

    void getOrderById(Long orderId, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Order>> handler);

    void placeOrder(Order body, Handler<AsyncResult<Order>> handler);

    void placeOrder(Order body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Order>> handler);

}
