package io.swagger.client.api;

import io.swagger.client.model.Order;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;

import java.util.*;

public interface StoreApi {

    void deleteOrder(String orderId, Handler<AsyncResult<Void>> handler);

    void getInventory(Handler<AsyncResult<Map<String, Integer>>> handler);

    void getOrderById(Long orderId, Handler<AsyncResult<Order>> handler);

    void placeOrder(Order body, Handler<AsyncResult<Order>> handler);

}
