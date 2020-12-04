package org.openapitools.vertxweb.server.api;

import org.openapitools.vertxweb.server.model.Order;

import org.openapitools.vertxweb.server.ApiResponse;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.handler.impl.HttpStatusException;

import java.util.List;
import java.util.Map;

// Implement this class

public class StoreApiImpl implements StoreApi {
    public Future<ApiResponse<Void>> deleteOrder(String orderId) {
        return Future.failedFuture(new HttpStatusException(501));
    }

    public Future<ApiResponse<Map<String, Integer>>> getInventory() {
        return Future.failedFuture(new HttpStatusException(501));
    }

    public Future<ApiResponse<Order>> getOrderById(Long orderId) {
        return Future.failedFuture(new HttpStatusException(501));
    }

    public Future<ApiResponse<Order>> placeOrder(Order order) {
        return Future.failedFuture(new HttpStatusException(501));
    }

}
