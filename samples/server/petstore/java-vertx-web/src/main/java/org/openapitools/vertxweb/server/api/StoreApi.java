package org.openapitools.vertxweb.server.api;

import org.openapitools.vertxweb.server.model.Order;

import org.openapitools.vertxweb.server.ApiResponse;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

import java.util.List;
import java.util.Map;

public interface StoreApi  {
    Future<ApiResponse<Void>> deleteOrder(String orderId);
    Future<ApiResponse<Map<String, Integer>>> getInventory();
    Future<ApiResponse<Order>> getOrderById(Long orderId);
    Future<ApiResponse<Order>> placeOrder(Order order);
}
