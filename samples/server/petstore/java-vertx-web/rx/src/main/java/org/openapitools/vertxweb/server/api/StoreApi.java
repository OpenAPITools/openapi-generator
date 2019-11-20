package org.openapitools.vertxweb.server.api;

import org.openapitools.vertxweb.server.model.Order;

import org.openapitools.vertxweb.server.ApiResponse;

import io.reactivex.Single;

import java.util.List;
import java.util.Map;

public interface StoreApi  {
    Single<ApiResponse<Void>> deleteOrder(String orderId);
    Single<ApiResponse<Map<String, Integer>>> getInventory();
    Single<ApiResponse<Order>> getOrderById(Long orderId);
    Single<ApiResponse<Order>> placeOrder(Order order);
}
