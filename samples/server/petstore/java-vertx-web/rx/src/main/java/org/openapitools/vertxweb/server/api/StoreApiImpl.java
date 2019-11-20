package org.openapitools.vertxweb.server.api;

import org.openapitools.vertxweb.server.model.Order;

import org.openapitools.vertxweb.server.ApiResponse;
import org.openapitools.vertxweb.server.ApiException;

import io.reactivex.Single;

import java.util.List;
import java.util.Map;

// Implement this class

public class StoreApiImpl implements StoreApi {
    public Single<ApiResponse<Void>> deleteOrder(String orderId) {
        return Single.error(new ApiException("Not Implemented").setStatusCode(501));
    }

    public Single<ApiResponse<Map<String, Integer>>> getInventory() {
        return Single.error(new ApiException("Not Implemented").setStatusCode(501));
    }

    public Single<ApiResponse<Order>> getOrderById(Long orderId) {
        return Single.error(new ApiException("Not Implemented").setStatusCode(501));
    }

    public Single<ApiResponse<Order>> placeOrder(Order order) {
        return Single.error(new ApiException("Not Implemented").setStatusCode(501));
    }

}
