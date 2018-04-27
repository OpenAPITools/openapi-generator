package org.openapitools.server.api.verticle;

import org.openapitools.server.api.MainApiException;
import org.openapitools.server.api.model.Order;

public final class StoreApiException extends MainApiException {
    public StoreApiException(int statusCode, String statusMessage) {
        super(statusCode, statusMessage);
    }
    
    public static final StoreApiException Store_deleteOrder_400_Exception = new StoreApiException(400, "Invalid ID supplied");
    public static final StoreApiException Store_deleteOrder_404_Exception = new StoreApiException(404, "Order not found");
    public static final StoreApiException Store_getOrderById_400_Exception = new StoreApiException(400, "Invalid ID supplied");
    public static final StoreApiException Store_getOrderById_404_Exception = new StoreApiException(404, "Order not found");
    public static final StoreApiException Store_placeOrder_400_Exception = new StoreApiException(400, "Invalid Order");
    

}