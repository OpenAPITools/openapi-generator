package org.openapitools.server.api.verticle;

import org.openapitools.server.api.MainApiException;
import org.openapitools.server.api.model.Order;

import rx.Completable;
import rx.Single;

import java.util.List;
import java.util.Map;

public interface StoreApi  {
    //deleteOrder
    public Completable deleteOrder(String orderId);
    
    //getInventory
    public Single<Map<String, Integer>> getInventory();
    
    //getOrderById
    public Single<Order> getOrderById(Long orderId);
    
    //placeOrder
    public Single<Order> placeOrder(Order body);
    
}
