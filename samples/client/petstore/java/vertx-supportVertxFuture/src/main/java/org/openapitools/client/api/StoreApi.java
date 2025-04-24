package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import org.openapitools.client.model.Order;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.json.JsonObject;

import java.util.*;

public interface StoreApi {

    void deleteOrder(@javax.annotation.Nonnull String orderId, Handler<AsyncResult<Void>> handler);

    default Future<Void> deleteOrder(@javax.annotation.Nonnull String orderId){
        Promise<Void> promise = Promise.promise();
        deleteOrder(orderId, promise);
        return promise.future();
    }

    void deleteOrder(@javax.annotation.Nonnull String orderId, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    default Future<Void> deleteOrder(@javax.annotation.Nonnull String orderId, ApiClient.AuthInfo authInfo){
        Promise<Void> promise = Promise.promise();
        deleteOrder(orderId, authInfo, promise);
        return promise.future();
    }

    void getInventory(Handler<AsyncResult<Map<String, Integer>>> handler);

    default Future<Map<String, Integer>> getInventory(){
        Promise<Map<String, Integer>> promise = Promise.promise();
        getInventory(promise);
        return promise.future();
    }

    void getInventory(ApiClient.AuthInfo authInfo, Handler<AsyncResult<Map<String, Integer>>> handler);

    default Future<Map<String, Integer>> getInventory(ApiClient.AuthInfo authInfo){
        Promise<Map<String, Integer>> promise = Promise.promise();
        getInventory(authInfo, promise);
        return promise.future();
    }

    void getOrderById(@javax.annotation.Nonnull Long orderId, Handler<AsyncResult<Order>> handler);

    default Future<Order> getOrderById(@javax.annotation.Nonnull Long orderId){
        Promise<Order> promise = Promise.promise();
        getOrderById(orderId, promise);
        return promise.future();
    }

    void getOrderById(@javax.annotation.Nonnull Long orderId, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Order>> handler);

    default Future<Order> getOrderById(@javax.annotation.Nonnull Long orderId, ApiClient.AuthInfo authInfo){
        Promise<Order> promise = Promise.promise();
        getOrderById(orderId, authInfo, promise);
        return promise.future();
    }

    void placeOrder(@javax.annotation.Nonnull Order order, Handler<AsyncResult<Order>> handler);

    default Future<Order> placeOrder(@javax.annotation.Nonnull Order order){
        Promise<Order> promise = Promise.promise();
        placeOrder(order, promise);
        return promise.future();
    }

    void placeOrder(@javax.annotation.Nonnull Order order, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Order>> handler);

    default Future<Order> placeOrder(@javax.annotation.Nonnull Order order, ApiClient.AuthInfo authInfo){
        Promise<Order> promise = Promise.promise();
        placeOrder(order, authInfo, promise);
        return promise.future();
    }

}
