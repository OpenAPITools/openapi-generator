package org.openapitools.client.api.rxjava;

import org.openapitools.client.model.Order;
import org.openapitools.client.ApiClient;

import java.util.*;

import rx.Single;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen")
public class StoreApi {

    private final org.openapitools.client.api.StoreApi delegate;

    public StoreApi(org.openapitools.client.api.StoreApi delegate) {
        this.delegate = delegate;
    }

    public org.openapitools.client.api.StoreApi getDelegate() {
        return delegate;
    }

    /**
    * Delete purchase order by ID
    * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
    * @param orderId ID of the order that needs to be deleted (required)
    * @param resultHandler Asynchronous result handler
    */
    public void deleteOrder(String orderId, Handler<AsyncResult<Void>> resultHandler) {
        delegate.deleteOrder(orderId, resultHandler);
    }

    /**
    * Delete purchase order by ID
    * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
    * @param orderId ID of the order that needs to be deleted (required)
    * @param authInfo call specific auth overrides
    * @param resultHandler Asynchronous result handler
    */
    public void deleteOrder(String orderId, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> resultHandler) {
        delegate.deleteOrder(orderId, authInfo, resultHandler);
    }

    /**
    * Delete purchase order by ID
    * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
    * @param orderId ID of the order that needs to be deleted (required)
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxDeleteOrder(String orderId) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.deleteOrder(orderId, fut)
        ));
    }

    /**
    * Delete purchase order by ID
    * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
    * @param orderId ID of the order that needs to be deleted (required)
    * @param authInfo call specific auth overrides
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Void> rxDeleteOrder(String orderId, ApiClient.AuthInfo authInfo) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.deleteOrder(orderId, authInfo, fut)
        ));
    }
    /**
    * Returns pet inventories by status
    * Returns a map of status codes to quantities
    * @param resultHandler Asynchronous result handler
    */
    public void getInventory(Handler<AsyncResult<Map<String, Integer>>> resultHandler) {
        delegate.getInventory(resultHandler);
    }

    /**
    * Returns pet inventories by status
    * Returns a map of status codes to quantities
    * @param authInfo call specific auth overrides
    * @param resultHandler Asynchronous result handler
    */
    public void getInventory(ApiClient.AuthInfo authInfo, Handler<AsyncResult<Map<String, Integer>>> resultHandler) {
        delegate.getInventory(authInfo, resultHandler);
    }

    /**
    * Returns pet inventories by status
    * Returns a map of status codes to quantities
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Map<String, Integer>> rxGetInventory() {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.getInventory(fut)
        ));
    }

    /**
    * Returns pet inventories by status
    * Returns a map of status codes to quantities
    * @param authInfo call specific auth overrides
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Map<String, Integer>> rxGetInventory(ApiClient.AuthInfo authInfo) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.getInventory(authInfo, fut)
        ));
    }
    /**
    * Find purchase order by ID
    * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
    * @param orderId ID of pet that needs to be fetched (required)
    * @param resultHandler Asynchronous result handler
    */
    public void getOrderById(Long orderId, Handler<AsyncResult<Order>> resultHandler) {
        delegate.getOrderById(orderId, resultHandler);
    }

    /**
    * Find purchase order by ID
    * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
    * @param orderId ID of pet that needs to be fetched (required)
    * @param authInfo call specific auth overrides
    * @param resultHandler Asynchronous result handler
    */
    public void getOrderById(Long orderId, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Order>> resultHandler) {
        delegate.getOrderById(orderId, authInfo, resultHandler);
    }

    /**
    * Find purchase order by ID
    * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
    * @param orderId ID of pet that needs to be fetched (required)
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Order> rxGetOrderById(Long orderId) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.getOrderById(orderId, fut)
        ));
    }

    /**
    * Find purchase order by ID
    * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
    * @param orderId ID of pet that needs to be fetched (required)
    * @param authInfo call specific auth overrides
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Order> rxGetOrderById(Long orderId, ApiClient.AuthInfo authInfo) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.getOrderById(orderId, authInfo, fut)
        ));
    }
    /**
    * Place an order for a pet
    * 
    * @param body order placed for purchasing the pet (required)
    * @param resultHandler Asynchronous result handler
    */
    public void placeOrder(Order body, Handler<AsyncResult<Order>> resultHandler) {
        delegate.placeOrder(body, resultHandler);
    }

    /**
    * Place an order for a pet
    * 
    * @param body order placed for purchasing the pet (required)
    * @param authInfo call specific auth overrides
    * @param resultHandler Asynchronous result handler
    */
    public void placeOrder(Order body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Order>> resultHandler) {
        delegate.placeOrder(body, authInfo, resultHandler);
    }

    /**
    * Place an order for a pet
    * 
    * @param body order placed for purchasing the pet (required)
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Order> rxPlaceOrder(Order body) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.placeOrder(body, fut)
        ));
    }

    /**
    * Place an order for a pet
    * 
    * @param body order placed for purchasing the pet (required)
    * @param authInfo call specific auth overrides
    * @return Asynchronous result handler (RxJava Single)
    */
    public Single<Order> rxPlaceOrder(Order body, ApiClient.AuthInfo authInfo) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut ->
            delegate.placeOrder(body, authInfo, fut)
        ));
    }

    public static StoreApi newInstance(org.openapitools.client.api.StoreApi arg) {
        return arg != null ? new StoreApi(arg) : null;
    }
}
