package io.swagger.client.api.rxjava;

import io.swagger.client.model.Order;

import java.util.*;

import rx.Single;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;


public class StoreApi {

	private final io.swagger.client.api.StoreApi delegate;

	public StoreApi(io.swagger.client.api.StoreApi delegate) {
	    this.delegate = delegate;
    }

	public io.swagger.client.api.StoreApi getDelegate() {
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
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<Void> rxDeleteOrder(String orderId) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.deleteOrder(orderId, fut);
        }));
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
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<Map<String, Integer>> rxGetInventory() {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.getInventory(fut);
        }));
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
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<Order> rxGetOrderById(Long orderId) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.getOrderById(orderId, fut);
        }));
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
     * @return Asynchronous result handler (RxJava Single)
     */
    public Single<Order> rxPlaceOrder(Order body) {
        return Single.create(new io.vertx.rx.java.SingleOnSubscribeAdapter<>(fut -> {
            delegate.placeOrder(body, fut);
        }));
    }

    public static StoreApi newInstance(io.swagger.client.api.StoreApi arg) {
        return arg != null ? new StoreApi(arg) : null;
    }
}
