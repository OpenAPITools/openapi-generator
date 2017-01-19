package io.swagger.api;

import java.util.Map;
import io.swagger.model.Order;

import io.swagger.annotations.*;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

/**
 * A delegate to be called by the {@link StoreApiController}}.
 * Should be implemented as a controller but without the {@link org.springframework.stereotype.Controller} annotation.
 * Instead, use spring to autowire this class into the {@link StoreApiController}.
 */

public interface StoreApiDelegate {

    /**
     * @see StoreApi#deleteOrder
     */
    default ResponseEntity<Void> deleteOrder(String orderId) {
    // do some magic!
    return new ResponseEntity<Void>(HttpStatus.OK);
    }

    /**
     * @see StoreApi#getInventory
     */
    default ResponseEntity<Map<String, Integer>> getInventory() {
    // do some magic!
    return new ResponseEntity<Map<String, Integer>>(HttpStatus.OK);
    }

    /**
     * @see StoreApi#getOrderById
     */
    default ResponseEntity<Order> getOrderById(Long orderId) {
    // do some magic!
    return new ResponseEntity<Order>(HttpStatus.OK);
    }

    /**
     * @see StoreApi#placeOrder
     */
    default ResponseEntity<Order> placeOrder(Order body) {
    // do some magic!
    return new ResponseEntity<Order>(HttpStatus.OK);
    }

}
