package org.openapitools.api;

import java.util.Map;
import org.openapitools.model.Order;
import io.swagger.annotations.*;
import org.springframework.http.ResponseEntity;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;
import java.util.Map;

/**
 * A delegate to be called by the {@link StoreApiController}}.
 * Implement this interface with a {@link org.springframework.stereotype.Service} annotated class.
 */

public interface StoreApiDelegate {

    /**
     * @see StoreApi#deleteOrder
     */
    ResponseEntity<Void> deleteOrder(String orderId);

    /**
     * @see StoreApi#getInventory
     */
    ResponseEntity<Map<String, Integer>> getInventory();

    /**
     * @see StoreApi#getOrderById
     */
    ResponseEntity<Order> getOrderById(Long orderId);

    /**
     * @see StoreApi#placeOrder
     */
    ResponseEntity<Order> placeOrder(Order body);

}
