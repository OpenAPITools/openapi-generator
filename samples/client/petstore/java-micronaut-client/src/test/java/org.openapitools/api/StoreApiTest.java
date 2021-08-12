package org.openapitools.api;

import org.openapitools.model.Order;
import io.micronaut.test.extensions.junit5.annotation.MicronautTest;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;
import jakarta.inject.Inject;
import reactor.core.publisher.Mono;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


/**
 * API tests for StoreApi
 */
@MicronautTest
public class StoreApiTest {

    @Inject
    StoreApi api;

    
    /**
     * Delete purchase order by ID
     *
     * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
     */
    @Test
    public void deleteOrderTest() {
        String orderId = null;
        // api.deleteOrder(orderId).block();
        // Mono<Void> asyncResponse = api.deleteOrder(orderId);
        // TODO: test validations
    }

    
    /**
     * Returns pet inventories by status
     *
     * Returns a map of status codes to quantities
     */
    @Test
    public void getInventoryTest() {
        // Map<String, Integer> response = api.getInventory().block();
        // Mono<Map<String, Integer>> asyncResponse = api.getInventory();
        // TODO: test validations
    }

    
    /**
     * Find purchase order by ID
     *
     * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
     */
    @Test
    public void getOrderByIdTest() {
        Long orderId = null;
        // Order response = api.getOrderById(orderId).block();
        // Mono<Order> asyncResponse = api.getOrderById(orderId);
        // TODO: test validations
    }

    
    /**
     * Place an order for a pet
     */
    @Test
    public void placeOrderTest() {
        Order _body = null;
        // Order response = api.placeOrder(_body).block();
        // Mono<Order> asyncResponse = api.placeOrder(_body);
        // TODO: test validations
    }

    
}
