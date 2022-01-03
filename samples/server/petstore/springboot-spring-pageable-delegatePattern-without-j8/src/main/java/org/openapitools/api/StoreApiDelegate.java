package org.openapitools.api;

import java.util.Map;
import org.openapitools.model.Order;
import org.springframework.http.ResponseEntity;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;
import java.util.Map;

/**
 * A delegate to be called by the {@link StoreApiController}}.
 * Implement this interface with a {@link org.springframework.stereotype.Service} annotated class.
 */
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public interface StoreApiDelegate {

    /**
     * DELETE /store/order/{order_id} : Delete purchase order by ID
     * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
     *
     * @param orderId ID of the order that needs to be deleted (required)
     * @return Invalid ID supplied (status code 400)
     *         or Order not found (status code 404)
     * @see StoreApi#deleteOrder
     */
    ResponseEntity<Void> deleteOrder(String orderId);

    /**
     * GET /store/inventory : Returns pet inventories by status
     * Returns a map of status codes to quantities
     *
     * @return successful operation (status code 200)
     * @see StoreApi#getInventory
     */
    ResponseEntity<Map<String, Integer>> getInventory();

    /**
     * GET /store/order/{order_id} : Find purchase order by ID
     * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
     *
     * @param orderId ID of pet that needs to be fetched (required)
     * @return successful operation (status code 200)
     *         or Invalid ID supplied (status code 400)
     *         or Order not found (status code 404)
     * @see StoreApi#getOrderById
     */
    ResponseEntity<Order> getOrderById(Long orderId);

    /**
     * POST /store/order : Place an order for a pet
     *
     * @param body order placed for purchasing the pet (required)
     * @return successful operation (status code 200)
     *         or Invalid Order (status code 400)
     * @see StoreApi#placeOrder
     */
    ResponseEntity<Order> placeOrder(Order body);

}
