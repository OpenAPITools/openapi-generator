package org.openapitools.api;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.beans.factory.annotation.Autowired;
import java.util.Optional;
import javax.annotation.Generated;

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
@Controller
@RequestMapping("${openapi.openAPIPetstore.base-path:/v2}")
public class StoreApiController implements StoreApi {

    private final StoreApiDelegate delegate;

    public StoreApiController(@Autowired(required = false) StoreApiDelegate delegate) {
        this.delegate = Optional.ofNullable(delegate).orElse(new StoreApiDelegate() {});
    }

    @Override
    public StoreApiDelegate getDelegate() {
        return delegate;
    }

    /**
     * DELETE /store/order/{order_id} : Delete purchase order by ID
     * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
     *
     * @param orderId ID of the order that needs to be deleted (required)
     * @return Invalid ID supplied (status code 400)
     *         or Order not found (status code 404)
     * @see StoreApi#deleteOrder
     */
    public ResponseEntity<Void> deleteOrder(
        @Parameter(name = "order_id", description = "ID of the order that needs to be deleted", required = true) @PathVariable("order_id") String orderId
    ) {
        return delegate.deleteOrder(orderId);
    }

    /**
     * GET /store/inventory : Returns pet inventories by status
     * Returns a map of status codes to quantities
     *
     * @return successful operation (status code 200)
     * @see StoreApi#getInventory
     */
    public ResponseEntity<Map<String, Integer>> getInventory(
        
    ) {
        return delegate.getInventory();
    }

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
    public ResponseEntity<Order> getOrderById(
        @Min(1L) @Max(5L) @Parameter(name = "order_id", description = "ID of pet that needs to be fetched", required = true) @PathVariable("order_id") Long orderId
    ) {
        return delegate.getOrderById(orderId);
    }

    /**
     * POST /store/order : Place an order for a pet
     *
     * @param body order placed for purchasing the pet (required)
     * @return successful operation (status code 200)
     *         or Invalid Order (status code 400)
     * @see StoreApi#placeOrder
     */
    public ResponseEntity<Order> placeOrder(
        @Parameter(name = "body", description = "order placed for purchasing the pet", required = true) @Valid @RequestBody Order body
    ) {
        return delegate.placeOrder(body);
    }

}
