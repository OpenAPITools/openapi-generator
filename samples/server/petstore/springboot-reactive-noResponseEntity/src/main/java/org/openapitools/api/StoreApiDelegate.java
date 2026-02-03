package org.openapitools.api;

import springfox.documentation.annotations.ApiIgnore;
import java.util.Map;
import org.openapitools.model.Order;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.web.context.request.NativeWebRequest;
import org.springframework.web.multipart.MultipartFile;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import org.springframework.http.codec.multipart.Part;

import javax.validation.constraints.*;
import javax.validation.Valid;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import javax.annotation.Generated;

/**
 * A delegate to be called by the {@link StoreApiController}}.
 * Implement this interface with a {@link org.springframework.stereotype.Service} annotated class.
 */
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.20.0-SNAPSHOT")
public interface StoreApiDelegate {

    default Optional<NativeWebRequest> getRequest() {
        return Optional.empty();
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
    default Mono<Void> deleteOrder(String orderId) {
        Mono<Void> result = Mono.empty();


        return result.then(Mono.empty());

    }

    /**
     * GET /store/inventory : Returns pet inventories by status
     * Returns a map of status codes to quantities
     *
     * @return successful operation (status code 200)
     * @see StoreApi#getInventory
     */
    default Mono<Map<String, Integer>> getInventory() {
        Mono<Void> result = Mono.empty();


        return result.then(Mono.empty());

    }

    /**
     * GET /store/order/{order_id} : Find purchase order by ID
     * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generate exceptions
     *
     * @param orderId ID of pet that needs to be fetched (required)
     * @return successful operation (status code 200)
     *         or Invalid ID supplied (status code 400)
     *         or Order not found (status code 404)
     * @see StoreApi#getOrderById
     */
    default Mono<Order> getOrderById(Long orderId) {
        Mono<Void> result = Mono.empty();


        return result.then(Mono.empty());

    }

    /**
     * POST /store/order : Place an order for a pet
     * 
     *
     * @param order order placed for purchasing the pet (required)
     * @return successful operation (status code 200)
     *         or Invalid Order (status code 400)
     * @see StoreApi#placeOrder
     */
    default Mono<Order> placeOrder(Mono<Order> order) {
        Mono<Void> result = Mono.empty();


        return result.then(order).then(Mono.empty());

    }

}
