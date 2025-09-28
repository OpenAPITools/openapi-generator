package org.openapitools.example.api.interfaces;

import org.openapitools.example.model.Order;
import org.openapitools.example.model.*;
import java.util.List;
import java.util.Map;
import java.time.OffsetDateTime;
import java.time.LocalDate;
import java.time.LocalDateTime;
import javax.annotation.Generated;


@Generated(value = "org.openapitools.codegen.languages.JavaDubboServerCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")

public interface StoreService {

    /**
     * Delete purchase order by ID
     * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
     *
     * @param orderId ID of the order that needs to be deleted (required)
     * @return void
     */
    void deleteOrder(
        String orderId
    );

    /**
     * Returns pet inventories by status
     * Returns a map of status codes to quantities
     *
     * @return Map<String, Integer>
     */
    Map<String, Integer> getInventory(
    );

    /**
     * Find purchase order by ID
     * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generate exceptions
     *
     * @param orderId ID of pet that needs to be fetched (required)
     * @return Order
     */
    Order getOrderById(
        Long orderId
    );

    /**
     * Place an order for a pet
     * 
     *
     * @param order order placed for purchasing the pet (required)
     * @return Order
     */
    Order placeOrder(
        Order order
    );
}
