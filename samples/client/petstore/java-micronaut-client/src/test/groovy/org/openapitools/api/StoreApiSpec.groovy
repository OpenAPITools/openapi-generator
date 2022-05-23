package org.openapitools.api

import org.openapitools.model.Order
import io.micronaut.test.extensions.spock.annotation.MicronautTest
import spock.lang.Specification
import jakarta.inject.Inject
import spock.lang.Ignore
import java.util.Arrays
import java.util.ArrayList
import java.util.HashMap
import java.util.List
import java.util.Map
import java.util.HashSet


/**
 * API tests for StoreApi
 */
@MicronautTest
class StoreApiSpec extends Specification {

    @Inject
    StoreApi api

    
    /**
     * Delete purchase order by ID
     *
     * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
     */
    @Ignore("Not Implemented")
    void 'deleteOrder() test'() {
        given:
        String orderId = 'example'

        when:
        api.deleteOrder(orderId).block()

        then:
        true
        // TODO: test validations
    }

    
    /**
     * Returns pet inventories by status
     *
     * Returns a map of status codes to quantities
     */
    @Ignore("Not Implemented")
    void 'getInventory() test'() {
        given:

        when:
        Map<String, Integer> body = api.getInventory().block()

        then:
        true
        // TODO: test validations
    }

    
    /**
     * Find purchase order by ID
     *
     * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
     */
    @Ignore("Not Implemented")
    void 'getOrderById() test'() {
        given:
        Long orderId = 56L

        when:
        Order body = api.getOrderById(orderId).block()

        then:
        true
        // TODO: test validations
    }

    
    /**
     * Place an order for a pet
     */
    @Ignore("Not Implemented")
    void 'placeOrder() test'() {
        given:
        Order _body = new Order()

        when:
        Order body = api.placeOrder(_body).block()

        then:
        true
        // TODO: test validations
    }

    
}
