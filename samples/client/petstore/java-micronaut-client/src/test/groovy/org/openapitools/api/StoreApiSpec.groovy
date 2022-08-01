package org.openapitools.api

import io.micronaut.http.client.exceptions.HttpClientResponseException
import org.openapitools.model.Order
import io.micronaut.test.extensions.spock.annotation.MicronautTest
import spock.lang.Ignore
import spock.lang.Specification
import jakarta.inject.Inject
import java.time.LocalDateTime


/**
 * API tests for StoreApi
 */
@MicronautTest
public class StoreApiSpec extends Specification {

    @Inject
    StoreApi api

    /**
     * Place an order for a pet
     */
    @Ignore("The test fails because the produces is specified as */* instead of a specific value")
    void "placeOrder() test"() {
        given:
        Order order = new Order().id(1L).shipDate(LocalDateTime.MIN).complete(true)
                .petId(5L).quantity(1).status(Order.StatusEnum.DELIVERED)

        when:
        Order response = api.placeOrder(order).block()

        then:
        order == response
    }

    /**
     * Returns pet inventories by status
     * Returns a map of status codes to quantities
     */
    void "getInventory() test"() {
        when:
        Map<String, Integer> response = api.getInventory().block()

        then:
        response != null
    }

    /**
     * Find purchase order by ID
     * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
     */
    void "getOrderById() test"() {
        when:
        Long orderId = 4L
        Order response = api.getOrderById(orderId).block()

        then:
        response != null
        orderId == response.getId()
    }


    /**
     * Delete purchase order by ID
     *
     * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
     */
    void "deleteOrder() not existing order error test"() {
        given:
        String orderId = "1001"

        when:
        api.deleteOrder(orderId).block()

        then:
        thrown(HttpClientResponseException)
    }
}
