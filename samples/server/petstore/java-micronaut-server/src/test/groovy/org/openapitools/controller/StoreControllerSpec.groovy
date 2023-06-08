package org.openapitools.controller

import org.openapitools.model.Order
import io.micronaut.test.extensions.spock.annotation.MicronautTest
import io.micronaut.http.client.HttpClient
import io.micronaut.http.client.annotation.Client
import io.micronaut.runtime.server.EmbeddedServer
import io.micronaut.http.HttpStatus
import io.micronaut.http.HttpRequest
import io.micronaut.http.MutableHttpRequest;
import io.micronaut.http.HttpResponse
import io.micronaut.http.MediaType
import io.micronaut.http.uri.UriTemplate
import io.micronaut.http.cookie.Cookie
import io.micronaut.http.client.multipart.MultipartBody
import io.micronaut.core.type.Argument
import jakarta.inject.Inject
import spock.lang.Specification
import spock.lang.Ignore
import reactor.core.publisher.Mono
import java.io.File
import java.io.FileReader


/**
 * Controller tests for StoreController
 */
@MicronautTest
class StoreControllerSpec extends Specification {

    @Inject
    EmbeddedServer server

    @Inject
    @Client('${context-path}')
    HttpClient client

    @Inject
    StoreController controller

    /**
     * This test is used to validate the implementation of deleteOrder() method
     *
     * The method should: Delete purchase order by ID
     *
     * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
     *
     * TODO fill in the parameters and test return value.
     */
    @Ignore("Not Implemented")
    def 'deleteOrder() method test'() {
        given:
        String orderId = 'example'

        when:
        controller.deleteOrder(orderId).block()

        then:
        true
    }

    /**
     * This test is used to check that the api available to client through
     * '/store/order/{orderId}' to the features of deleteOrder() works as desired.
     *
     * TODO fill in the request parameters and test response.
     */
    @Ignore("Not Implemented")
    def 'deleteOrder() test with client through path /store/order/{orderId}'() {
        given:
        var uri = UriTemplate.of('/store/order/{orderId}').expand([
            // Fill in the path variables
            'orderId': 'example'
        ])
        MutableHttpRequest request = HttpRequest.DELETE(uri)
            .accept('application/json')

        when:
        HttpResponse response = client.toBlocking().exchange(request); // To retrieve body you must specify required type (e.g. Map.class) as second argument 

        then:
        response.status() == HttpStatus.OK
    }

    /**
     * This test is used to validate the implementation of getInventory() method
     *
     * The method should: Returns pet inventories by status
     *
     * Returns a map of status codes to quantities
     *
     * TODO fill in the parameters and test return value.
     */
    @Ignore("Not Implemented")
    def 'getInventory() method test'() {
        given:

        when:
        Map<String, Integer> result = controller.getInventory().block()

        then:
        true
    }

    /**
     * This test is used to check that the api available to client through
     * '/store/inventory' to the features of getInventory() works as desired.
     *
     * TODO fill in the request parameters and test response.
     */
    @Ignore("Not Implemented")
    def 'getInventory() test with client through path /store/inventory'() {
        given:
        var uri = UriTemplate.of('/store/inventory').expand([:])
        MutableHttpRequest request = HttpRequest.GET(uri)
            .accept('application/json')

        when:
        HttpResponse response = client.toBlocking().exchange(request, Argument.of(Map.class, String.class, Integer.class));

        then:
        response.status() == HttpStatus.OK
    }

    /**
     * This test is used to validate the implementation of getOrderById() method
     *
     * The method should: Find purchase order by ID
     *
     * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
     *
     * TODO fill in the parameters and test return value.
     */
    @Ignore("Not Implemented")
    def 'getOrderById() method test'() {
        given:
        Long orderId = 56L

        when:
        Order result = controller.getOrderById(orderId).block()

        then:
        true
    }

    /**
     * This test is used to check that the api available to client through
     * '/store/order/{orderId}' to the features of getOrderById() works as desired.
     *
     * TODO fill in the request parameters and test response.
     */
    @Ignore("Not Implemented")
    def 'getOrderById() test with client through path /store/order/{orderId}'() {
        given:
        var uri = UriTemplate.of('/store/order/{orderId}').expand([
            // Fill in the path variables
            'orderId': 56L
        ])
        MutableHttpRequest request = HttpRequest.GET(uri)
            .accept('application/json')

        when:
        HttpResponse response = client.toBlocking().exchange(request, Order.class);

        then:
        response.status() == HttpStatus.OK
    }

    /**
     * This test is used to validate the implementation of placeOrder() method
     *
     * The method should: Place an order for a pet
     *
     * TODO fill in the parameters and test return value.
     */
    @Ignore("Not Implemented")
    def 'placeOrder() method test'() {
        given:
        Order order = new Order()

        when:
        Order result = controller.placeOrder(order).block()

        then:
        true
    }

    /**
     * This test is used to check that the api available to client through
     * '/store/order' to the features of placeOrder() works as desired.
     *
     * TODO fill in the request parameters and test response.
     */
    @Ignore("Not Implemented")
    def 'placeOrder() test with client through path /store/order'() {
        given:
        Order body = new Order()
        var uri = UriTemplate.of('/store/order').expand([:])
        MutableHttpRequest request = HttpRequest.POST(uri, body)
            .contentType('application/json')
            .accept('application/json')

        when:
        HttpResponse response = client.toBlocking().exchange(request, Order.class);

        then:
        response.status() == HttpStatus.OK
    }

}
