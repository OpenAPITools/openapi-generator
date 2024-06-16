package org.openapitools.server.api;

import java.util.Map;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.openapitools.server.model.Order;
import java.util.logging.Logger;

import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;
import org.openapitools.server.model.GenericTypes;

public class StoreServiceImpl implements StoreService {

    private static final int HTTP_CODE_NOT_IMPLEMENTED = 501;
    private static final Logger LOGGER = Logger.getLogger(StoreService.class.getName());
    private static final ObjectMapper MAPPER = JsonProvider.objectMapper();


    public void deleteOrder(ServerRequest request, ServerResponse response) {
        String orderId = ValidatorUtils.nonEmpty(request.path()
                .pathParameters()
                .first("order_id"));


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void getInventory(ServerRequest request, ServerResponse response) {

        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void getOrderById(ServerRequest request, ServerResponse response) {
        Long orderId = ValidatorUtils.nonEmpty(request.path()
                .pathParameters()
                .first("order_id")
                .map(Long::valueOf));


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void placeOrder(ServerRequest request, ServerResponse response) {
        Order order = request.content().as(Order.class);


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    @Override
    public void afterStop() {
        System.out.println("Service StoreService is down. Goodbye!");
    }
}
