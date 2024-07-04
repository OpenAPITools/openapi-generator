package org.openapitools.server.api;

import io.helidon.webserver.Handler;
import java.util.Map;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.openapitools.server.model.Order;
import java.util.logging.Logger;

import io.helidon.webserver.ServerRequest;
import io.helidon.webserver.ServerResponse;

public class StoreServiceImpl implements StoreService {

    private static final int HTTP_CODE_NOT_IMPLEMENTED = 501;
    private static final Logger LOGGER = Logger.getLogger(StoreService.class.getName());
    private static final ObjectMapper MAPPER = JsonProvider.objectMapper();


    public void deleteOrder(ServerRequest request, ServerResponse response) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void getInventory(ServerRequest request, ServerResponse response) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void getOrderById(ServerRequest request, ServerResponse response) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void placeOrder(ServerRequest request, ServerResponse response, Order order) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

}
