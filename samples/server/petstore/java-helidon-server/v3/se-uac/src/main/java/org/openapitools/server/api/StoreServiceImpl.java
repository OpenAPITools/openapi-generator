package org.openapitools.server.api;

import io.helidon.webserver.Handler;
import java.util.Map;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.openapitools.server.model.Order;
import io.helidon.webserver.ServerRequest;
import io.helidon.webserver.ServerResponse;

public class StoreServiceImpl extends StoreService {

    private static final int HTTP_CODE_NOT_IMPLEMENTED = 501;

    public void handleDeleteOrder(ServerRequest request, ServerResponse response, String orderId) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void handleGetInventory(ServerRequest request, ServerResponse response) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void handleGetOrderById(ServerRequest request, ServerResponse response, Long orderId) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void handlePlaceOrder(ServerRequest request, ServerResponse response, Order order) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }


    public Void handleError(ServerRequest request, ServerResponse response, Throwable throwable) {
        return response.send(throwable);
    }
}
