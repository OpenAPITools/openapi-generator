package org.openapitools.server.api;

import java.util.HexFormat;
import java.util.Map;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.openapitools.server.model.Order;
import io.helidon.http.Status;
import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;

public class StoreServiceImpl extends StoreService {

    private static final int HTTP_CODE_NOT_IMPLEMENTED = 501;

    @Override
    protected void handleDeleteOrder(ServerRequest request, ServerResponse response, 
                String orderId) {


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    @Override
    protected void handleGetInventory(ServerRequest request, ServerResponse response) {


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    @Override
    protected void handleGetOrderById(ServerRequest request, ServerResponse response, 
                Long orderId) {


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    @Override
    protected void handlePlaceOrder(ServerRequest request, ServerResponse response, 
                Order order) {


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

}
