package org.openapitools.server.api;

import java.util.HexFormat;
import java.util.Map;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.openapitools.server.model.Order;
import io.helidon.http.Status;
import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;

public class StoreServiceImpl implements StoreService {

    private static final int HTTP_CODE_NOT_IMPLEMENTED = 501;
    private static final ObjectMapper MAPPER = JsonProvider.objectMapper();


    @Override
    public void deleteOrder(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator();


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    @Override
    public void getInventory(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator();


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    @Override
    public void getOrderById(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator();


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    @Override
    public void placeOrder(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator();


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }


    @Override
    public void afterStop() {
        System.out.println("Service StoreService is down. Goodbye!");
    }

}
