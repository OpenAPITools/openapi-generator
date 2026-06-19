package org.openapitools.server.api;

import org.openapitools.server.model.Client;
import java.util.HexFormat;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.helidon.http.Status;
import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;

public class AnotherFakeServiceImpl implements AnotherFakeService {
    private static final ObjectMapper MAPPER = JsonProvider.objectMapper();


    @Override
    public void call123testSpecialTags(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }


    @Override
    public void afterStop() {
        System.out.println("Service AnotherFakeService is down. Goodbye!");
    }

}
