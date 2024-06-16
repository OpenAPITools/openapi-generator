package org.openapitools.server.api;

import org.openapitools.server.model.Client;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.logging.Logger;

import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;
import org.openapitools.server.model.GenericTypes;

public class FakeClassnameTags123ServiceImpl implements FakeClassnameTags123Service {

    private static final int HTTP_CODE_NOT_IMPLEMENTED = 501;
    private static final Logger LOGGER = Logger.getLogger(FakeClassnameTags123Service.class.getName());
    private static final ObjectMapper MAPPER = JsonProvider.objectMapper();


    public void testClassname(ServerRequest request, ServerResponse response) {
        Client client = request.content().as(Client.class);


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    @Override
    public void afterStop() {
        System.out.println("Service FakeClassnameTags123Service is down. Goodbye!");
    }
}
