package org.openapitools.server.api;

import org.openapitools.server.model.Client;
import io.helidon.webserver.Handler;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.logging.Logger;

import io.helidon.webserver.ServerRequest;
import io.helidon.webserver.ServerResponse;

public class AnotherFakeServiceImpl implements AnotherFakeService {

    private static final int HTTP_CODE_NOT_IMPLEMENTED = 501;
    private static final Logger LOGGER = Logger.getLogger(AnotherFakeService.class.getName());
    private static final ObjectMapper MAPPER = JsonProvider.objectMapper();


    public void call123testSpecialTags(ServerRequest request, ServerResponse response, Client client) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

}
