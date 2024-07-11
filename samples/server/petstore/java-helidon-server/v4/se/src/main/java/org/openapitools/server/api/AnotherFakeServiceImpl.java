package org.openapitools.server.api;

import org.openapitools.server.model.Client;
import java.util.HexFormat;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.helidon.http.Status;
import java.util.logging.Logger;import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;

public class AnotherFakeServiceImpl implements AnotherFakeService {

    private static final int HTTP_CODE_NOT_IMPLEMENTED = 501;
    private static final Logger LOGGER = Logger.getLogger(AnotherFakeService.class.getName());
    private static final ObjectMapper MAPPER = JsonProvider.objectMapper();


    @Override
    public void call123testSpecialTags(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }


    @Override
    public void afterStop() {
        System.out.println("Service AnotherFakeService is down. Goodbye!");
    }

}
