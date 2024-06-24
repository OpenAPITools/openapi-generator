package org.openapitools.server.api;

import org.openapitools.server.model.Client;
import com.fasterxml.jackson.databind.ObjectMapper;
import 
import io.helidon.common.Errors;
import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;
import jakarta.validation.ValidationException;

public class AnotherFakeServiceImpl implements AnotherFakeService {

    private static final int HTTP_CODE_NOT_IMPLEMENTED = 501;
    private static final System.Logger LOGGER = System.getLogger(AnotherFakeService.class.getName());
    private static final ObjectMapper MAPPER = JsonProvider.objectMapper();


    public void call123testSpecialTags(ServerRequest request, ServerResponse response) {
        Errors.Collector errorsCollector = Errors.collector();
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        Client client = request.content().as(Client.class);


        Errors errors = errorsCollector.collect();
        errors.log(LOGGER);
        if (errors.hasFatal()) {
            throw new ValidationException("Validation errors: " + errors);
        }
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    @Override
    public void afterStop() {
        System.out.println("Service AnotherFakeService is down. Goodbye!");
    }
}
