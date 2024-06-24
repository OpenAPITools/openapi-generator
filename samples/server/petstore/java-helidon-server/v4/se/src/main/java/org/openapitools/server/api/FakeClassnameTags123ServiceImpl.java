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
import org.openapitools.server.model.GenericTypes;

public class FakeClassnameTags123ServiceImpl implements FakeClassnameTags123Service {

    private static final int HTTP_CODE_NOT_IMPLEMENTED = 501;
    private static final System.Logger LOGGER = System.getLogger(FakeClassnameTags123Service.class.getName());
    private static final ObjectMapper MAPPER = JsonProvider.objectMapper();


    public void testClassname(ServerRequest request, ServerResponse response) {
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
        System.out.println("Service FakeClassnameTags123Service is down. Goodbye!");
    }
}
