package org.openapitools.server.api;

import io.helidon.common.GenericType;
import java.util.List;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.time.OffsetDateTime;
import java.util.Optional;
import org.openapitools.server.model.User;
import 
import io.helidon.common.Errors;
import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;
import jakarta.validation.ValidationException;
import org.openapitools.server.model.GenericTypes;

public class UserServiceImpl implements UserService {

    private static final int HTTP_CODE_NOT_IMPLEMENTED = 501;
    private static final System.Logger LOGGER = System.getLogger(UserService.class.getName());
    private static final ObjectMapper MAPPER = JsonProvider.objectMapper();


    public void createUser(ServerRequest request, ServerResponse response) {
        Errors.Collector errorsCollector = Errors.collector();
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        User user = request.content().as(User.class);


        Errors errors = errorsCollector.collect();
        errors.log(LOGGER);
        if (errors.hasFatal()) {
            throw new ValidationException("Validation errors: " + errors);
        }
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void createUsersWithArrayInput(ServerRequest request, ServerResponse response) {
        Errors.Collector errorsCollector = Errors.collector();
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        List<@Valid User> user = request.content().as(GenericTypes.TYPE__List_User);


        Errors errors = errorsCollector.collect();
        errors.log(LOGGER);
        if (errors.hasFatal()) {
            throw new ValidationException("Validation errors: " + errors);
        }
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void createUsersWithListInput(ServerRequest request, ServerResponse response) {
        Errors.Collector errorsCollector = Errors.collector();
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        List<@Valid User> user = request.content().as(GenericTypes.TYPE__List_User);


        Errors errors = errorsCollector.collect();
        errors.log(LOGGER);
        if (errors.hasFatal()) {
            throw new ValidationException("Validation errors: " + errors);
        }
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void deleteUser(ServerRequest request, ServerResponse response) {
        Errors.Collector errorsCollector = Errors.collector();
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        String username = request.path()
                .pathParameters()
                .first("username")
                .asOptional()
                .map(v -> validator.require("username", v))
                .orElse(null);


        Errors errors = errorsCollector.collect();
        errors.log(LOGGER);
        if (errors.hasFatal()) {
            throw new ValidationException("Validation errors: " + errors);
        }
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void getUserByName(ServerRequest request, ServerResponse response) {
        Errors.Collector errorsCollector = Errors.collector();
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        String username = request.path()
                .pathParameters()
                .first("username")
                .asOptional()
                .map(v -> validator.require("username", v))
                .orElse(null);


        Errors errors = errorsCollector.collect();
        errors.log(LOGGER);
        if (errors.hasFatal()) {
            throw new ValidationException("Validation errors: " + errors);
        }
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void loginUser(ServerRequest request, ServerResponse response) {
        Errors.Collector errorsCollector = Errors.collector();
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        String username = request.query()
                .first("username")
                .asOptional()
                .map(v -> validator.require("username", v))
                .orElse(null);

        String password = request.query()
                .first("password")
                .asOptional()
                .map(v -> validator.require("password", v))
                .orElse(null);


        Errors errors = errorsCollector.collect();
        errors.log(LOGGER);
        if (errors.hasFatal()) {
            throw new ValidationException("Validation errors: " + errors);
        }
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void logoutUser(ServerRequest request, ServerResponse response) {
        Errors.Collector errorsCollector = Errors.collector();
        ValidatorUtils.Validator validator = ValidatorUtils.validator();


        Errors errors = errorsCollector.collect();
        errors.log(LOGGER);
        if (errors.hasFatal()) {
            throw new ValidationException("Validation errors: " + errors);
        }
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void updateUser(ServerRequest request, ServerResponse response) {
        Errors.Collector errorsCollector = Errors.collector();
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        String username = request.path()
                .pathParameters()
                .first("username")
                .asOptional()
                .map(v -> validator.require("username", v))
                .orElse(null);

        User user = request.content().as(User.class);


        Errors errors = errorsCollector.collect();
        errors.log(LOGGER);
        if (errors.hasFatal()) {
            throw new ValidationException("Validation errors: " + errors);
        }
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    @Override
    public void afterStop() {
        System.out.println("Service UserService is down. Goodbye!");
    }
}
