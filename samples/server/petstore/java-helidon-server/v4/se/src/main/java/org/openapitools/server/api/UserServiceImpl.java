package org.openapitools.server.api;

import io.helidon.common.GenericType;
import java.util.List;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.time.OffsetDateTime;
import java.util.Optional;
import org.openapitools.server.model.User;
import java.util.logging.Logger;

import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;
import org.openapitools.server.model.GenericTypes;

public class UserServiceImpl implements UserService {

    private static final int HTTP_CODE_NOT_IMPLEMENTED = 501;
    private static final Logger LOGGER = Logger.getLogger(UserService.class.getName());
    private static final ObjectMapper MAPPER = JsonProvider.objectMapper();


    public void createUser(ServerRequest request, ServerResponse response) {
        User user = request.content().as(User.class);


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void createUsersWithArrayInput(ServerRequest request, ServerResponse response) {
        List<@Valid User> user = request.content().as(GenericTypes.TYPE__List_User);


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void createUsersWithListInput(ServerRequest request, ServerResponse response) {
        List<@Valid User> user = request.content().as(GenericTypes.TYPE__List_User);


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void deleteUser(ServerRequest request, ServerResponse response) {
        String username = ValidatorUtils.nonEmpty(request.path()
                .pathParameters()
                .first("username"));


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void getUserByName(ServerRequest request, ServerResponse response) {
        String username = ValidatorUtils.nonEmpty(request.path()
                .pathParameters()
                .first("username"));


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void loginUser(ServerRequest request, ServerResponse response) {
        String username = ValidatorUtils.nonEmpty(request.query()
                .first("username"));

        String password = ValidatorUtils.nonEmpty(request.query()
                .first("password"));


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void logoutUser(ServerRequest request, ServerResponse response) {

        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void updateUser(ServerRequest request, ServerResponse response) {
        String username = ValidatorUtils.nonEmpty(request.path()
                .pathParameters()
                .first("username"));

        User user = request.content().as(User.class);


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    @Override
    public void afterStop() {
        System.out.println("Service UserService is down. Goodbye!");
    }
}
