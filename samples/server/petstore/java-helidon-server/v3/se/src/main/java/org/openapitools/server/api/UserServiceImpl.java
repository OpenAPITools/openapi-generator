package org.openapitools.server.api;

import io.helidon.webserver.Handler;
import java.util.List;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.time.OffsetDateTime;
import org.openapitools.server.model.User;
import java.util.logging.Logger;

import io.helidon.webserver.ServerRequest;
import io.helidon.webserver.ServerResponse;

public class UserServiceImpl implements UserService {

    private static final int HTTP_CODE_NOT_IMPLEMENTED = 501;
    private static final Logger LOGGER = Logger.getLogger(UserService.class.getName());
    private static final ObjectMapper MAPPER = JsonProvider.objectMapper();


    public void createUser(ServerRequest request, ServerResponse response, User user) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void createUsersWithArrayInput(ServerRequest request, ServerResponse response) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void createUsersWithListInput(ServerRequest request, ServerResponse response) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void deleteUser(ServerRequest request, ServerResponse response) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void getUserByName(ServerRequest request, ServerResponse response) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void loginUser(ServerRequest request, ServerResponse response) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void logoutUser(ServerRequest request, ServerResponse response) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void updateUser(ServerRequest request, ServerResponse response, User user) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

}
