package org.openapitools.server.api;

import io.helidon.webserver.Handler;
import java.util.List;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.time.OffsetDateTime;
import org.openapitools.server.model.User;
import io.helidon.webserver.ServerRequest;
import io.helidon.webserver.ServerResponse;

public class UserServiceImpl extends UserService {

    private static final int HTTP_CODE_NOT_IMPLEMENTED = 501;

    public void handleCreateUser(ServerRequest request, ServerResponse response, User user) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void handleCreateUsersWithArrayInput(ServerRequest request, ServerResponse response, List<@Valid User> user) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void handleCreateUsersWithListInput(ServerRequest request, ServerResponse response, List<@Valid User> user) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void handleDeleteUser(ServerRequest request, ServerResponse response, String username) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void handleGetUserByName(ServerRequest request, ServerResponse response, String username) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void handleLoginUser(ServerRequest request, ServerResponse response, String username, String password) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void handleLogoutUser(ServerRequest request, ServerResponse response) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void handleUpdateUser(ServerRequest request, ServerResponse response, String username, User user) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }


    public Void handleError(ServerRequest request, ServerResponse response, Throwable throwable) {
        return response.send(throwable);
    }
}
