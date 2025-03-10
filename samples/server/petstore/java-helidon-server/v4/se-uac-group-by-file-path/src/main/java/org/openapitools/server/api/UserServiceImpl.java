package org.openapitools.server.api;

import java.util.stream.Collectors;
import org.openapitools.server.model.GenericTypes;
import java.util.HexFormat;
import java.util.List;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.time.OffsetDateTime;
import java.util.Optional;
import io.helidon.http.Status;
import org.openapitools.server.model.User;
import jakarta.validation.Valid;
import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;

public class UserServiceImpl extends UserService {

    @Override
    protected void handleCreateUser(ServerRequest request, ServerResponse response, 
                User user) {

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    protected void handleCreateUsersWithArrayInput(ServerRequest request, ServerResponse response, 
                List<@Valid User> user) {

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    protected void handleCreateUsersWithListInput(ServerRequest request, ServerResponse response, 
                List<@Valid User> user) {

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    protected void handleDeleteUser(ServerRequest request, ServerResponse response, 
                String username) {

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    protected void handleGetUserByName(ServerRequest request, ServerResponse response, 
                String username) {

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    protected void handleLoginUser(ServerRequest request, ServerResponse response, 
                String username, 
                String password) {

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    protected void handleLogoutUser(ServerRequest request, ServerResponse response) {

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    protected void handleUpdateUser(ServerRequest request, ServerResponse response, 
                String username, 
                User user) {

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

}
