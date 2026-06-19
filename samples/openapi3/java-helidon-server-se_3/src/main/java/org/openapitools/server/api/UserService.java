package org.openapitools.server.api;

import io.helidon.webserver.Handler;
import java.util.List;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.time.OffsetDateTime;
import org.openapitools.server.model.User;

import io.helidon.webserver.Routing;
import io.helidon.webserver.ServerRequest;
import io.helidon.webserver.ServerResponse;
import io.helidon.webserver.Service;

public interface UserService extends Service { 

    /**
     * A service registers itself by updating the routing rules.
     * @param rules the routing rules.
     */
    @Override
    default void update(Routing.Rules rules) {
        rules.post("/user", Handler.create(User.class, this::createUser));
        rules.post("/user/createWithArray", this::createUsersWithArrayInput);
        rules.post("/user/createWithList", this::createUsersWithListInput);
        rules.delete("/user/{username}", this::deleteUser);
        rules.get("/user/{username}", this::getUserByName);
        rules.get("/user/login", this::loginUser);
        rules.get("/user/logout", this::logoutUser);
        rules.put("/user/{username}", Handler.create(User.class, this::updateUser));
    }


    /**
     * POST /user : Create user.
     * @param request the server request
     * @param response the server response
     * @param user Created user object 
     */
    void createUser(ServerRequest request, ServerResponse response, User user);

    /**
     * POST /user/createWithArray : Creates list of users with given input array.
     * @param request the server request
     * @param response the server response
     */
    void createUsersWithArrayInput(ServerRequest request, ServerResponse response);

    /**
     * POST /user/createWithList : Creates list of users with given input array.
     * @param request the server request
     * @param response the server response
     */
    void createUsersWithListInput(ServerRequest request, ServerResponse response);

    /**
     * DELETE /user/{username} : Delete user.
     * @param request the server request
     * @param response the server response
     */
    void deleteUser(ServerRequest request, ServerResponse response);

    /**
     * GET /user/{username} : Get user by user name.
     * @param request the server request
     * @param response the server response
     */
    void getUserByName(ServerRequest request, ServerResponse response);

    /**
     * GET /user/login : Logs user into the system.
     * @param request the server request
     * @param response the server response
     */
    void loginUser(ServerRequest request, ServerResponse response);

    /**
     * GET /user/logout : Logs out current logged in user session.
     * @param request the server request
     * @param response the server response
     */
    void logoutUser(ServerRequest request, ServerResponse response);

    /**
     * PUT /user/{username} : Updated user.
     * @param request the server request
     * @param response the server response
     * @param user Updated user object 
     */
    void updateUser(ServerRequest request, ServerResponse response, User user);

}
