package org.openapitools.server.api;

import io.helidon.common.GenericType;
import java.util.List;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.time.OffsetDateTime;
import java.util.Optional;
import org.openapitools.server.model.User;

import io.helidon.webserver.http.HttpRules;
import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;
import io.helidon.webserver.http.HttpService;

public interface UserService extends HttpService { 

    /**
     * A service registers itself by updating the routing rules.
     * @param rules the routing rules.
     */
    @Override
    default void routing(HttpRules rules) {
        rules.post("/user", this::createUser);
        rules.post("/user/createWithArray", this::createUsersWithArrayInput);
        rules.post("/user/createWithList", this::createUsersWithListInput);
        rules.delete("/user/{username}", this::deleteUser);
        rules.get("/user/{username}", this::getUserByName);
        rules.get("/user/login", this::loginUser);
        rules.get("/user/logout", this::logoutUser);
        rules.put("/user/{username}", this::updateUser);
    }


    /**
     * POST /user : Create user.
     *
     * @param request the server request
     * @param response the server response
     */
    void createUser(ServerRequest request, ServerResponse response);


    /**
     * POST /user/createWithArray : Creates list of users with given input array.
     *
     * @param request the server request
     * @param response the server response
     */
    void createUsersWithArrayInput(ServerRequest request, ServerResponse response);


    /**
     * POST /user/createWithList : Creates list of users with given input array.
     *
     * @param request the server request
     * @param response the server response
     */
    void createUsersWithListInput(ServerRequest request, ServerResponse response);


    /**
     * DELETE /user/{username} : Delete user.
     *
     * @param request the server request
     * @param response the server response
     */
    void deleteUser(ServerRequest request, ServerResponse response);


    /**
     * GET /user/{username} : Get user by user name.
     *
     * @param request the server request
     * @param response the server response
     */
    void getUserByName(ServerRequest request, ServerResponse response);


    /**
     * GET /user/login : Logs user into the system.
     *
     * @param request the server request
     * @param response the server response
     */
    void loginUser(ServerRequest request, ServerResponse response);


    /**
     * GET /user/logout : Logs out current logged in user session.
     *
     * @param request the server request
     * @param response the server response
     */
    void logoutUser(ServerRequest request, ServerResponse response);


    /**
     * PUT /user/{username} : Updated user.
     *
     * @param request the server request
     * @param response the server response
     */
    void updateUser(ServerRequest request, ServerResponse response);


}
