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

import io.helidon.webserver.http.HttpRules;
import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;
import io.helidon.webserver.http.HttpService;

@io.helidon.common.Generated(value = "org.openapitools.codegen.languages.JavaHelidonServerCodegen",
                             trigger = "tag = 'User'",
                             version = "stable")
public interface UserService extends HttpService {

    /**
     * A service registers itself by updating the routing rules.
     * @param rules the routing rules.
     */
    @Override
    default void routing(HttpRules rules) {
        rules.post("/", this::createUser);
        rules.post("/createWithArray", this::createUsersWithArrayInput);
        rules.post("/createWithList", this::createUsersWithListInput);
        rules.delete("/{username}", this::deleteUser);
        rules.get("/{username}", this::getUserByName);
        rules.get("/login", this::loginUser);
        rules.get("/logout", this::logoutUser);
        rules.put("/{username}", this::updateUser);
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
