package org.openapitools.server.api;

import io.helidon.webserver.Handler;
import java.util.List;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.time.OffsetDateTime;
import org.openapitools.server.model.User;

import java.util.Optional;
import java.util.logging.Logger;

import io.helidon.common.GenericType;
import io.helidon.common.reactive.Single;
import io.helidon.config.Config;
import io.helidon.webserver.Routing;
import io.helidon.webserver.ServerRequest;
import io.helidon.webserver.ServerResponse;
import io.helidon.webserver.Service;


public abstract class UserService implements Service { 

    protected static final Logger LOGGER = Logger.getLogger(UserService.class.getName());
    private static final ObjectMapper MAPPER = JsonProvider.objectMapper();

    protected final Config config;

    public UserService(Config config) {
        this.config = config;
    }

    /**
     * A service registers itself by updating the routing rules.
     * @param rules the routing rules.
     */
    @Override
    public void update(Routing.Rules rules) {
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
    void createUser(ServerRequest request, ServerResponse response, User user) { 
        Single.create(Single.empty())
            .thenAccept(val -> {
                ValidatorUtils.checkNonNull(user);
                
                handleCreateUser(request, response, user);
            })
            .exceptionally(throwable -> handleError(request, response, throwable));
    }
    /**
     * Handle POST /user : Create user.
     * @param request the server request
     * @param response the server response
     * @param user Created user object 
     */
    abstract void handleCreateUser(ServerRequest request, ServerResponse response, User user);


    /**
     * POST /user/createWithArray : Creates list of users with given input array.
     * @param request the server request
     * @param response the server response
     */
    void createUsersWithArrayInput(ServerRequest request, ServerResponse response) { 
        Single.create(request.content().as(new GenericType<List<User>>() { }))
            .thenAccept(user -> {
                ValidatorUtils.checkNonNull(user);
                
                handleCreateUsersWithArrayInput(request, response, user);
            })
            .exceptionally(throwable -> handleError(request, response, throwable));
    }
    /**
     * Handle POST /user/createWithArray : Creates list of users with given input array.
     * @param request the server request
     * @param response the server response
     * @param user List of user object 
     */
    abstract void handleCreateUsersWithArrayInput(ServerRequest request, ServerResponse response, List<User> user);


    /**
     * POST /user/createWithList : Creates list of users with given input array.
     * @param request the server request
     * @param response the server response
     */
    void createUsersWithListInput(ServerRequest request, ServerResponse response) { 
        Single.create(request.content().as(new GenericType<List<User>>() { }))
            .thenAccept(user -> {
                ValidatorUtils.checkNonNull(user);
                
                handleCreateUsersWithListInput(request, response, user);
            })
            .exceptionally(throwable -> handleError(request, response, throwable));
    }
    /**
     * Handle POST /user/createWithList : Creates list of users with given input array.
     * @param request the server request
     * @param response the server response
     * @param user List of user object 
     */
    abstract void handleCreateUsersWithListInput(ServerRequest request, ServerResponse response, List<User> user);


    /**
     * DELETE /user/{username} : Delete user.
     * @param request the server request
     * @param response the server response
     */
    void deleteUser(ServerRequest request, ServerResponse response) { 
        Single.create(Single.empty())
            .thenAccept(val -> {
                String username = Optional.ofNullable(request.path().param("username")).orElse(null);
                ValidatorUtils.checkNonNull(username);
                
                handleDeleteUser(request, response, username);
            })
            .exceptionally(throwable -> handleError(request, response, throwable));
    }
    /**
     * Handle DELETE /user/{username} : Delete user.
     * @param request the server request
     * @param response the server response
     * @param username The name that needs to be deleted 
     */
    abstract void handleDeleteUser(ServerRequest request, ServerResponse response, String username);


    /**
     * GET /user/{username} : Get user by user name.
     * @param request the server request
     * @param response the server response
     */
    void getUserByName(ServerRequest request, ServerResponse response) { 
        Single.create(Single.empty())
            .thenAccept(val -> {
                String username = Optional.ofNullable(request.path().param("username")).orElse(null);
                ValidatorUtils.checkNonNull(username);
                
                handleGetUserByName(request, response, username);
            })
            .exceptionally(throwable -> handleError(request, response, throwable));
    }
    /**
     * Handle GET /user/{username} : Get user by user name.
     * @param request the server request
     * @param response the server response
     * @param username The name that needs to be fetched. Use user1 for testing. 
     */
    abstract void handleGetUserByName(ServerRequest request, ServerResponse response, String username);


    /**
     * GET /user/login : Logs user into the system.
     * @param request the server request
     * @param response the server response
     */
    void loginUser(ServerRequest request, ServerResponse response) { 
        Single.create(Single.empty())
            .thenAccept(val -> {
                String username = request.queryParams().toMap().getOrDefault("username", List.of()).stream().findFirst().orElse(null);
                ValidatorUtils.checkNonNull(username);
                String password = request.queryParams().toMap().getOrDefault("password", List.of()).stream().findFirst().orElse(null);
                ValidatorUtils.checkNonNull(password);
                
                handleLoginUser(request, response, username, password);
            })
            .exceptionally(throwable -> handleError(request, response, throwable));
    }
    /**
     * Handle GET /user/login : Logs user into the system.
     * @param request the server request
     * @param response the server response
     * @param username The user name for login 
     * @param password The password for login in clear text 
     */
    abstract void handleLoginUser(ServerRequest request, ServerResponse response, String username, String password);


    /**
     * GET /user/logout : Logs out current logged in user session.
     * @param request the server request
     * @param response the server response
     */
    void logoutUser(ServerRequest request, ServerResponse response) { 
        Single.create(Single.empty())
            .thenAccept(val -> {
                
                handleLogoutUser(request, response);
            })
            .exceptionally(throwable -> handleError(request, response, throwable));
    }
    /**
     * Handle GET /user/logout : Logs out current logged in user session.
     * @param request the server request
     * @param response the server response
     */
    abstract void handleLogoutUser(ServerRequest request, ServerResponse response);


    /**
     * PUT /user/{username} : Updated user.
     * @param request the server request
     * @param response the server response
     * @param user Updated user object 
     */
    void updateUser(ServerRequest request, ServerResponse response, User user) { 
        Single.create(Single.empty())
            .thenAccept(val -> {
                String username = Optional.ofNullable(request.path().param("username")).orElse(null);
                ValidatorUtils.checkNonNull(username);
                ValidatorUtils.checkNonNull(user);
                
                handleUpdateUser(request, response, username, user);
            })
            .exceptionally(throwable -> handleError(request, response, throwable));
    }
    /**
     * Handle PUT /user/{username} : Updated user.
     * @param request the server request
     * @param response the server response
     * @param username name that need to be deleted 
     * @param user Updated user object 
     */
    abstract void handleUpdateUser(ServerRequest request, ServerResponse response, String username, User user);


    abstract Void handleError(ServerRequest request, ServerResponse response, Throwable throwable);
}
