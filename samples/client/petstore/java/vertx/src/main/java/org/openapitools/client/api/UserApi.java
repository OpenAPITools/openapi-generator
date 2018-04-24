package org.openapitools.client.api;

import org.openapitools.client.model.User;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;

import java.util.*;

public interface UserApi {

    void createUser(User user, Handler<AsyncResult<Void>> handler);

    void createUsersWithArrayInput(List<User> user, Handler<AsyncResult<Void>> handler);

    void createUsersWithListInput(List<User> user, Handler<AsyncResult<Void>> handler);

    void deleteUser(String username, Handler<AsyncResult<Void>> handler);

    void getUserByName(String username, Handler<AsyncResult<User>> handler);

    void loginUser(String username, String password, Handler<AsyncResult<String>> handler);

    void logoutUser(Handler<AsyncResult<Void>> handler);

    void updateUser(String username, User user, Handler<AsyncResult<Void>> handler);

}
