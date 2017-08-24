package io.swagger.client.api;

import io.swagger.client.model.User;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;

import java.util.*;

public interface UserApi {

    void createUser(User body, Handler<AsyncResult<Void>> handler);

    void createUsersWithArrayInput(List<User> body, Handler<AsyncResult<Void>> handler);

    void createUsersWithListInput(List<User> body, Handler<AsyncResult<Void>> handler);

    void deleteUser(String username, Handler<AsyncResult<Void>> handler);

    void getUserByName(String username, Handler<AsyncResult<User>> handler);

    void loginUser(String username, String password, Handler<AsyncResult<String>> handler);

    void logoutUser(Handler<AsyncResult<Void>> handler);

    void updateUser(String username, User body, Handler<AsyncResult<Void>> handler);

}
