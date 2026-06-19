package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import java.time.OffsetDateTime;
import org.openapitools.client.model.User;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;

import java.util.*;

public interface UserApi {

    void createUser(@javax.annotation.Nonnull User user, Handler<AsyncResult<Void>> handler);

    void createUser(@javax.annotation.Nonnull User user, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void createUsersWithArrayInput(@javax.annotation.Nonnull List<User> user, Handler<AsyncResult<Void>> handler);

    void createUsersWithArrayInput(@javax.annotation.Nonnull List<User> user, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void createUsersWithListInput(@javax.annotation.Nonnull List<User> user, Handler<AsyncResult<Void>> handler);

    void createUsersWithListInput(@javax.annotation.Nonnull List<User> user, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void deleteUser(@javax.annotation.Nonnull String username, Handler<AsyncResult<Void>> handler);

    void deleteUser(@javax.annotation.Nonnull String username, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void getUserByName(@javax.annotation.Nonnull String username, Handler<AsyncResult<User>> handler);

    void getUserByName(@javax.annotation.Nonnull String username, ApiClient.AuthInfo authInfo, Handler<AsyncResult<User>> handler);

    void loginUser(@javax.annotation.Nonnull String username, @javax.annotation.Nonnull String password, Handler<AsyncResult<String>> handler);

    void loginUser(@javax.annotation.Nonnull String username, @javax.annotation.Nonnull String password, ApiClient.AuthInfo authInfo, Handler<AsyncResult<String>> handler);

    void logoutUser(Handler<AsyncResult<Void>> handler);

    void logoutUser(ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    void updateUser(@javax.annotation.Nonnull String username, @javax.annotation.Nonnull User user, Handler<AsyncResult<Void>> handler);

    void updateUser(@javax.annotation.Nonnull String username, @javax.annotation.Nonnull User user, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

}
