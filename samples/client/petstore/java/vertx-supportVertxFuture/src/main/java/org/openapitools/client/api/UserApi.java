package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import java.time.OffsetDateTime;
import org.openapitools.client.model.User;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.json.JsonObject;

import java.util.*;

public interface UserApi {

    void createUser(@javax.annotation.Nonnull User user, Handler<AsyncResult<Void>> handler);

    default Future<Void> createUser(@javax.annotation.Nonnull User user){
        Promise<Void> promise = Promise.promise();
        createUser(user, promise);
        return promise.future();
    }

    void createUser(@javax.annotation.Nonnull User user, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    default Future<Void> createUser(@javax.annotation.Nonnull User user, ApiClient.AuthInfo authInfo){
        Promise<Void> promise = Promise.promise();
        createUser(user, authInfo, promise);
        return promise.future();
    }

    void createUsersWithArrayInput(@javax.annotation.Nonnull List<User> user, Handler<AsyncResult<Void>> handler);

    default Future<Void> createUsersWithArrayInput(@javax.annotation.Nonnull List<User> user){
        Promise<Void> promise = Promise.promise();
        createUsersWithArrayInput(user, promise);
        return promise.future();
    }

    void createUsersWithArrayInput(@javax.annotation.Nonnull List<User> user, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    default Future<Void> createUsersWithArrayInput(@javax.annotation.Nonnull List<User> user, ApiClient.AuthInfo authInfo){
        Promise<Void> promise = Promise.promise();
        createUsersWithArrayInput(user, authInfo, promise);
        return promise.future();
    }

    void createUsersWithListInput(@javax.annotation.Nonnull List<User> user, Handler<AsyncResult<Void>> handler);

    default Future<Void> createUsersWithListInput(@javax.annotation.Nonnull List<User> user){
        Promise<Void> promise = Promise.promise();
        createUsersWithListInput(user, promise);
        return promise.future();
    }

    void createUsersWithListInput(@javax.annotation.Nonnull List<User> user, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    default Future<Void> createUsersWithListInput(@javax.annotation.Nonnull List<User> user, ApiClient.AuthInfo authInfo){
        Promise<Void> promise = Promise.promise();
        createUsersWithListInput(user, authInfo, promise);
        return promise.future();
    }

    void deleteUser(@javax.annotation.Nonnull String username, Handler<AsyncResult<Void>> handler);

    default Future<Void> deleteUser(@javax.annotation.Nonnull String username){
        Promise<Void> promise = Promise.promise();
        deleteUser(username, promise);
        return promise.future();
    }

    void deleteUser(@javax.annotation.Nonnull String username, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    default Future<Void> deleteUser(@javax.annotation.Nonnull String username, ApiClient.AuthInfo authInfo){
        Promise<Void> promise = Promise.promise();
        deleteUser(username, authInfo, promise);
        return promise.future();
    }

    void getUserByName(@javax.annotation.Nonnull String username, Handler<AsyncResult<User>> handler);

    default Future<User> getUserByName(@javax.annotation.Nonnull String username){
        Promise<User> promise = Promise.promise();
        getUserByName(username, promise);
        return promise.future();
    }

    void getUserByName(@javax.annotation.Nonnull String username, ApiClient.AuthInfo authInfo, Handler<AsyncResult<User>> handler);

    default Future<User> getUserByName(@javax.annotation.Nonnull String username, ApiClient.AuthInfo authInfo){
        Promise<User> promise = Promise.promise();
        getUserByName(username, authInfo, promise);
        return promise.future();
    }

    void loginUser(@javax.annotation.Nonnull String username, @javax.annotation.Nonnull String password, Handler<AsyncResult<String>> handler);

    default Future<String> loginUser(@javax.annotation.Nonnull String username, @javax.annotation.Nonnull String password){
        Promise<String> promise = Promise.promise();
        loginUser(username, password, promise);
        return promise.future();
    }

    void loginUser(@javax.annotation.Nonnull String username, @javax.annotation.Nonnull String password, ApiClient.AuthInfo authInfo, Handler<AsyncResult<String>> handler);

    default Future<String> loginUser(@javax.annotation.Nonnull String username, @javax.annotation.Nonnull String password, ApiClient.AuthInfo authInfo){
        Promise<String> promise = Promise.promise();
        loginUser(username, password, authInfo, promise);
        return promise.future();
    }

    void logoutUser(Handler<AsyncResult<Void>> handler);

    default Future<Void> logoutUser(){
        Promise<Void> promise = Promise.promise();
        logoutUser(promise);
        return promise.future();
    }

    void logoutUser(ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    default Future<Void> logoutUser(ApiClient.AuthInfo authInfo){
        Promise<Void> promise = Promise.promise();
        logoutUser(authInfo, promise);
        return promise.future();
    }

    void updateUser(@javax.annotation.Nonnull String username, @javax.annotation.Nonnull User user, Handler<AsyncResult<Void>> handler);

    default Future<Void> updateUser(@javax.annotation.Nonnull String username, @javax.annotation.Nonnull User user){
        Promise<Void> promise = Promise.promise();
        updateUser(username, user, promise);
        return promise.future();
    }

    void updateUser(@javax.annotation.Nonnull String username, @javax.annotation.Nonnull User user, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> handler);

    default Future<Void> updateUser(@javax.annotation.Nonnull String username, @javax.annotation.Nonnull User user, ApiClient.AuthInfo authInfo){
        Promise<Void> promise = Promise.promise();
        updateUser(username, user, authInfo, promise);
        return promise.future();
    }

}
