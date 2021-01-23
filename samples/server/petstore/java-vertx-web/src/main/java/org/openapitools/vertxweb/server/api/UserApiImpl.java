package org.openapitools.vertxweb.server.api;

import org.openapitools.vertxweb.server.model.User;

import org.openapitools.vertxweb.server.ApiResponse;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.handler.impl.HttpStatusException;

import java.util.List;
import java.util.Map;

// Implement this class

public class UserApiImpl implements UserApi {
    public Future<ApiResponse<Void>> createUser(User user) {
        return Future.failedFuture(new HttpStatusException(501));
    }

    public Future<ApiResponse<Void>> createUsersWithArrayInput(List<User> user) {
        return Future.failedFuture(new HttpStatusException(501));
    }

    public Future<ApiResponse<Void>> createUsersWithListInput(List<User> user) {
        return Future.failedFuture(new HttpStatusException(501));
    }

    public Future<ApiResponse<Void>> deleteUser(String username) {
        return Future.failedFuture(new HttpStatusException(501));
    }

    public Future<ApiResponse<User>> getUserByName(String username) {
        return Future.failedFuture(new HttpStatusException(501));
    }

    public Future<ApiResponse<String>> loginUser(String username, String password) {
        return Future.failedFuture(new HttpStatusException(501));
    }

    public Future<ApiResponse<Void>> logoutUser() {
        return Future.failedFuture(new HttpStatusException(501));
    }

    public Future<ApiResponse<Void>> updateUser(String username, User user) {
        return Future.failedFuture(new HttpStatusException(501));
    }

}
