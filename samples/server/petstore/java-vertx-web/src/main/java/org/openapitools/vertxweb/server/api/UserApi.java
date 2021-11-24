package org.openapitools.vertxweb.server.api;

import org.openapitools.vertxweb.server.model.User;

import org.openapitools.vertxweb.server.ApiResponse;

import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

import java.util.List;
import java.util.Map;

public interface UserApi  {
    Future<ApiResponse<Void>> createUser(User user);
    Future<ApiResponse<Void>> createUsersWithArrayInput(List<User> user);
    Future<ApiResponse<Void>> createUsersWithListInput(List<User> user);
    Future<ApiResponse<Void>> deleteUser(String username);
    Future<ApiResponse<User>> getUserByName(String username);
    Future<ApiResponse<String>> loginUser(String username, String password);
    Future<ApiResponse<Void>> logoutUser();
    Future<ApiResponse<Void>> updateUser(String username, User user);
}
