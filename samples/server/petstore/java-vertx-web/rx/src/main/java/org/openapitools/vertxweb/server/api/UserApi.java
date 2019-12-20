package org.openapitools.vertxweb.server.api;

import org.openapitools.vertxweb.server.model.User;

import org.openapitools.vertxweb.server.ApiResponse;

import io.reactivex.Single;

import java.util.List;
import java.util.Map;

public interface UserApi  {
    Single<ApiResponse<Void>> createUser(User user);
    Single<ApiResponse<Void>> createUsersWithArrayInput(List<User> user);
    Single<ApiResponse<Void>> createUsersWithListInput(List<User> user);
    Single<ApiResponse<Void>> deleteUser(String username);
    Single<ApiResponse<User>> getUserByName(String username);
    Single<ApiResponse<String>> loginUser(String username,String password);
    Single<ApiResponse<Void>> logoutUser();
    Single<ApiResponse<Void>> updateUser(String username,User user);
}
