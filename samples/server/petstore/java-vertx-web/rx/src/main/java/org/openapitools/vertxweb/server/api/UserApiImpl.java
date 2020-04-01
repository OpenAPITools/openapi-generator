package org.openapitools.vertxweb.server.api;

import org.openapitools.vertxweb.server.model.User;

import org.openapitools.vertxweb.server.ApiResponse;
import org.openapitools.vertxweb.server.ApiException;

import io.reactivex.Single;

import java.util.List;
import java.util.Map;

// Implement this class

public class UserApiImpl implements UserApi {
    public Single<ApiResponse<Void>> createUser(User user) {
        return Single.error(new ApiException("Not Implemented").setStatusCode(501));
    }

    public Single<ApiResponse<Void>> createUsersWithArrayInput(List<User> user) {
        return Single.error(new ApiException("Not Implemented").setStatusCode(501));
    }

    public Single<ApiResponse<Void>> createUsersWithListInput(List<User> user) {
        return Single.error(new ApiException("Not Implemented").setStatusCode(501));
    }

    public Single<ApiResponse<Void>> deleteUser(String username) {
        return Single.error(new ApiException("Not Implemented").setStatusCode(501));
    }

    public Single<ApiResponse<User>> getUserByName(String username) {
        return Single.error(new ApiException("Not Implemented").setStatusCode(501));
    }

    public Single<ApiResponse<String>> loginUser(String username,String password) {
        return Single.error(new ApiException("Not Implemented").setStatusCode(501));
    }

    public Single<ApiResponse<Void>> logoutUser() {
        return Single.error(new ApiException("Not Implemented").setStatusCode(501));
    }

    public Single<ApiResponse<Void>> updateUser(String username,User user) {
        return Single.error(new ApiException("Not Implemented").setStatusCode(501));
    }

}
