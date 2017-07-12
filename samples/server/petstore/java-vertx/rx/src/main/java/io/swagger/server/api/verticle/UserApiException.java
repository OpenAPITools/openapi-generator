package io.swagger.server.api.verticle;

import io.swagger.server.api.MainApiException;
import io.swagger.server.api.model.User;

public final class UserApiException extends MainApiException {
    public UserApiException(int statusCode, String statusMessage) {
        super(statusCode, statusMessage);
    }
    
    public static final UserApiException User_deleteUser_400_Exception = new UserApiException(400, "Invalid username supplied");
    public static final UserApiException User_deleteUser_404_Exception = new UserApiException(404, "User not found");
    public static final UserApiException User_getUserByName_400_Exception = new UserApiException(400, "Invalid username supplied");
    public static final UserApiException User_getUserByName_404_Exception = new UserApiException(404, "User not found");
    public static final UserApiException User_loginUser_400_Exception = new UserApiException(400, "Invalid username/password supplied");
    public static final UserApiException User_updateUser_400_Exception = new UserApiException(400, "Invalid user supplied");
    public static final UserApiException User_updateUser_404_Exception = new UserApiException(404, "User not found");
    

}