package org.openapitools.server.api.verticle;

import org.openapitools.server.api.MainApiException;
import org.openapitools.server.api.model.User;

public final class UserApiException extends MainApiException {
    public UserApiException(int statusCode, String statusMessage) {
        super(statusCode, statusMessage);
    }
    
    public static final UserApiException User_deleteUser_400_Exception = new UserApiException(400, "Invalid username supplied");
    public static final UserApiException User_deleteUser_404_Exception = new UserApiException(404, "User not found");
    public static final UserApiException User_getUserByName_200_Exception = new UserApiException(200, "successful operation");
    public static final UserApiException User_getUserByName_400_Exception = new UserApiException(400, "Invalid username supplied");
    public static final UserApiException User_getUserByName_404_Exception = new UserApiException(404, "User not found");
    public static final UserApiException User_loginUser_200_Exception = new UserApiException(200, "successful operation");
    public static final UserApiException User_loginUser_400_Exception = new UserApiException(400, "Invalid username/password supplied");
    public static final UserApiException User_updateUser_400_Exception = new UserApiException(400, "Invalid user supplied");
    public static final UserApiException User_updateUser_404_Exception = new UserApiException(404, "User not found");
    

}