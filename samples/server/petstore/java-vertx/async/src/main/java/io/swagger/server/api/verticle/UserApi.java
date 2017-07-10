package io.swagger.server.api.verticle;

import io.swagger.server.api.MainApiException;
import io.swagger.server.api.model.User;

import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;

import java.util.List;
import java.util.Map;

public interface UserApi  {
    //createUser
    void createUser(User body, Handler<AsyncResult<Void>> handler);
    
    //createUsersWithArrayInput
    void createUsersWithArrayInput(List<User> body, Handler<AsyncResult<Void>> handler);
    
    //createUsersWithListInput
    void createUsersWithListInput(List<User> body, Handler<AsyncResult<Void>> handler);
    
    //deleteUser
    void deleteUser(String username, Handler<AsyncResult<Void>> handler);
    
    //getUserByName
    void getUserByName(String username, Handler<AsyncResult<User>> handler);
    
    //loginUser
    void loginUser(String username, String password, Handler<AsyncResult<String>> handler);
    
    //logoutUser
    void logoutUser(Handler<AsyncResult<Void>> handler);
    
    //updateUser
    void updateUser(String username, User body, Handler<AsyncResult<Void>> handler);
    
}
