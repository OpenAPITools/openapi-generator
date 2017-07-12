package io.swagger.server.api.verticle;

import io.swagger.server.api.MainApiException;
import io.swagger.server.api.model.User;

import rx.Completable;
import rx.Single;

import java.util.List;
import java.util.Map;

public interface UserApi  {
    //createUser
    public Completable createUser(User body);
    
    //createUsersWithArrayInput
    public Completable createUsersWithArrayInput(List<User> body);
    
    //createUsersWithListInput
    public Completable createUsersWithListInput(List<User> body);
    
    //deleteUser
    public Completable deleteUser(String username);
    
    //getUserByName
    public Single<User> getUserByName(String username);
    
    //loginUser
    public Single<String> loginUser(String username,String password);
    
    //logoutUser
    public Completable logoutUser();
    
    //updateUser
    public Completable updateUser(String username,User body);
    
}
