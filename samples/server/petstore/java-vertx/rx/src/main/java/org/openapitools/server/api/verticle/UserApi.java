package org.openapitools.server.api.verticle;

import org.openapitools.server.api.MainApiException;
import org.openapitools.server.api.model.User;

import rx.Completable;
import rx.Single;

import java.util.List;
import java.util.Map;

public interface UserApi  {
    //createUser
    public Completable createUser(User user);
    
    //createUsersWithArrayInput
    public Completable createUsersWithArrayInput(List<User> user);
    
    //createUsersWithListInput
    public Completable createUsersWithListInput(List<User> user);
    
    //deleteUser
    public Completable deleteUser(String username);
    
    //getUserByName
    public Single<User> getUserByName(String username);
    
    //loginUser
    public Single<String> loginUser(String username,String password);
    
    //logoutUser
    public Completable logoutUser();
    
    //updateUser
    public Completable updateUser(String username,User user);
    
}
