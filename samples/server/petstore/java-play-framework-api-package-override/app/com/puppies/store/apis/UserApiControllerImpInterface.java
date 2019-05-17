package com.puppies.store.apis;

import java.util.List;
import apimodels.User;

import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;

import javax.validation.constraints.*;

@SuppressWarnings("RedundantThrows")
public interface UserApiControllerImpInterface {
    void createUser(User user) throws Exception;

    void createUsersWithArrayInput(List<User> user) throws Exception;

    void createUsersWithListInput(List<User> user) throws Exception;

    void deleteUser(String username) throws Exception;

    User getUserByName(String username) throws Exception;

    String loginUser( @NotNull String username,  @NotNull String password) throws Exception;

    void logoutUser() throws Exception;

    void updateUser(String username, User user) throws Exception;

}
