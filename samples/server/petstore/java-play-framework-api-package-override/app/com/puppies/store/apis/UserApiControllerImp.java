package com.puppies.store.apis;

import java.util.List;
import apimodels.User;

import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;
import java.io.FileInputStream;
import javax.validation.constraints.*;

public class UserApiControllerImp implements UserApiControllerImpInterface {
    @Override
    public void createUser(User user) throws Exception {
        //Do your magic!!!
    }

    @Override
    public void createUsersWithArrayInput(List<User> user) throws Exception {
        //Do your magic!!!
    }

    @Override
    public void createUsersWithListInput(List<User> user) throws Exception {
        //Do your magic!!!
    }

    @Override
    public void deleteUser(String username) throws Exception {
        //Do your magic!!!
    }

    @Override
    public User getUserByName(String username) throws Exception {
        //Do your magic!!!
        return new User();
    }

    @Override
    public String loginUser( @NotNull String username,  @NotNull String password) throws Exception {
        //Do your magic!!!
        return new String();
    }

    @Override
    public void logoutUser() throws Exception {
        //Do your magic!!!
    }

    @Override
    public void updateUser(String username, User user) throws Exception {
        //Do your magic!!!
    }

}
