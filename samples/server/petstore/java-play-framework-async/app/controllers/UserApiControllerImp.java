package controllers;

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
    public void createUser(User body) throws Exception {
        //Do your magic!!!
    }

    @Override
    public void createUsersWithArrayInput(List<User> body) throws Exception {
        //Do your magic!!!
    }

    @Override
    public void createUsersWithListInput(List<User> body) throws Exception {
        //Do your magic!!!
    }

    @Override
    public void deleteUser(String username) throws Exception {
        //Do your magic!!!
    }

    @Override
    public CompletionStage<User> getUserByName(String username) throws Exception {
        //Do your magic!!!
        return CompletableFuture.supplyAsync(() -> {
           return new User();
        });
    }

    @Override
    public CompletionStage<String> loginUser( @NotNull String username,  @NotNull String password) throws Exception {
        //Do your magic!!!
        return CompletableFuture.supplyAsync(() -> {
           return new String();
        });
    }

    @Override
    public void logoutUser() throws Exception {
        //Do your magic!!!
    }

    @Override
    public void updateUser(String username, User body) throws Exception {
        //Do your magic!!!
    }

}
