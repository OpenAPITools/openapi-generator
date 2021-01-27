package com.puppies.store.apis;

import java.util.List;
import apimodels.User;

import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.io.FileInputStream;
import play.libs.Files.TemporaryFile;
import javax.validation.constraints.*;
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaPlayFrameworkCodegen")
public class UserApiControllerImp extends UserApiControllerImpInterface {
    @Override
    public void createUser(Http.Request request, User body) throws Exception {
        //Do your magic!!!
    }

    @Override
    public void createUsersWithArrayInput(Http.Request request, List<User> body) throws Exception {
        //Do your magic!!!
    }

    @Override
    public void createUsersWithListInput(Http.Request request, List<User> body) throws Exception {
        //Do your magic!!!
    }

    @Override
    public void deleteUser(Http.Request request, String username) throws Exception {
        //Do your magic!!!
    }

    @Override
    public User getUserByName(Http.Request request, String username) throws Exception {
        //Do your magic!!!
        return new User();
    }

    @Override
    public String loginUser(Http.Request request, @NotNull String username, @NotNull String password) throws Exception {
        //Do your magic!!!
        return new String();
    }

    @Override
    public void logoutUser(Http.Request request) throws Exception {
        //Do your magic!!!
    }

    @Override
    public void updateUser(Http.Request request, String username, User body) throws Exception {
        //Do your magic!!!
    }

}
