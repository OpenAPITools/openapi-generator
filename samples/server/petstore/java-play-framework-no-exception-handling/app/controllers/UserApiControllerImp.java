package controllers;

import java.util.List;
import apimodels.User;

import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.io.FileInputStream;
import javax.validation.constraints.*;
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaPlayFrameworkCodegen")
public class UserApiControllerImp implements UserApiControllerImpInterface {
    @Override
    public void createUser(Request request, User body)  {
        //Do your magic!!!
    }

    @Override
    public void createUsersWithArrayInput(Request request, List<User> body)  {
        //Do your magic!!!
    }

    @Override
    public void createUsersWithListInput(Request request, List<User> body)  {
        //Do your magic!!!
    }

    @Override
    public void deleteUser(Request request, String username)  {
        //Do your magic!!!
    }

    @Override
    public User getUserByName(Request request, String username)  {
        //Do your magic!!!
        return new User();
    }

    @Override
    public String loginUser(Request request, @NotNull String username, @NotNull String password)  {
        //Do your magic!!!
        return new String();
    }

    @Override
    public void logoutUser(Request request)  {
        //Do your magic!!!
    }

    @Override
    public void updateUser(Request request, String username, User body)  {
        //Do your magic!!!
    }

}
