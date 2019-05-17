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
    public void createUser(User body)  {
        //Do your magic!!!
    }

    @Override
    public void createUsersWithArrayInput(List<User> body)  {
        //Do your magic!!!
    }

    @Override
    public void createUsersWithListInput(List<User> body)  {
        //Do your magic!!!
    }

    @Override
    public void deleteUser(String username)  {
        //Do your magic!!!
    }

    @Override
    public User getUserByName(String username)  {
        //Do your magic!!!
        return new User();
    }

    @Override
    public String loginUser( @NotNull String username,  @NotNull String password)  {
        //Do your magic!!!
        return new String();
    }

    @Override
    public void logoutUser()  {
        //Do your magic!!!
    }

    @Override
    public void updateUser(String username, User body)  {
        //Do your magic!!!
    }

}
