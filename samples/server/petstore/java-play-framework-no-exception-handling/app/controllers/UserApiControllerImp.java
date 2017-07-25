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

    private final ObjectMapper mapper;

    @Inject
    private UserApiControllerImp() {
        mapper = new ObjectMapper();
    }

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
        String accept = request().getHeader("Accept");
        if (accept != null && accept.contains("application/xml")) {
            return mapper.readValue("", User.class);
        }
        String accept = request().getHeader("Accept");
        if (accept != null && accept.contains("application/json")) {
            return mapper.readValue("", User.class);
        }
        return new User();
    }

    @Override
    public String loginUser( @NotNull String username,  @NotNull String password)  {
        //Do your magic!!!
        String accept = request().getHeader("Accept");
        if (accept != null && accept.contains("application/xml")) {
            return mapper.readValue("", String.class);
        }
        String accept = request().getHeader("Accept");
        if (accept != null && accept.contains("application/json")) {
            return mapper.readValue("", String.class);
        }
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
