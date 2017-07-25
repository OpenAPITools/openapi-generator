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
    public User getUserByName(String username) throws Exception {
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
    public String loginUser( @NotNull String username,  @NotNull String password) throws Exception {
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
    public void logoutUser() throws Exception {
        //Do your magic!!!
        
    }

    @Override
    public void updateUser(String username, User body) throws Exception {
        //Do your magic!!!
        
    }

}
