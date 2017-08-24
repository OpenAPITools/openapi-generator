package controllers;

import java.util.List;
import apimodels.User;

import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;
import java.io.FileInputStream;
import javax.validation.constraints.*;

public class UserApiControllerImp  {
    
    public void createUser(User body) throws Exception {
        //Do your magic!!!
        
    }

    
    public void createUsersWithArrayInput(List<User> body) throws Exception {
        //Do your magic!!!
        
    }

    
    public void createUsersWithListInput(List<User> body) throws Exception {
        //Do your magic!!!
        
    }

    
    public void deleteUser(String username) throws Exception {
        //Do your magic!!!
        
    }

    
    public User getUserByName(String username) throws Exception {
        //Do your magic!!!
        return new User();
    }

    
    public String loginUser( @NotNull String username,  @NotNull String password) throws Exception {
        //Do your magic!!!
        return new String();
    }

    
    public void logoutUser() throws Exception {
        //Do your magic!!!
        
    }

    
    public void updateUser(String username, User body) throws Exception {
        //Do your magic!!!
        
    }

}
