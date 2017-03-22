package controllers;

import java.util.List;
import apimodels.User;

import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;
import javax.validation.constraints.*;

public class UserApiControllerImp implements UserApiControllerImpInterface {
    public void createUser(User body) {
        //Do your magic!!!
        
    }

    public void createUsersWithArrayInput(List<User> body) {
        //Do your magic!!!
        
    }

    public void createUsersWithListInput(List<User> body) {
        //Do your magic!!!
        
    }

    public void deleteUser(String username) {
        //Do your magic!!!
        
    }

    public User getUserByName(String username) {
        //Do your magic!!!
        return new User();
    }

    public String loginUser( String username,  String password) {
        //Do your magic!!!
        return new String();
    }

    public void logoutUser() {
        //Do your magic!!!
        
    }

    public void updateUser(String username, User body) {
        //Do your magic!!!
        
    }

}
