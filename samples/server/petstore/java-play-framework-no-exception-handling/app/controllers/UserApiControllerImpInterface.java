package controllers;

import java.util.List;
import apimodels.User;

import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;

import javax.validation.constraints.*;

@SuppressWarnings("RedundantThrows")
public interface UserApiControllerImpInterface {
    void createUser(User user) ;

    void createUsersWithArrayInput(List<User> user) ;

    void createUsersWithListInput(List<User> user) ;

    void deleteUser(String username) ;

    User getUserByName(String username) ;

    String loginUser( @NotNull String username,  @NotNull String password) ;

    void logoutUser() ;

    void updateUser(String username, User user) ;

}
