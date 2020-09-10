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
    void createUser(Request request, User body) ;

    void createUsersWithArrayInput(Request request, List<User> body) ;

    void createUsersWithListInput(Request request, List<User> body) ;

    void deleteUser(Request request, String username) ;

    User getUserByName(Request request, String username) ;

    String loginUser(Request request, @NotNull String username, @NotNull String password) ;

    void logoutUser(Request request) ;

    void updateUser(Request request, String username, User body) ;

}
