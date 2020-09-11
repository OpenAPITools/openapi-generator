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
    void createUser(Http.Request request, User body) ;

    void createUsersWithArrayInput(Http.Request request, List<User> body) ;

    void createUsersWithListInput(Http.Request request, List<User> body) ;

    void deleteUser(Http.Request request, String username) ;

    User getUserByName(Http.Request request, String username) ;

    String loginUser(Http.Request request, @NotNull String username, @NotNull String password) ;

    void logoutUser(Http.Request request) ;

    void updateUser(Http.Request request, String username, User body) ;

}
