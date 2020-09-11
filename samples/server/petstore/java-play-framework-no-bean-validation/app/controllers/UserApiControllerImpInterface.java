package controllers;

import java.util.List;
import apimodels.User;

import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;


@SuppressWarnings("RedundantThrows")
public interface UserApiControllerImpInterface {
    void createUser(Http.Request request, User body) throws Exception;

    void createUsersWithArrayInput(Http.Request request, List<User> body) throws Exception;

    void createUsersWithListInput(Http.Request request, List<User> body) throws Exception;

    void deleteUser(Http.Request request, String username) throws Exception;

    User getUserByName(Http.Request request, String username) throws Exception;

    String loginUser(Http.Request request, String username, String password) throws Exception;

    void logoutUser(Http.Request request) throws Exception;

    void updateUser(Http.Request request, String username, User body) throws Exception;

}
