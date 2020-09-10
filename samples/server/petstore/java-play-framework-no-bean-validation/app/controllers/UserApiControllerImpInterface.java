package controllers;

import java.util.List;
import apimodels.User;

import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;


@SuppressWarnings("RedundantThrows")
public interface UserApiControllerImpInterface {
    void createUser(Request request, User body) throws Exception;

    void createUsersWithArrayInput(Request request, List<User> body) throws Exception;

    void createUsersWithListInput(Request request, List<User> body) throws Exception;

    void deleteUser(Request request, String username) throws Exception;

    User getUserByName(Request request, String username) throws Exception;

    String loginUser(Request request, String username, String password) throws Exception;

    void logoutUser(Request request) throws Exception;

    void updateUser(Request request, String username, User body) throws Exception;

}
