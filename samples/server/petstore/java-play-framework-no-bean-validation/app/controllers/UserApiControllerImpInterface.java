package controllers;

import java.util.List;
import apimodels.User;

import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;


public interface UserApiControllerImpInterface {
    void createUser(User body) throws Exception;

    void createUsersWithArrayInput(List<User> body) throws Exception;

    void createUsersWithListInput(List<User> body) throws Exception;

    void deleteUser(String username) throws Exception;

    User getUserByName(String username) throws Exception;

    String loginUser(String username, String password) throws Exception;

    void logoutUser() throws Exception;

    void updateUser(String username, User body) throws Exception;

}
