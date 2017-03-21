package controllers;

import java.util.List;
import apimodels.User;

import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;

public interface UserApiControllerImpInterface {
    void createUser(User body);

    void createUsersWithArrayInput(List<User> body);

    void createUsersWithListInput(List<User> body);

    void deleteUser( String username);

    User getUserByName( String username);

    String loginUser( String username,  String password);

    void logoutUser();

    void updateUser( String username, User body);

}
