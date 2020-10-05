package com.puppies.store.apis;

import java.util.List;
import apimodels.User;

import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;

import javax.validation.constraints.*;

@SuppressWarnings("RedundantThrows")
public interface UserApiControllerImpInterface {
    default Result createUserHttp(Http.Request request, User body) throws Exception {
        createUser(request, body);
        return ok();
    }

    void createUser(Http.Request request, User body) throws Exception;

    default Result createUsersWithArrayInputHttp(Http.Request request, List<User> body) throws Exception {
        createUsersWithArrayInput(request, body);
        return ok();
    }

    void createUsersWithArrayInput(Http.Request request, List<User> body) throws Exception;

    default Result createUsersWithListInputHttp(Http.Request request, List<User> body) throws Exception {
        createUsersWithListInput(request, body);
        return ok();
    }

    void createUsersWithListInput(Http.Request request, List<User> body) throws Exception;

    default Result deleteUserHttp(Http.Request request, String username) throws Exception {
        deleteUser(request, username);
        return ok();
    }

    void deleteUser(Http.Request request, String username) throws Exception;

    default Result getUserByNameHttp(Http.Request request, String username) throws Exception {
        User obj = getUserByName(request, username);
        if (configuration.getBoolean("useOutputBeanValidation")) {
                OpenAPIUtils.validate(obj);
        }
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
    }

    User getUserByName(Http.Request request, String username) throws Exception;

    default Result loginUserHttp(Http.Request request, @NotNull String username, @NotNull String password) throws Exception {
        String obj = loginUser(request, username, password);
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
    }

    String loginUser(Http.Request request, @NotNull String username, @NotNull String password) throws Exception;

    default Result logoutUserHttp(Http.Request request) throws Exception {
        logoutUser(request);
        return ok();
    }

    void logoutUser(Http.Request request) throws Exception;

    default Result updateUserHttp(Http.Request request, String username, User body) throws Exception {
        updateUser(request, username, body);
        return ok();
    }

    void updateUser(Http.Request request, String username, User body) throws Exception;

}
