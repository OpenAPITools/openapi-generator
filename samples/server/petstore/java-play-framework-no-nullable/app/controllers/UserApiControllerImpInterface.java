package controllers;

import java.util.List;
import apimodels.User;

import com.google.inject.Inject;
import com.google.inject.Singleton;
import com.typesafe.config.Config;
import play.mvc.Controller;
import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;
import play.mvc.Result;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.JsonNode;
import openapitools.OpenAPIUtils;
import static play.mvc.Results.ok;

import javax.validation.constraints.*;

@Singleton
@SuppressWarnings("RedundantThrows")
public abstract class UserApiControllerImpInterface {
    @Inject private Config configuration;
    private ObjectMapper mapper = new ObjectMapper();

    Result createUserHttp(Http.Request request, User body) throws Exception {
        createUser(request, body);
return ok();

    }

    abstract void createUser(Http.Request request, User body) throws Exception;

    Result createUsersWithArrayInputHttp(Http.Request request, List<User> body) throws Exception {
        createUsersWithArrayInput(request, body);
return ok();

    }

    abstract void createUsersWithArrayInput(Http.Request request, List<User> body) throws Exception;

    Result createUsersWithListInputHttp(Http.Request request, List<User> body) throws Exception {
        createUsersWithListInput(request, body);
return ok();

    }

    abstract void createUsersWithListInput(Http.Request request, List<User> body) throws Exception;

    Result deleteUserHttp(Http.Request request, String username) throws Exception {
        deleteUser(request, username);
return ok();

    }

    abstract void deleteUser(Http.Request request, String username) throws Exception;

    Result getUserByNameHttp(Http.Request request, String username) throws Exception {
        User obj = getUserByName(request, username);
    if (configuration.getBoolean("useOutputBeanValidation")) {
            OpenAPIUtils.validate(obj);
    }
JsonNode result = mapper.valueToTree(obj);
return ok(result);

    }

    abstract User getUserByName(Http.Request request, String username) throws Exception;

    Result loginUserHttp(Http.Request request, @NotNull String username, @NotNull String password) throws Exception {
        String obj = loginUser(request, username, password);
JsonNode result = mapper.valueToTree(obj);
return ok(result);

    }

    abstract String loginUser(Http.Request request, @NotNull String username, @NotNull String password) throws Exception;

    Result logoutUserHttp(Http.Request request) throws Exception {
        logoutUser(request);
return ok();

    }

    abstract void logoutUser(Http.Request request) throws Exception;

    Result updateUserHttp(Http.Request request, String username, User body) throws Exception {
        updateUser(request, username, body);
return ok();

    }

    abstract void updateUser(Http.Request request, String username, User body) throws Exception;

}
