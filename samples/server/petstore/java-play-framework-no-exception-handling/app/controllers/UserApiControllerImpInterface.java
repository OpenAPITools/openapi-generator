package controllers;

import java.util.List;
import java.time.OffsetDateTime;
import apimodels.User;

import com.google.inject.Inject;
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
import openapitools.SecurityAPIUtils;
import static play.mvc.Results.ok;
import static play.mvc.Results.unauthorized;
import play.libs.Files.TemporaryFile;

import javax.validation.constraints.*;

@SuppressWarnings("RedundantThrows")
public abstract class UserApiControllerImpInterface {
    @Inject private Config configuration;
    @Inject private SecurityAPIUtils securityAPIUtils;
    private ObjectMapper mapper = new ObjectMapper();

    public Result createUserHttp(Http.Request request, User body)  {
        createUser(request, body);
        return ok();

    }

    public abstract void createUser(Http.Request request, User body) ;

    public Result createUsersWithArrayInputHttp(Http.Request request, List<User> body)  {
        createUsersWithArrayInput(request, body);
        return ok();

    }

    public abstract void createUsersWithArrayInput(Http.Request request, List<User> body) ;

    public Result createUsersWithListInputHttp(Http.Request request, List<User> body)  {
        createUsersWithListInput(request, body);
        return ok();

    }

    public abstract void createUsersWithListInput(Http.Request request, List<User> body) ;

    public Result deleteUserHttp(Http.Request request, String username)  {
        deleteUser(request, username);
        return ok();

    }

    public abstract void deleteUser(Http.Request request, String username) ;

    public Result getUserByNameHttp(Http.Request request, String username)  {
        User obj = getUserByName(request, username);

        if (configuration.getBoolean("useOutputBeanValidation")) {
            OpenAPIUtils.validate(obj);
        }

        JsonNode result = mapper.valueToTree(obj);

        return ok(result);

    }

    public abstract User getUserByName(Http.Request request, String username) ;

    public Result loginUserHttp(Http.Request request, @NotNull String username, @NotNull String password)  {
        String obj = loginUser(request, username, password);
        JsonNode result = mapper.valueToTree(obj);

        return ok(result);

    }

    public abstract String loginUser(Http.Request request, @NotNull String username, @NotNull String password) ;

    public Result logoutUserHttp(Http.Request request)  {
        logoutUser(request);
        return ok();

    }

    public abstract void logoutUser(Http.Request request) ;

    public Result updateUserHttp(Http.Request request, String username, User body)  {
        updateUser(request, username, body);
        return ok();

    }

    public abstract void updateUser(Http.Request request, String username, User body) ;

}
