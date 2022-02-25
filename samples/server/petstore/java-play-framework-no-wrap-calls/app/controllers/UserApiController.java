package controllers;

import java.util.List;
import java.time.OffsetDateTime;
import apimodels.User;

import com.typesafe.config.Config;
import play.mvc.Controller;
import play.mvc.Result;
import play.mvc.Http;
import java.util.List;
import java.util.Map;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.JsonNode;
import com.google.inject.Inject;
import java.io.File;
import play.libs.Files.TemporaryFile;
import openapitools.OpenAPIUtils;
import com.fasterxml.jackson.core.type.TypeReference;

import javax.validation.constraints.*;
import com.typesafe.config.Config;


@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaPlayFrameworkCodegen")
public class UserApiController extends Controller {
    private final UserApiControllerImpInterface imp;
    private final ObjectMapper mapper;
    private final Config configuration;

    @Inject
    private UserApiController(Config configuration, UserApiControllerImpInterface imp) {
        this.imp = imp;
        mapper = new ObjectMapper();
        this.configuration = configuration;
    }

    
    public Result createUser(Http.Request request) throws Exception {
        JsonNode nodebody = request.body().asJson();
        User body;
        if (nodebody != null) {
            body = mapper.readValue(nodebody.toString(), User.class);
            if (configuration.getBoolean("useInputBeanValidation")) {
                OpenAPIUtils.validate(body);
            }
        } else {
            throw new IllegalArgumentException("'body' parameter is required");
        }
        return imp.createUserHttp(request, body);
    }

    
    public Result createUsersWithArrayInput(Http.Request request) throws Exception {
        JsonNode nodebody = request.body().asJson();
        List<User> body;
        if (nodebody != null) {
            body = mapper.readValue(nodebody.toString(), new TypeReference<List<User>>(){});
            if (configuration.getBoolean("useInputBeanValidation")) {
                for (User curItem : body) {
                    OpenAPIUtils.validate(curItem);
                }
            }
        } else {
            throw new IllegalArgumentException("'body' parameter is required");
        }
        return imp.createUsersWithArrayInputHttp(request, body);
    }

    
    public Result createUsersWithListInput(Http.Request request) throws Exception {
        JsonNode nodebody = request.body().asJson();
        List<User> body;
        if (nodebody != null) {
            body = mapper.readValue(nodebody.toString(), new TypeReference<List<User>>(){});
            if (configuration.getBoolean("useInputBeanValidation")) {
                for (User curItem : body) {
                    OpenAPIUtils.validate(curItem);
                }
            }
        } else {
            throw new IllegalArgumentException("'body' parameter is required");
        }
        return imp.createUsersWithListInputHttp(request, body);
    }

    
    public Result deleteUser(Http.Request request, String username) throws Exception {
        return imp.deleteUserHttp(request, username);
    }

    
    public Result getUserByName(Http.Request request, String username) throws Exception {
        return imp.getUserByNameHttp(request, username);
    }

    
    public Result loginUser(Http.Request request) throws Exception {
        String valueusername = request.getQueryString("username");
        String username;
        if (valueusername != null) {
            username = valueusername;
        } else {
            throw new IllegalArgumentException("'username' parameter is required");
        }
        String valuepassword = request.getQueryString("password");
        String password;
        if (valuepassword != null) {
            password = valuepassword;
        } else {
            throw new IllegalArgumentException("'password' parameter is required");
        }
        return imp.loginUserHttp(request, username, password);
    }

    
    public Result logoutUser(Http.Request request) throws Exception {
        return imp.logoutUserHttp(request);
    }

    
    public Result updateUser(Http.Request request, String username) throws Exception {
        JsonNode nodebody = request.body().asJson();
        User body;
        if (nodebody != null) {
            body = mapper.readValue(nodebody.toString(), User.class);
            if (configuration.getBoolean("useInputBeanValidation")) {
                OpenAPIUtils.validate(body);
            }
        } else {
            throw new IllegalArgumentException("'body' parameter is required");
        }
        return imp.updateUserHttp(request, username, body);
    }

}
