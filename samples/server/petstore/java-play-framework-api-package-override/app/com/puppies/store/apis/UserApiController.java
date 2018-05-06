package com.puppies.store.apis;

import java.util.List;
import apimodels.User;

import play.mvc.Controller;
import play.mvc.Result;
import play.mvc.Http;
import java.util.List;
import java.util.Map;
import java.util.ArrayList;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.JsonNode;
import com.google.inject.Inject;
import java.io.File;
import openapitools.OpenAPIUtils;
import com.fasterxml.jackson.core.type.TypeReference;

import javax.validation.constraints.*;
import play.Configuration;

import openapitools.OpenAPIUtils.ApiAction;


public class UserApiController extends Controller {

    private final UserApiControllerImpInterface imp;
    private final ObjectMapper mapper;
    private final Configuration configuration;

    @Inject
    private UserApiController(Configuration configuration, UserApiControllerImpInterface imp) {
        this.imp = imp;
        mapper = new ObjectMapper();
        this.configuration = configuration;
    }


    @ApiAction
    public Result createUser() throws Exception {
        JsonNode nodeuser = request().body().asJson();
        User user;
        if (nodeuser != null) {
            user = mapper.readValue(nodeuser.toString(), User.class);
            if (configuration.getBoolean("useInputBeanValidation")) {
                OpenAPIUtils.validate(user);
            }
        } else {
            throw new IllegalArgumentException("'User' parameter is required");
        }
        imp.createUser(user);
        return ok();
    }

    @ApiAction
    public Result createUsersWithArrayInput() throws Exception {
        JsonNode nodeuser = request().body().asJson();
        List<User> user;
        if (nodeuser != null) {
            user = mapper.readValue(nodeuser.toString(), new TypeReference<List<User>>(){});
            if (configuration.getBoolean("useInputBeanValidation")) {
                for (User curItem : user) {
                    OpenAPIUtils.validate(curItem);
                }
            }
        } else {
            throw new IllegalArgumentException("'User' parameter is required");
        }
        imp.createUsersWithArrayInput(user);
        return ok();
    }

    @ApiAction
    public Result createUsersWithListInput() throws Exception {
        JsonNode nodeuser = request().body().asJson();
        List<User> user;
        if (nodeuser != null) {
            user = mapper.readValue(nodeuser.toString(), new TypeReference<List<User>>(){});
            if (configuration.getBoolean("useInputBeanValidation")) {
                for (User curItem : user) {
                    OpenAPIUtils.validate(curItem);
                }
            }
        } else {
            throw new IllegalArgumentException("'User' parameter is required");
        }
        imp.createUsersWithListInput(user);
        return ok();
    }

    @ApiAction
    public Result deleteUser(String username) throws Exception {
        imp.deleteUser(username);
        return ok();
    }

    @ApiAction
    public Result getUserByName(String username) throws Exception {
        User obj = imp.getUserByName(username);
        if (configuration.getBoolean("useOutputBeanValidation")) {
            OpenAPIUtils.validate(obj);
        }
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
    }

    @ApiAction
    public Result loginUser() throws Exception {
        String valueusername = request().getQueryString("username");
        String username;
        if (valueusername != null) {
            username = valueusername;
        } else {
            throw new IllegalArgumentException("'username' parameter is required");
        }
        String valuepassword = request().getQueryString("password");
        String password;
        if (valuepassword != null) {
            password = valuepassword;
        } else {
            throw new IllegalArgumentException("'password' parameter is required");
        }
        String obj = imp.loginUser(username, password);
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
    }

    @ApiAction
    public Result logoutUser() throws Exception {
        imp.logoutUser();
        return ok();
    }

    @ApiAction
    public Result updateUser(String username) throws Exception {
        JsonNode nodeuser = request().body().asJson();
        User user;
        if (nodeuser != null) {
            user = mapper.readValue(nodeuser.toString(), User.class);
            if (configuration.getBoolean("useInputBeanValidation")) {
                OpenAPIUtils.validate(user);
            }
        } else {
            throw new IllegalArgumentException("'User' parameter is required");
        }
        imp.updateUser(username, user);
        return ok();
    }
}
