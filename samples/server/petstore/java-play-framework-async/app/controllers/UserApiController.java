package controllers;

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

import java.util.concurrent.CompletionStage;
import java.util.concurrent.CompletableFuture;

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
    public CompletionStage<Result> createUser() throws Exception {
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
        return CompletableFuture.supplyAsync(() -> {
            imp.createUser(user)
            return ok();
        });
    }

    @ApiAction
    public CompletionStage<Result> createUsersWithArrayInput() throws Exception {
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
        return CompletableFuture.supplyAsync(() -> {
            imp.createUsersWithArrayInput(user)
            return ok();
        });
    }

    @ApiAction
    public CompletionStage<Result> createUsersWithListInput() throws Exception {
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
        return CompletableFuture.supplyAsync(() -> {
            imp.createUsersWithListInput(user)
            return ok();
        });
    }

    @ApiAction
    public CompletionStage<Result> deleteUser(String username) throws Exception {
        return CompletableFuture.supplyAsync(() -> {
            imp.deleteUser(username)
            return ok();
        });
    }

    @ApiAction
    public CompletionStage<Result> getUserByName(String username) throws Exception {
        CompletionStage<User> stage = imp.getUserByName(username).thenApply(obj -> { 
            if (configuration.getBoolean("useOutputBeanValidation")) {
                OpenAPIUtils.validate(obj);
            }
            return obj;
        });
        stage.thenApply(obj -> {
            JsonNode result = mapper.valueToTree(obj);
            return ok(result);
        });
    }

    @ApiAction
    public CompletionStage<Result> loginUser() throws Exception {
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
        CompletionStage<String> stage = imp.loginUser(username, password).thenApply(obj -> { 
            return obj;
        });
        stage.thenApply(obj -> {
            JsonNode result = mapper.valueToTree(obj);
            return ok(result);
        });
    }

    @ApiAction
    public CompletionStage<Result> logoutUser() throws Exception {
        return CompletableFuture.supplyAsync(() -> {
            imp.logoutUser()
            return ok();
        });
    }

    @ApiAction
    public CompletionStage<Result> updateUser(String username) throws Exception {
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
        return CompletableFuture.supplyAsync(() -> {
            imp.updateUser(username, user)
            return ok();
        });
    }
}
