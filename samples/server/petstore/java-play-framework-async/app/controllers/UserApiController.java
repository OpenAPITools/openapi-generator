package controllers;

import java.util.List;
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
import openapitools.OpenAPIUtils;
import com.fasterxml.jackson.core.type.TypeReference;

import java.util.concurrent.CompletionStage;
import java.util.concurrent.CompletableFuture;

import javax.validation.constraints.*;
import com.typesafe.config.Config;

import openapitools.OpenAPIUtils.ApiAction;

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


    @ApiAction
    public CompletionStage<Result> createUser(Http.Request request) throws Exception {
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
        return CompletableFuture.supplyAsync(() -> {
            imp.createUser(request, body);
            return ok();
        });
    }

    @ApiAction
    public CompletionStage<Result> createUsersWithArrayInput(Http.Request request) throws Exception {
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
        return CompletableFuture.supplyAsync(() -> {
            imp.createUsersWithArrayInput(request, body);
            return ok();
        });
    }

    @ApiAction
    public CompletionStage<Result> createUsersWithListInput(Http.Request request) throws Exception {
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
        return CompletableFuture.supplyAsync(() -> {
            imp.createUsersWithListInput(request, body);
            return ok();
        });
    }

    @ApiAction
    public CompletionStage<Result> deleteUser(Http.Request request, String username) throws Exception {
        return CompletableFuture.supplyAsync(() -> {
            imp.deleteUser(request, username);
            return ok();
        });
    }

    @ApiAction
    public CompletionStage<Result> getUserByName(Http.Request request, String username) throws Exception {
        CompletionStage<User> stage = imp.getUserByName(request, username).thenApply(obj -> { 
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
    public CompletionStage<Result> loginUser(Http.Request request) throws Exception {
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
        CompletionStage<String> stage = imp.loginUser(request, username, password).thenApply(obj -> { 
            return obj;
        });
        stage.thenApply(obj -> {
            JsonNode result = mapper.valueToTree(obj);
            return ok(result);
        });
    }

    @ApiAction
    public CompletionStage<Result> logoutUser(Http.Request request) throws Exception {
        return CompletableFuture.supplyAsync(() -> {
            imp.logoutUser(request);
            return ok();
        });
    }

    @ApiAction
    public CompletionStage<Result> updateUser(Http.Request request, String username) throws Exception {
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
        return CompletableFuture.supplyAsync(() -> {
            imp.updateUser(request, username, body);
            return ok();
        });
    }
}
