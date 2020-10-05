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
import java.util.concurrent.CompletionStage;
import java.util.concurrent.CompletableFuture;

import javax.validation.constraints.*;

@Singleton
@SuppressWarnings("RedundantThrows")
public abstract class UserApiControllerImpInterface {
    @Inject private Config configuration;
    private ObjectMapper mapper = new ObjectMapper();

    CompletionStage<Result> createUserHttp(Http.Request request, User body) throws Exception {
        return CompletableFuture.supplyAsync(() -> {
    createUser(request, body);
    return ok();
});

    }

    abstract void createUser(Http.Request request, User body) throws Exception;

    CompletionStage<Result> createUsersWithArrayInputHttp(Http.Request request, List<User> body) throws Exception {
        return CompletableFuture.supplyAsync(() -> {
    createUsersWithArrayInput(request, body);
    return ok();
});

    }

    abstract void createUsersWithArrayInput(Http.Request request, List<User> body) throws Exception;

    CompletionStage<Result> createUsersWithListInputHttp(Http.Request request, List<User> body) throws Exception {
        return CompletableFuture.supplyAsync(() -> {
    createUsersWithListInput(request, body);
    return ok();
});

    }

    abstract void createUsersWithListInput(Http.Request request, List<User> body) throws Exception;

    CompletionStage<Result> deleteUserHttp(Http.Request request, String username) throws Exception {
        return CompletableFuture.supplyAsync(() -> {
    deleteUser(request, username);
    return ok();
});

    }

    abstract void deleteUser(Http.Request request, String username) throws Exception;

    CompletionStage<Result> getUserByNameHttp(Http.Request request, String username) throws Exception {
        CompletionStage<User> stage = getUserByName(request, username).thenApply(obj -> { 
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

    abstract CompletionStage<User> getUserByName(Http.Request request, String username) throws Exception;

    CompletionStage<Result> loginUserHttp(Http.Request request, @NotNull String username, @NotNull String password) throws Exception {
        CompletionStage<String> stage = loginUser(request, username, password).thenApply(obj -> { 
    return obj;
});
    stage.thenApply(obj -> {
    JsonNode result = mapper.valueToTree(obj);
    return ok(result);
});

    }

    abstract CompletionStage<String> loginUser(Http.Request request, @NotNull String username, @NotNull String password) throws Exception;

    CompletionStage<Result> logoutUserHttp(Http.Request request) throws Exception {
        return CompletableFuture.supplyAsync(() -> {
    logoutUser(request);
    return ok();
});

    }

    abstract void logoutUser(Http.Request request) throws Exception;

    CompletionStage<Result> updateUserHttp(Http.Request request, String username, User body) throws Exception {
        return CompletableFuture.supplyAsync(() -> {
    updateUser(request, username, body);
    return ok();
});

    }

    abstract void updateUser(Http.Request request, String username, User body) throws Exception;

}
