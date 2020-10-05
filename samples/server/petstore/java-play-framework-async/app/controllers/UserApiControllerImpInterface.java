package controllers;

import java.util.List;
import apimodels.User;

import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.CompletableFuture;

import javax.validation.constraints.*;

@SuppressWarnings("RedundantThrows")
public interface UserApiControllerImpInterface {
    default CompletionStage<Result> createUserHttp(Http.Request request, User body) throws Exception {
        return CompletableFuture.supplyAsync(() -> {
            createUser(request, body);
            return ok();
        });
    }

    void createUser(Http.Request request, User body) throws Exception;

    default CompletionStage<Result> createUsersWithArrayInputHttp(Http.Request request, List<User> body) throws Exception {
        return CompletableFuture.supplyAsync(() -> {
            createUsersWithArrayInput(request, body);
            return ok();
        });
    }

    void createUsersWithArrayInput(Http.Request request, List<User> body) throws Exception;

    default CompletionStage<Result> createUsersWithListInputHttp(Http.Request request, List<User> body) throws Exception {
        return CompletableFuture.supplyAsync(() -> {
            createUsersWithListInput(request, body);
            return ok();
        });
    }

    void createUsersWithListInput(Http.Request request, List<User> body) throws Exception;

    default CompletionStage<Result> deleteUserHttp(Http.Request request, String username) throws Exception {
        return CompletableFuture.supplyAsync(() -> {
            deleteUser(request, username);
            return ok();
        });
    }

    void deleteUser(Http.Request request, String username) throws Exception;

    default CompletionStage<Result> getUserByNameHttp(Http.Request request, String username) throws Exception {
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

    CompletionStage<User> getUserByName(Http.Request request, String username) throws Exception;

    default CompletionStage<Result> loginUserHttp(Http.Request request, @NotNull String username, @NotNull String password) throws Exception {
        CompletionStage<String> stage = loginUser(request, username, password).thenApply(obj -> { 
            return obj;
        });
            stage.thenApply(obj -> {
            JsonNode result = mapper.valueToTree(obj);
            return ok(result);
        });
    }

    CompletionStage<String> loginUser(Http.Request request, @NotNull String username, @NotNull String password) throws Exception;

    default CompletionStage<Result> logoutUserHttp(Http.Request request) throws Exception {
        return CompletableFuture.supplyAsync(() -> {
            logoutUser(request);
            return ok();
        });
    }

    void logoutUser(Http.Request request) throws Exception;

    default CompletionStage<Result> updateUserHttp(Http.Request request, String username, User body) throws Exception {
        return CompletableFuture.supplyAsync(() -> {
            updateUser(request, username, body);
            return ok();
        });
    }

    void updateUser(Http.Request request, String username, User body) throws Exception;

}
