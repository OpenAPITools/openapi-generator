package org.openapitools.vertxweb.server.api;

import org.openapitools.vertxweb.server.model.User;

import org.openapitools.vertxweb.server.ParameterCast;
import org.openapitools.vertxweb.server.ApiException;

import com.fasterxml.jackson.core.type.TypeReference;
import io.vertx.core.json.Json;
import io.vertx.core.Handler;
import io.vertx.core.http.HttpServerResponse;
import io.vertx.ext.web.RoutingContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.reactivex.Single;

import java.util.List;
import java.util.Map;

public class UserApiHandler {

    private static final Logger logger = LoggerFactory.getLogger(UserApiHandler.class);
    private UserApi apiImpl = new UserApiImpl();

    public UserApiHandler(Map<String, Handler<RoutingContext>> operationHandlers) {
        operationHandlers.put("createUser", this::createUser);
        operationHandlers.put("createUsersWithArrayInput", this::createUsersWithArrayInput);
        operationHandlers.put("createUsersWithListInput", this::createUsersWithListInput);
        operationHandlers.put("deleteUser", this::deleteUser);
        operationHandlers.put("getUserByName", this::getUserByName);
        operationHandlers.put("loginUser", this::loginUser);
        operationHandlers.put("logoutUser", this::logoutUser);
        operationHandlers.put("updateUser", this::updateUser);
    }

    private void createUser(RoutingContext routingContext) {
        logger.info("createUser()");
        HttpServerResponse response = routingContext.response();

        Single.defer( () -> {

            String jsonString = routingContext.getBodyAsString();
            User user = jsonString == null ? null : Json.decodeValue(jsonString, new TypeReference<User>(){});
            logger.info("Parameter user is {}", user);
            return apiImpl.createUser(user);
        })
        .subscribe(
            apiResponse -> {
                response.setStatusCode(apiResponse.getStatusCode())
                        .end(Json.encodePrettily(apiResponse.getData()));
            }, error -> {
                if (error instanceof ApiException) {
                    ApiException apiException = (ApiException) error;
                    response.setStatusCode(apiException.getStatusCode()).end(apiException.getMessage());
                } else {
                    response.setStatusCode(500).end(error.getMessage());
                }
            }).dispose();
    }


    private void createUsersWithArrayInput(RoutingContext routingContext) {
        logger.info("createUsersWithArrayInput()");
        HttpServerResponse response = routingContext.response();

        Single.defer( () -> {

            String jsonString = routingContext.getBodyAsString();
            List<User> user = jsonString == null ? null : Json.decodeValue(jsonString, new TypeReference<List<User>>(){});
            logger.info("Parameter user is {}", user);
            return apiImpl.createUsersWithArrayInput(user);
        })
        .subscribe(
            apiResponse -> {
                response.setStatusCode(apiResponse.getStatusCode())
                        .end(Json.encodePrettily(apiResponse.getData()));
            }, error -> {
                if (error instanceof ApiException) {
                    ApiException apiException = (ApiException) error;
                    response.setStatusCode(apiException.getStatusCode()).end(apiException.getMessage());
                } else {
                    response.setStatusCode(500).end(error.getMessage());
                }
            }).dispose();
    }


    private void createUsersWithListInput(RoutingContext routingContext) {
        logger.info("createUsersWithListInput()");
        HttpServerResponse response = routingContext.response();

        Single.defer( () -> {

            String jsonString = routingContext.getBodyAsString();
            List<User> user = jsonString == null ? null : Json.decodeValue(jsonString, new TypeReference<List<User>>(){});
            logger.info("Parameter user is {}", user);
            return apiImpl.createUsersWithListInput(user);
        })
        .subscribe(
            apiResponse -> {
                response.setStatusCode(apiResponse.getStatusCode())
                        .end(Json.encodePrettily(apiResponse.getData()));
            }, error -> {
                if (error instanceof ApiException) {
                    ApiException apiException = (ApiException) error;
                    response.setStatusCode(apiException.getStatusCode()).end(apiException.getMessage());
                } else {
                    response.setStatusCode(500).end(error.getMessage());
                }
            }).dispose();
    }


    private void deleteUser(RoutingContext routingContext) {
        logger.info("deleteUser()");
        HttpServerResponse response = routingContext.response();

        Single.defer( () -> {
            String username = ParameterCast.toString(routingContext.pathParams().get("username"));

            logger.info("Parameter username is {}", username);
            return apiImpl.deleteUser(username);
        })
        .subscribe(
            apiResponse -> {
                response.setStatusCode(apiResponse.getStatusCode())
                        .end(Json.encodePrettily(apiResponse.getData()));
            }, error -> {
                if (error instanceof ApiException) {
                    ApiException apiException = (ApiException) error;
                    response.setStatusCode(apiException.getStatusCode()).end(apiException.getMessage());
                } else {
                    response.setStatusCode(500).end(error.getMessage());
                }
            }).dispose();
    }


    private void getUserByName(RoutingContext routingContext) {
        logger.info("getUserByName()");
        HttpServerResponse response = routingContext.response();

        Single.defer( () -> {
            String username = ParameterCast.toString(routingContext.pathParams().get("username"));

            logger.info("Parameter username is {}", username);
            return apiImpl.getUserByName(username);
        })
        .subscribe(
            apiResponse -> {
                response.setStatusCode(apiResponse.getStatusCode())
                        .end(Json.encodePrettily(apiResponse.getData()));
            }, error -> {
                if (error instanceof ApiException) {
                    ApiException apiException = (ApiException) error;
                    response.setStatusCode(apiException.getStatusCode()).end(apiException.getMessage());
                } else {
                    response.setStatusCode(500).end(error.getMessage());
                }
            }).dispose();
    }


    private void loginUser(RoutingContext routingContext) {
        logger.info("loginUser()");
        HttpServerResponse response = routingContext.response();

        Single.defer( () -> {
            String username = ParameterCast.toString(routingContext.queryParams().get("username"));
            String password = ParameterCast.toString(routingContext.queryParams().get("password"));

            logger.info("Parameter username is {}", username);
            logger.info("Parameter password is {}", password);
            return apiImpl.loginUser(username, password);
        })
        .subscribe(
            apiResponse -> {
                response.setStatusCode(apiResponse.getStatusCode())
                        .end(Json.encodePrettily(apiResponse.getData()));
            }, error -> {
                if (error instanceof ApiException) {
                    ApiException apiException = (ApiException) error;
                    response.setStatusCode(apiException.getStatusCode()).end(apiException.getMessage());
                } else {
                    response.setStatusCode(500).end(error.getMessage());
                }
            }).dispose();
    }


    private void logoutUser(RoutingContext routingContext) {
        logger.info("logoutUser()");
        HttpServerResponse response = routingContext.response();

        Single.defer( () -> {

            return apiImpl.logoutUser();
        })
        .subscribe(
            apiResponse -> {
                response.setStatusCode(apiResponse.getStatusCode())
                        .end(Json.encodePrettily(apiResponse.getData()));
            }, error -> {
                if (error instanceof ApiException) {
                    ApiException apiException = (ApiException) error;
                    response.setStatusCode(apiException.getStatusCode()).end(apiException.getMessage());
                } else {
                    response.setStatusCode(500).end(error.getMessage());
                }
            }).dispose();
    }


    private void updateUser(RoutingContext routingContext) {
        logger.info("updateUser()");
        HttpServerResponse response = routingContext.response();

        Single.defer( () -> {
            String username = ParameterCast.toString(routingContext.pathParams().get("username"));

            String jsonString = routingContext.getBodyAsString();
            User user = jsonString == null ? null : Json.decodeValue(jsonString, new TypeReference<User>(){});
            logger.info("Parameter username is {}", username);
            logger.info("Parameter user is {}", user);
            return apiImpl.updateUser(username, user);
        })
        .subscribe(
            apiResponse -> {
                response.setStatusCode(apiResponse.getStatusCode())
                        .end(Json.encodePrettily(apiResponse.getData()));
            }, error -> {
                if (error instanceof ApiException) {
                    ApiException apiException = (ApiException) error;
                    response.setStatusCode(apiException.getStatusCode()).end(apiException.getMessage());
                } else {
                    response.setStatusCode(500).end(error.getMessage());
                }
            }).dispose();
    }

}
