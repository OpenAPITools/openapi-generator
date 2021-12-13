package org.openapitools.vertxweb.server.api;

import org.openapitools.vertxweb.server.model.User;

import com.fasterxml.jackson.core.type.TypeReference;
import io.vertx.core.json.jackson.DatabindCodec;
import io.vertx.ext.web.openapi.RouterBuilder;
import io.vertx.ext.web.validation.RequestParameters;
import io.vertx.ext.web.validation.RequestParameter;
import io.vertx.ext.web.validation.ValidationHandler;
import io.vertx.ext.web.RoutingContext;
import io.vertx.core.json.JsonObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Map;

public class UserApiHandler {

    private static final Logger logger = LoggerFactory.getLogger(UserApiHandler.class);

    private final UserApi api;

    public UserApiHandler(UserApi api) {
        this.api = api;
    }

    @Deprecated
    public UserApiHandler() {
        this(new UserApiImpl());
    }

    public void mount(RouterBuilder builder) {
        builder.operation("createUser").handler(this::createUser);
        builder.operation("createUsersWithArrayInput").handler(this::createUsersWithArrayInput);
        builder.operation("createUsersWithListInput").handler(this::createUsersWithListInput);
        builder.operation("deleteUser").handler(this::deleteUser);
        builder.operation("getUserByName").handler(this::getUserByName);
        builder.operation("loginUser").handler(this::loginUser);
        builder.operation("logoutUser").handler(this::logoutUser);
        builder.operation("updateUser").handler(this::updateUser);
    }

    private void createUser(RoutingContext routingContext) {
        logger.info("createUser()");

        // Param extraction
        RequestParameters requestParameters = routingContext.get(ValidationHandler.REQUEST_CONTEXT_KEY);

        RequestParameter body = requestParameters.body();
        User user = body != null ? DatabindCodec.mapper().convertValue(body.get(), new TypeReference<User>(){}) : null;

        logger.debug("Parameter user is {}", user);

        api.createUser(user)
            .onSuccess(apiResponse -> {
                routingContext.response().setStatusCode(apiResponse.getStatusCode());
                if (apiResponse.hasData()) {
                    routingContext.json(apiResponse.getData());
                } else {
                    routingContext.response().end();
                }
            })
            .onFailure(routingContext::fail);
    }

    private void createUsersWithArrayInput(RoutingContext routingContext) {
        logger.info("createUsersWithArrayInput()");

        // Param extraction
        RequestParameters requestParameters = routingContext.get(ValidationHandler.REQUEST_CONTEXT_KEY);

        RequestParameter body = requestParameters.body();
        List<User> user = body != null ? DatabindCodec.mapper().convertValue(body.get(), new TypeReference<List<User>>(){}) : null;

        logger.debug("Parameter user is {}", user);

        api.createUsersWithArrayInput(user)
            .onSuccess(apiResponse -> {
                routingContext.response().setStatusCode(apiResponse.getStatusCode());
                if (apiResponse.hasData()) {
                    routingContext.json(apiResponse.getData());
                } else {
                    routingContext.response().end();
                }
            })
            .onFailure(routingContext::fail);
    }

    private void createUsersWithListInput(RoutingContext routingContext) {
        logger.info("createUsersWithListInput()");

        // Param extraction
        RequestParameters requestParameters = routingContext.get(ValidationHandler.REQUEST_CONTEXT_KEY);

        RequestParameter body = requestParameters.body();
        List<User> user = body != null ? DatabindCodec.mapper().convertValue(body.get(), new TypeReference<List<User>>(){}) : null;

        logger.debug("Parameter user is {}", user);

        api.createUsersWithListInput(user)
            .onSuccess(apiResponse -> {
                routingContext.response().setStatusCode(apiResponse.getStatusCode());
                if (apiResponse.hasData()) {
                    routingContext.json(apiResponse.getData());
                } else {
                    routingContext.response().end();
                }
            })
            .onFailure(routingContext::fail);
    }

    private void deleteUser(RoutingContext routingContext) {
        logger.info("deleteUser()");

        // Param extraction
        RequestParameters requestParameters = routingContext.get(ValidationHandler.REQUEST_CONTEXT_KEY);

        String username = requestParameters.pathParameter("username") != null ? requestParameters.pathParameter("username").getString() : null;

        logger.debug("Parameter username is {}", username);

        api.deleteUser(username)
            .onSuccess(apiResponse -> {
                routingContext.response().setStatusCode(apiResponse.getStatusCode());
                if (apiResponse.hasData()) {
                    routingContext.json(apiResponse.getData());
                } else {
                    routingContext.response().end();
                }
            })
            .onFailure(routingContext::fail);
    }

    private void getUserByName(RoutingContext routingContext) {
        logger.info("getUserByName()");

        // Param extraction
        RequestParameters requestParameters = routingContext.get(ValidationHandler.REQUEST_CONTEXT_KEY);

        String username = requestParameters.pathParameter("username") != null ? requestParameters.pathParameter("username").getString() : null;

        logger.debug("Parameter username is {}", username);

        api.getUserByName(username)
            .onSuccess(apiResponse -> {
                routingContext.response().setStatusCode(apiResponse.getStatusCode());
                if (apiResponse.hasData()) {
                    routingContext.json(apiResponse.getData());
                } else {
                    routingContext.response().end();
                }
            })
            .onFailure(routingContext::fail);
    }

    private void loginUser(RoutingContext routingContext) {
        logger.info("loginUser()");

        // Param extraction
        RequestParameters requestParameters = routingContext.get(ValidationHandler.REQUEST_CONTEXT_KEY);

        String username = requestParameters.queryParameter("username") != null ? requestParameters.queryParameter("username").getString() : null;
        String password = requestParameters.queryParameter("password") != null ? requestParameters.queryParameter("password").getString() : null;

        logger.debug("Parameter username is {}", username);
        logger.debug("Parameter password is {}", password);

        api.loginUser(username, password)
            .onSuccess(apiResponse -> {
                routingContext.response().setStatusCode(apiResponse.getStatusCode());
                if (apiResponse.hasData()) {
                    routingContext.json(apiResponse.getData());
                } else {
                    routingContext.response().end();
                }
            })
            .onFailure(routingContext::fail);
    }

    private void logoutUser(RoutingContext routingContext) {
        logger.info("logoutUser()");

        // Param extraction
        RequestParameters requestParameters = routingContext.get(ValidationHandler.REQUEST_CONTEXT_KEY);



        api.logoutUser()
            .onSuccess(apiResponse -> {
                routingContext.response().setStatusCode(apiResponse.getStatusCode());
                if (apiResponse.hasData()) {
                    routingContext.json(apiResponse.getData());
                } else {
                    routingContext.response().end();
                }
            })
            .onFailure(routingContext::fail);
    }

    private void updateUser(RoutingContext routingContext) {
        logger.info("updateUser()");

        // Param extraction
        RequestParameters requestParameters = routingContext.get(ValidationHandler.REQUEST_CONTEXT_KEY);

        String username = requestParameters.pathParameter("username") != null ? requestParameters.pathParameter("username").getString() : null;
        RequestParameter body = requestParameters.body();
        User user = body != null ? DatabindCodec.mapper().convertValue(body.get(), new TypeReference<User>(){}) : null;

        logger.debug("Parameter username is {}", username);
        logger.debug("Parameter user is {}", user);

        api.updateUser(username, user)
            .onSuccess(apiResponse -> {
                routingContext.response().setStatusCode(apiResponse.getStatusCode());
                if (apiResponse.hasData()) {
                    routingContext.json(apiResponse.getData());
                } else {
                    routingContext.response().end();
                }
            })
            .onFailure(routingContext::fail);
    }

}
