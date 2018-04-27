package org.openapitools.server.api.verticle;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.eventbus.Message;
import io.vertx.core.json.Json;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

import org.openapitools.server.api.MainApiException;
import org.openapitools.server.api.model.User;

import java.util.List;
import java.util.Map;

public class UserApiVerticle extends AbstractVerticle {
    final static Logger LOGGER = LoggerFactory.getLogger(UserApiVerticle.class); 
    
    final static String CREATEUSER_SERVICE_ID = "createUser";
    final static String CREATEUSERSWITHARRAYINPUT_SERVICE_ID = "createUsersWithArrayInput";
    final static String CREATEUSERSWITHLISTINPUT_SERVICE_ID = "createUsersWithListInput";
    final static String DELETEUSER_SERVICE_ID = "deleteUser";
    final static String GETUSERBYNAME_SERVICE_ID = "getUserByName";
    final static String LOGINUSER_SERVICE_ID = "loginUser";
    final static String LOGOUTUSER_SERVICE_ID = "logoutUser";
    final static String UPDATEUSER_SERVICE_ID = "updateUser";
    
    final UserApi service;

    public UserApiVerticle() {
        try {
            Class serviceImplClass = getClass().getClassLoader().loadClass("org.openapitools.server.api.verticle.UserApiImpl");
            service = (UserApi)serviceImplClass.newInstance();
        } catch (Exception e) {
            logUnexpectedError("UserApiVerticle constructor", e);
            throw new RuntimeException(e);
        }
    }

    @Override
    public void start() throws Exception {
        
        //Consumer for createUser
        vertx.eventBus().<JsonObject> consumer(CREATEUSER_SERVICE_ID).handler(message -> {
            try {
                // Workaround for #allParams section clearing the vendorExtensions map
                String serviceId = "createUser";
                JsonObject userParam = message.body().getJsonObject("User");
                if (userParam == null) {
                    manageError(message, new MainApiException(400, "User is required"), serviceId);
                    return;
                }
                User user = Json.mapper.readValue(userParam.encode(), User.class);
                service.createUser(user).subscribe(
                    () -> {
                        message.reply(null);
                    },
                    error -> {
                        manageError(message, error, "createUser");
                    });
            } catch (Exception e) {
                logUnexpectedError("createUser", e);
                message.fail(MainApiException.INTERNAL_SERVER_ERROR.getStatusCode(), MainApiException.INTERNAL_SERVER_ERROR.getStatusMessage());
            }
        });
        
        //Consumer for createUsersWithArrayInput
        vertx.eventBus().<JsonObject> consumer(CREATEUSERSWITHARRAYINPUT_SERVICE_ID).handler(message -> {
            try {
                // Workaround for #allParams section clearing the vendorExtensions map
                String serviceId = "createUsersWithArrayInput";
                JsonArray userParam = message.body().getJsonArray("User");
                if(userParam == null) {
                    manageError(message, new MainApiException(400, "User is required"), serviceId);
                    return;
                }
                List<User> user = Json.mapper.readValue(userParam.encode(),
                    Json.mapper.getTypeFactory().constructCollectionType(List.class, List.class));
                service.createUsersWithArrayInput(user).subscribe(
                    () -> {
                        message.reply(null);
                    },
                    error -> {
                        manageError(message, error, "createUsersWithArrayInput");
                    });
            } catch (Exception e) {
                logUnexpectedError("createUsersWithArrayInput", e);
                message.fail(MainApiException.INTERNAL_SERVER_ERROR.getStatusCode(), MainApiException.INTERNAL_SERVER_ERROR.getStatusMessage());
            }
        });
        
        //Consumer for createUsersWithListInput
        vertx.eventBus().<JsonObject> consumer(CREATEUSERSWITHLISTINPUT_SERVICE_ID).handler(message -> {
            try {
                // Workaround for #allParams section clearing the vendorExtensions map
                String serviceId = "createUsersWithListInput";
                JsonArray userParam = message.body().getJsonArray("User");
                if(userParam == null) {
                    manageError(message, new MainApiException(400, "User is required"), serviceId);
                    return;
                }
                List<User> user = Json.mapper.readValue(userParam.encode(),
                    Json.mapper.getTypeFactory().constructCollectionType(List.class, List.class));
                service.createUsersWithListInput(user).subscribe(
                    () -> {
                        message.reply(null);
                    },
                    error -> {
                        manageError(message, error, "createUsersWithListInput");
                    });
            } catch (Exception e) {
                logUnexpectedError("createUsersWithListInput", e);
                message.fail(MainApiException.INTERNAL_SERVER_ERROR.getStatusCode(), MainApiException.INTERNAL_SERVER_ERROR.getStatusMessage());
            }
        });
        
        //Consumer for deleteUser
        vertx.eventBus().<JsonObject> consumer(DELETEUSER_SERVICE_ID).handler(message -> {
            try {
                // Workaround for #allParams section clearing the vendorExtensions map
                String serviceId = "deleteUser";
                String usernameParam = message.body().getString("username");
                if(usernameParam == null) {
                    manageError(message, new MainApiException(400, "username is required"), serviceId);
                    return;
                }
                String username = usernameParam;
                service.deleteUser(username).subscribe(
                    () -> {
                        message.reply(null);
                    },
                    error -> {
                        manageError(message, error, "deleteUser");
                    });
            } catch (Exception e) {
                logUnexpectedError("deleteUser", e);
                message.fail(MainApiException.INTERNAL_SERVER_ERROR.getStatusCode(), MainApiException.INTERNAL_SERVER_ERROR.getStatusMessage());
            }
        });
        
        //Consumer for getUserByName
        vertx.eventBus().<JsonObject> consumer(GETUSERBYNAME_SERVICE_ID).handler(message -> {
            try {
                // Workaround for #allParams section clearing the vendorExtensions map
                String serviceId = "getUserByName";
                String usernameParam = message.body().getString("username");
                if(usernameParam == null) {
                    manageError(message, new MainApiException(400, "username is required"), serviceId);
                    return;
                }
                String username = usernameParam;
                service.getUserByName(username).subscribe(
                    result -> {
                        message.reply(new JsonObject(Json.encode(result)).encodePrettily());
                    },
                    error -> {
                        manageError(message, error, "getUserByName");
                    });
            } catch (Exception e) {
                logUnexpectedError("getUserByName", e);
                message.fail(MainApiException.INTERNAL_SERVER_ERROR.getStatusCode(), MainApiException.INTERNAL_SERVER_ERROR.getStatusMessage());
            }
        });
        
        //Consumer for loginUser
        vertx.eventBus().<JsonObject> consumer(LOGINUSER_SERVICE_ID).handler(message -> {
            try {
                // Workaround for #allParams section clearing the vendorExtensions map
                String serviceId = "loginUser";
                String usernameParam = message.body().getString("username");
                if(usernameParam == null) {
                    manageError(message, new MainApiException(400, "username is required"), serviceId);
                    return;
                }
                String username = usernameParam;
                String passwordParam = message.body().getString("password");
                if(passwordParam == null) {
                    manageError(message, new MainApiException(400, "password is required"), serviceId);
                    return;
                }
                String password = passwordParam;
                service.loginUser(username, password).subscribe(
                    result -> {
                        message.reply(new JsonObject(Json.encode(result)).encodePrettily());
                    },
                    error -> {
                        manageError(message, error, "loginUser");
                    });
            } catch (Exception e) {
                logUnexpectedError("loginUser", e);
                message.fail(MainApiException.INTERNAL_SERVER_ERROR.getStatusCode(), MainApiException.INTERNAL_SERVER_ERROR.getStatusMessage());
            }
        });
        
        //Consumer for logoutUser
        vertx.eventBus().<JsonObject> consumer(LOGOUTUSER_SERVICE_ID).handler(message -> {
            try {
                // Workaround for #allParams section clearing the vendorExtensions map
                String serviceId = "logoutUser";
                service.logoutUser().subscribe(
                    () -> {
                        message.reply(null);
                    },
                    error -> {
                        manageError(message, error, "logoutUser");
                    });
            } catch (Exception e) {
                logUnexpectedError("logoutUser", e);
                message.fail(MainApiException.INTERNAL_SERVER_ERROR.getStatusCode(), MainApiException.INTERNAL_SERVER_ERROR.getStatusMessage());
            }
        });
        
        //Consumer for updateUser
        vertx.eventBus().<JsonObject> consumer(UPDATEUSER_SERVICE_ID).handler(message -> {
            try {
                // Workaround for #allParams section clearing the vendorExtensions map
                String serviceId = "updateUser";
                String usernameParam = message.body().getString("username");
                if(usernameParam == null) {
                    manageError(message, new MainApiException(400, "username is required"), serviceId);
                    return;
                }
                String username = usernameParam;
                JsonObject userParam = message.body().getJsonObject("User");
                if (userParam == null) {
                    manageError(message, new MainApiException(400, "User is required"), serviceId);
                    return;
                }
                User user = Json.mapper.readValue(userParam.encode(), User.class);
                service.updateUser(username, user).subscribe(
                    () -> {
                        message.reply(null);
                    },
                    error -> {
                        manageError(message, error, "updateUser");
                    });
            } catch (Exception e) {
                logUnexpectedError("updateUser", e);
                message.fail(MainApiException.INTERNAL_SERVER_ERROR.getStatusCode(), MainApiException.INTERNAL_SERVER_ERROR.getStatusMessage());
            }
        });
        
    }
    
    private void manageError(Message<JsonObject> message, Throwable cause, String serviceName) {
        int code = MainApiException.INTERNAL_SERVER_ERROR.getStatusCode();
        String statusMessage = MainApiException.INTERNAL_SERVER_ERROR.getStatusMessage();
        if (cause instanceof MainApiException) {
            code = ((MainApiException)cause).getStatusCode();
            statusMessage = ((MainApiException)cause).getStatusMessage();
        } else {
            logUnexpectedError(serviceName, cause); 
        }
            
        message.fail(code, statusMessage);
    }
    
    private void logUnexpectedError(String serviceName, Throwable cause) {
        LOGGER.error("Unexpected error in "+ serviceName, cause);
    }
}
