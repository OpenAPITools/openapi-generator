package io.swagger.server.api.verticle;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.eventbus.Message;
import io.vertx.core.json.Json;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.core.logging.Logger;
import io.vertx.core.logging.LoggerFactory;

import io.swagger.server.api.MainApiException;
import io.swagger.server.api.model.Order;

import java.util.List;
import java.util.Map;

public class StoreApiVerticle extends AbstractVerticle {
    final static Logger LOGGER = LoggerFactory.getLogger(StoreApiVerticle.class); 
    
    final static String DELETEORDER_SERVICE_ID = "deleteOrder";
    final static String GETINVENTORY_SERVICE_ID = "getInventory";
    final static String GETORDERBYID_SERVICE_ID = "getOrderById";
    final static String PLACEORDER_SERVICE_ID = "placeOrder";
    
    final StoreApi service;

    public StoreApiVerticle() {
        try {
            Class serviceImplClass = getClass().getClassLoader().loadClass("io.swagger.server.api.verticle.StoreApiImpl");
            service = (StoreApi)serviceImplClass.newInstance();
        } catch (Exception e) {
            logUnexpectedError("StoreApiVerticle constructor", e);
            throw new RuntimeException(e);
        }
    }

    @Override
    public void start() throws Exception {
        
        //Consumer for deleteOrder
        vertx.eventBus().<JsonObject> consumer(DELETEORDER_SERVICE_ID).handler(message -> {
            try {
                // Workaround for #allParams section clearing the vendorExtensions map
                String serviceId = "deleteOrder";
                String orderIdParam = message.body().getString("orderId");
                if(orderIdParam == null) {
                    manageError(message, new MainApiException(400, "orderId is required"), serviceId);
                    return;
                }
                String orderId = orderIdParam;
                service.deleteOrder(orderId, result -> {
                    if (result.succeeded())
                        message.reply(null);
                    else {
                        Throwable cause = result.cause();
                        manageError(message, cause, "deleteOrder");
                    }
                });
            } catch (Exception e) {
                logUnexpectedError("deleteOrder", e);
                message.fail(MainApiException.INTERNAL_SERVER_ERROR.getStatusCode(), MainApiException.INTERNAL_SERVER_ERROR.getStatusMessage());
            }
        });
        
        //Consumer for getInventory
        vertx.eventBus().<JsonObject> consumer(GETINVENTORY_SERVICE_ID).handler(message -> {
            try {
                // Workaround for #allParams section clearing the vendorExtensions map
                String serviceId = "getInventory";
                service.getInventory(result -> {
                    if (result.succeeded())
                        message.reply(new JsonObject(Json.encode(result.result())).encodePrettily());
                    else {
                        Throwable cause = result.cause();
                        manageError(message, cause, "getInventory");
                    }
                });
            } catch (Exception e) {
                logUnexpectedError("getInventory", e);
                message.fail(MainApiException.INTERNAL_SERVER_ERROR.getStatusCode(), MainApiException.INTERNAL_SERVER_ERROR.getStatusMessage());
            }
        });
        
        //Consumer for getOrderById
        vertx.eventBus().<JsonObject> consumer(GETORDERBYID_SERVICE_ID).handler(message -> {
            try {
                // Workaround for #allParams section clearing the vendorExtensions map
                String serviceId = "getOrderById";
                String orderIdParam = message.body().getString("orderId");
                if(orderIdParam == null) {
                    manageError(message, new MainApiException(400, "orderId is required"), serviceId);
                    return;
                }
                Long orderId = Json.mapper.readValue(orderIdParam, Long.class);
                service.getOrderById(orderId, result -> {
                    if (result.succeeded())
                        message.reply(new JsonObject(Json.encode(result.result())).encodePrettily());
                    else {
                        Throwable cause = result.cause();
                        manageError(message, cause, "getOrderById");
                    }
                });
            } catch (Exception e) {
                logUnexpectedError("getOrderById", e);
                message.fail(MainApiException.INTERNAL_SERVER_ERROR.getStatusCode(), MainApiException.INTERNAL_SERVER_ERROR.getStatusMessage());
            }
        });
        
        //Consumer for placeOrder
        vertx.eventBus().<JsonObject> consumer(PLACEORDER_SERVICE_ID).handler(message -> {
            try {
                // Workaround for #allParams section clearing the vendorExtensions map
                String serviceId = "placeOrder";
                JsonObject bodyParam = message.body().getJsonObject("body");
                if (bodyParam == null) {
                    manageError(message, new MainApiException(400, "body is required"), serviceId);
                    return;
                }
                Order body = Json.mapper.readValue(bodyParam.encode(), Order.class);
                service.placeOrder(body, result -> {
                    if (result.succeeded())
                        message.reply(new JsonObject(Json.encode(result.result())).encodePrettily());
                    else {
                        Throwable cause = result.cause();
                        manageError(message, cause, "placeOrder");
                    }
                });
            } catch (Exception e) {
                logUnexpectedError("placeOrder", e);
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
