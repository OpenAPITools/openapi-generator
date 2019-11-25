package org.openapitools.vertxweb.server.api;

import org.openapitools.vertxweb.server.model.Order;

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

public class StoreApiHandler {

    private static final Logger logger = LoggerFactory.getLogger(StoreApiHandler.class);
    private StoreApi apiImpl = new StoreApiImpl();

    public StoreApiHandler(Map<String, Handler<RoutingContext>> operationHandlers) {
        operationHandlers.put("deleteOrder", this::deleteOrder);
        operationHandlers.put("getInventory", this::getInventory);
        operationHandlers.put("getOrderById", this::getOrderById);
        operationHandlers.put("placeOrder", this::placeOrder);
    }

    private void deleteOrder(RoutingContext routingContext) {
        logger.info("deleteOrder()");
        HttpServerResponse response = routingContext.response();

        Single.defer( () -> {
            String orderId = ParameterCast.toString(routingContext.pathParams().get("orderId"));

            logger.info("Parameter orderId is {}", orderId);
            return apiImpl.deleteOrder(orderId);
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


    private void getInventory(RoutingContext routingContext) {
        logger.info("getInventory()");
        HttpServerResponse response = routingContext.response();

        Single.defer( () -> {

            return apiImpl.getInventory();
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


    private void getOrderById(RoutingContext routingContext) {
        logger.info("getOrderById()");
        HttpServerResponse response = routingContext.response();

        Single.defer( () -> {
            Long orderId = ParameterCast.toLong(routingContext.pathParams().get("orderId"));

            logger.info("Parameter orderId is {}", orderId);
            return apiImpl.getOrderById(orderId);
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


    private void placeOrder(RoutingContext routingContext) {
        logger.info("placeOrder()");
        HttpServerResponse response = routingContext.response();

        Single.defer( () -> {

            String jsonString = routingContext.getBodyAsString();
            Order order = jsonString == null ? null : Json.decodeValue(jsonString, new TypeReference<Order>(){});
            logger.info("Parameter order is {}", order);
            return apiImpl.placeOrder(order);
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
