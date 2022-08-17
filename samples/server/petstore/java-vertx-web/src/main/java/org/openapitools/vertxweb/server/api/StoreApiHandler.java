package org.openapitools.vertxweb.server.api;

import org.openapitools.vertxweb.server.model.Order;

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

public class StoreApiHandler {

    private static final Logger logger = LoggerFactory.getLogger(StoreApiHandler.class);

    private final StoreApi api;

    public StoreApiHandler(StoreApi api) {
        this.api = api;
    }

    @Deprecated
    public StoreApiHandler() {
        this(new StoreApiImpl());
    }

    public void mount(RouterBuilder builder) {
        builder.operation("deleteOrder").handler(this::deleteOrder);
        builder.operation("getInventory").handler(this::getInventory);
        builder.operation("getOrderById").handler(this::getOrderById);
        builder.operation("placeOrder").handler(this::placeOrder);
    }

    private void deleteOrder(RoutingContext routingContext) {
        logger.info("deleteOrder()");

        // Param extraction
        RequestParameters requestParameters = routingContext.get(ValidationHandler.REQUEST_CONTEXT_KEY);

        String orderId = requestParameters.pathParameter("orderId") != null ? requestParameters.pathParameter("orderId").getString() : null;

        logger.debug("Parameter orderId is {}", orderId);

        api.deleteOrder(orderId)
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

    private void getInventory(RoutingContext routingContext) {
        logger.info("getInventory()");

        // Param extraction
        RequestParameters requestParameters = routingContext.get(ValidationHandler.REQUEST_CONTEXT_KEY);



        api.getInventory()
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

    private void getOrderById(RoutingContext routingContext) {
        logger.info("getOrderById()");

        // Param extraction
        RequestParameters requestParameters = routingContext.get(ValidationHandler.REQUEST_CONTEXT_KEY);

        Long orderId = requestParameters.pathParameter("orderId") != null ? requestParameters.pathParameter("orderId").getLong() : null;

        logger.debug("Parameter orderId is {}", orderId);

        api.getOrderById(orderId)
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

    private void placeOrder(RoutingContext routingContext) {
        logger.info("placeOrder()");

        // Param extraction
        RequestParameters requestParameters = routingContext.get(ValidationHandler.REQUEST_CONTEXT_KEY);

        RequestParameter body = requestParameters.body();
        Order order = body != null ? DatabindCodec.mapper().convertValue(body.get(), new TypeReference<Order>(){}) : null;

        logger.debug("Parameter order is {}", order);

        api.placeOrder(order)
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
