package controllers;

import java.util.Map;
import apimodels.Order;

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
public abstract class StoreApiControllerImpInterface {
    @Inject private Config configuration;
    private ObjectMapper mapper = new ObjectMapper();

    CompletionStage<Result> deleteOrderHttp(Http.Request request, String orderId) throws Exception {
        return CompletableFuture.supplyAsync(() -> {
    deleteOrder(request, orderId);
    return ok();
});

    }

    abstract void deleteOrder(Http.Request request, String orderId) throws Exception;

    CompletionStage<Result> getInventoryHttp(Http.Request request) throws Exception {
        CompletionStage<Map<String, Integer>> stage = getInventory(request).thenApply(obj -> { 
    return obj;
});
    stage.thenApply(obj -> {
    JsonNode result = mapper.valueToTree(obj);
    return ok(result);
});

    }

    abstract CompletionStage<Map<String, Integer>> getInventory(Http.Request request) throws Exception;

    CompletionStage<Result> getOrderByIdHttp(Http.Request request,  @Min(1) @Max(5)Long orderId) throws Exception {
        CompletionStage<Order> stage = getOrderById(request, orderId).thenApply(obj -> { 
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

    abstract CompletionStage<Order> getOrderById(Http.Request request,  @Min(1) @Max(5)Long orderId) throws Exception;

    CompletionStage<Result> placeOrderHttp(Http.Request request, Order body) throws Exception {
        CompletionStage<Order> stage = placeOrder(request, body).thenApply(obj -> { 
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

    abstract CompletionStage<Order> placeOrder(Http.Request request, Order body) throws Exception;

}
