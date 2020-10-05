package controllers;

import java.util.Map;
import apimodels.Order;

import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.CompletableFuture;

import javax.validation.constraints.*;

@SuppressWarnings("RedundantThrows")
public interface StoreApiControllerImpInterface {
    default CompletionStage<Result> deleteOrderHttp(Http.Request request, String orderId) throws Exception {
        return CompletableFuture.supplyAsync(() -> {
            deleteOrder(request, orderId);
            return ok();
        });
    }

    void deleteOrder(Http.Request request, String orderId) throws Exception;

    default CompletionStage<Result> getInventoryHttp(Http.Request request) throws Exception {
        CompletionStage<Map<String, Integer>> stage = getInventory(request).thenApply(obj -> { 
            return obj;
        });
            stage.thenApply(obj -> {
            JsonNode result = mapper.valueToTree(obj);
            return ok(result);
        });
    }

    CompletionStage<Map<String, Integer>> getInventory(Http.Request request) throws Exception;

    default CompletionStage<Result> getOrderByIdHttp(Http.Request request,  @Min(1) @Max(5)Long orderId) throws Exception {
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

    CompletionStage<Order> getOrderById(Http.Request request,  @Min(1) @Max(5)Long orderId) throws Exception;

    default CompletionStage<Result> placeOrderHttp(Http.Request request, Order body) throws Exception {
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

    CompletionStage<Order> placeOrder(Http.Request request, Order body) throws Exception;

}
