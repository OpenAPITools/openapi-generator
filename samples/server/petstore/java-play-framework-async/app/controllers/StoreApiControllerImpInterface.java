package controllers;

import java.util.Map;
import apimodels.Order;

import com.google.inject.Inject;
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
import openapitools.SecurityAPIUtils;
import static play.mvc.Results.ok;
import static play.mvc.Results.unauthorized;
import play.libs.Files.TemporaryFile;
import java.util.concurrent.CompletionException;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.CompletableFuture;

import javax.validation.constraints.*;

@SuppressWarnings("RedundantThrows")
public abstract class StoreApiControllerImpInterface {
    @Inject private Config configuration;
    @Inject private SecurityAPIUtils securityAPIUtils;
    private ObjectMapper mapper = new ObjectMapper();

    public CompletionStage<Result> deleteOrderHttp(Http.Request request, String orderId) throws Exception {
        CompletableFuture<Result> result = CompletableFuture.supplyAsync(() -> {
        try {
            deleteOrder(request, orderId);
        } catch (Exception e) {
            throw new CompletionException(e);
        }
        return ok();
    });
    return result;

    }

    public abstract void deleteOrder(Http.Request request, String orderId) throws Exception;

    public CompletionStage<Result> getInventoryHttp(Http.Request request) throws Exception {
        CompletionStage<Map<String, Integer>> stage = getInventory(request).thenApply(obj -> { 
        return obj;
    });
return stage.thenApply(obj -> {
            JsonNode result = mapper.valueToTree(obj);

            return ok(result);
    });

    }

    public abstract CompletionStage<Map<String, Integer>> getInventory(Http.Request request) throws Exception;

    public CompletionStage<Result> getOrderByIdHttp(Http.Request request,  @Min(1) @Max(5)Long orderId) throws Exception {
        CompletionStage<Order> stage = getOrderById(request, orderId).thenApply(obj -> { 

        if (configuration.getBoolean("useOutputBeanValidation")) {
            OpenAPIUtils.validate(obj);
        }

        return obj;
    });
return stage.thenApply(obj -> {
            JsonNode result = mapper.valueToTree(obj);

            return ok(result);
    });

    }

    public abstract CompletionStage<Order> getOrderById(Http.Request request,  @Min(1) @Max(5)Long orderId) throws Exception;

    public CompletionStage<Result> placeOrderHttp(Http.Request request, Order body) throws Exception {
        CompletionStage<Order> stage = placeOrder(request, body).thenApply(obj -> { 

        if (configuration.getBoolean("useOutputBeanValidation")) {
            OpenAPIUtils.validate(obj);
        }

        return obj;
    });
return stage.thenApply(obj -> {
            JsonNode result = mapper.valueToTree(obj);

            return ok(result);
    });

    }

    public abstract CompletionStage<Order> placeOrder(Http.Request request, Order body) throws Exception;

}
