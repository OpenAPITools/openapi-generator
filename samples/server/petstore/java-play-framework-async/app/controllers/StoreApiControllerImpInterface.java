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
    void deleteOrder(Http.Request request, String orderId) throws Exception;

    CompletionStage<Map<String, Integer>> getInventory(Http.Request request) throws Exception;

    CompletionStage<Order> getOrderById(Http.Request request,  @Min(1) @Max(5)Long orderId) throws Exception;

    CompletionStage<Order> placeOrder(Http.Request request, Order body) throws Exception;

}
