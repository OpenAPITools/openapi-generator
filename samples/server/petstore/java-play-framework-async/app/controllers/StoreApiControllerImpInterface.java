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
    void deleteOrder(Request request, String orderId) throws Exception;

    CompletionStage<Map<String, Integer>> getInventory(Request request) throws Exception;

    CompletionStage<Order> getOrderById(Request request,  @Min(1) @Max(5)Long orderId) throws Exception;

    CompletionStage<Order> placeOrder(Request request, Order body) throws Exception;

}
