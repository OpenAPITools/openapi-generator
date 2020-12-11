package controllers;

import java.util.Map;
import apimodels.Order;

import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.io.FileInputStream;
import play.libs.Files.TemporaryFile;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.CompletableFuture;
import javax.validation.constraints.*;
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaPlayFrameworkCodegen")
public class StoreApiControllerImp extends StoreApiControllerImpInterface {
    @Override
    public void deleteOrder(Http.Request request, String orderId) throws Exception {
        //Do your magic!!!
    }

    @Override
    public CompletionStage<Map<String, Integer>> getInventory(Http.Request request) throws Exception {
        //Do your magic!!!
        return CompletableFuture.supplyAsync(() -> {
           return new HashMap<String, Integer>();
        });
    }

    @Override
    public CompletionStage<Order> getOrderById(Http.Request request,  @Min(1) @Max(5)Long orderId) throws Exception {
        //Do your magic!!!
        return CompletableFuture.supplyAsync(() -> {
           return new Order();
        });
    }

    @Override
    public CompletionStage<Order> placeOrder(Http.Request request, Order body) throws Exception {
        //Do your magic!!!
        return CompletableFuture.supplyAsync(() -> {
           return new Order();
        });
    }

}
