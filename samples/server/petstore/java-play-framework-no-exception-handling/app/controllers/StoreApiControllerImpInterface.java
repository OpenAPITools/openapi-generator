package controllers;

import java.util.Map;
import apimodels.Order;

import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;

import javax.validation.constraints.*;

@SuppressWarnings("RedundantThrows")
public interface StoreApiControllerImpInterface {
    void deleteOrder(Request request, String orderId) ;

    Map<String, Integer> getInventory(Request request) ;

    Order getOrderById(Request request,  @Min(1) @Max(5)Long orderId) ;

    Order placeOrder(Request request, Order body) ;

}
