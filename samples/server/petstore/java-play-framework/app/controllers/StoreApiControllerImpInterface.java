package controllers;

import java.util.Map;
import apimodels.Order;

import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;

public interface StoreApiControllerImpInterface {
    void deleteOrder( String orderId);

    Map<String, Integer> getInventory();

    Order getOrderById( String orderId);

    Order placeOrder(Order body);

}
