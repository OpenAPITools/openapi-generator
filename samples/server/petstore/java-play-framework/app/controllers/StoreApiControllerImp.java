package controllers;

import java.util.Map;
import apimodels.Order;

import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;
import javax.validation.constraints.*;

public class StoreApiControllerImp implements StoreApiControllerImpInterface {
    public void deleteOrder(String orderId) {
        //Do your magic!!!
        
    }

    public Map<String, Integer> getInventory() {
        //Do your magic!!!
        return new HashMap<String, Integer>();
    }

    public Order getOrderById(String orderId) {
        //Do your magic!!!
        return new Order();
    }

    public Order placeOrder(Order body) {
        //Do your magic!!!
        return new Order();
    }

}
