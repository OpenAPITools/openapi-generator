package controllers;

import java.util.Map;
import apimodels.Order;

import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;
import javax.validation.constraints.*;

public class StoreApiControllerImp {
    void deleteOrder( String orderId) {
        //Do your magic!!!
        
    }

    Map<String, Integer> getInventory() {
        //Do your magic!!!
        return new HashMap<String, Integer>();
    }

    Order getOrderById( String orderId) {
        //Do your magic!!!
        return new Order();
    }

    Order placeOrder(Order body) {
        //Do your magic!!!
        return new Order();
    }

}
