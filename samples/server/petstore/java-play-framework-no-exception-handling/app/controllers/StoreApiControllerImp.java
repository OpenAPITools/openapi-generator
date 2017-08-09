package controllers;

import java.util.Map;
import apimodels.Order;

import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;
import java.io.FileInputStream;
import javax.validation.constraints.*;

public class StoreApiControllerImp implements StoreApiControllerImpInterface {
    @Override
    public void deleteOrder(String orderId)  {
        //Do your magic!!!
        
    }

    @Override
    public Map<String, Integer> getInventory()  {
        //Do your magic!!!
        return new HashMap<String, Integer>();
    }

    @Override
    public Order getOrderById( @Min(1) @Max(5)Long orderId)  {
        //Do your magic!!!
        return new Order();
    }

    @Override
    public Order placeOrder(Order body)  {
        //Do your magic!!!
        return new Order();
    }

}
