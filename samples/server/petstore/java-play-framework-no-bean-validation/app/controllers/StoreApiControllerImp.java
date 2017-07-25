package controllers;

import java.util.Map;
import apimodels.Order;

import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;
import java.io.FileInputStream;

public class StoreApiControllerImp implements StoreApiControllerImpInterface {

    private final ObjectMapper mapper;

    @Inject
    private StoreApiControllerImp() {
        mapper = new ObjectMapper();
    }

    @Override
    public void deleteOrder(String orderId) throws Exception {
        //Do your magic!!!
        
    }

    @Override
    public Map<String, Integer> getInventory() throws Exception {
        //Do your magic!!!
        String accept = request().getHeader("Accept");
        if (accept != null && accept.contains("application/json")) {
            return mapper.readValue("", Map.class);
        }
        return new HashMap<String, Integer>();
    }

    @Override
    public Order getOrderById(Long orderId) throws Exception {
        //Do your magic!!!
        String accept = request().getHeader("Accept");
        if (accept != null && accept.contains("application/xml")) {
            return mapper.readValue("", Order.class);
        }
        String accept = request().getHeader("Accept");
        if (accept != null && accept.contains("application/json")) {
            return mapper.readValue("", Order.class);
        }
        return new Order();
    }

    @Override
    public Order placeOrder(Order body) throws Exception {
        //Do your magic!!!
        String accept = request().getHeader("Accept");
        if (accept != null && accept.contains("application/xml")) {
            return mapper.readValue("", Order.class);
        }
        String accept = request().getHeader("Accept");
        if (accept != null && accept.contains("application/json")) {
            return mapper.readValue("", Order.class);
        }
        return new Order();
    }

}
