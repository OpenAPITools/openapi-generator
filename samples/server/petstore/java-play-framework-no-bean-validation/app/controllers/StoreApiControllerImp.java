package controllers;

import java.util.Map;
import apimodels.Order;

import play.mvc.Http;
import java.util.HashMap;
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaPlayFrameworkCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
public class StoreApiControllerImp extends StoreApiControllerImpInterface {
    @Override
    public void deleteOrder(Http.Request request, String orderId) throws Exception {
        //Do your magic!!!
    }

    @Override
    public Map<String, Integer> getInventory(Http.Request request) throws Exception {
        //Do your magic!!!
        return new HashMap<String, Integer>();
    }

    @Override
    public Order getOrderById(Http.Request request, Long orderId) throws Exception {
        //Do your magic!!!
        return new Order();
    }

    @Override
    public Order placeOrder(Http.Request request, Order body) throws Exception {
        //Do your magic!!!
        return new Order();
    }

}
