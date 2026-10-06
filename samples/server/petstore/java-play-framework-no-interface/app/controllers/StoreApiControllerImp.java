package controllers;

import java.util.Map;
import apimodels.Order;

import play.mvc.Http;
import java.util.HashMap;
import javax.validation.constraints.*;
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaPlayFrameworkCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
public class StoreApiControllerImp  {
    
    public void deleteOrder(Http.Request request, String orderId) throws Exception {
        //Do your magic!!!
    }

    
    public Map<String, Integer> getInventory(Http.Request request) throws Exception {
        //Do your magic!!!
        return new HashMap<String, Integer>();
    }

    
    public Order getOrderById(Http.Request request,  @Min(1) @Max(5)Long orderId) throws Exception {
        //Do your magic!!!
        return new Order();
    }

    
    public Order placeOrder(Http.Request request, Order body) throws Exception {
        //Do your magic!!!
        return new Order();
    }

}
