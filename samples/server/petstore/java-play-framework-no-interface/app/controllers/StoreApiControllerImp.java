package controllers;

import java.util.Map;
import apimodels.Order;

import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.io.FileInputStream;
import javax.validation.constraints.*;
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaPlayFrameworkCodegen")
public class StoreApiControllerImp  {
    
    public void deleteOrder(Http.Request request, String orderId) throws Exception {
        //Do your magic!!!
    }

    
    public  getInventory(Http.Request request) throws Exception {
        //Do your magic!!!
        return new ();
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
