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
public class StoreApiControllerImp implements StoreApiControllerImpInterface {
    @Override
    public void deleteOrder(Http.Request request, String orderId)  {
        //Do your magic!!!
    }

    @Override
    public Map<String, Integer> getInventory(Http.Request request)  {
        //Do your magic!!!
        return new HashMap<String, Integer>();
    }

    @Override
    public Order getOrderById(Http.Request request,  @Min(1) @Max(5)Long orderId)  {
        //Do your magic!!!
        return new Order();
    }

    @Override
    public Order placeOrder(Http.Request request, Order body)  {
        //Do your magic!!!
        return new Order();
    }

}
