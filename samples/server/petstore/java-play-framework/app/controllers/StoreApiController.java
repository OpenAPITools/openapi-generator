package controllers;

import java.util.Map;
import apimodels.Order;

import play.mvc.Controller;
import play.mvc.Result;
import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.JsonNode;
import com.google.inject.Inject;
import java.io.IOException;
import swagger.SwaggerUtils;
import com.fasterxml.jackson.core.type.TypeReference;

import javax.validation.constraints.*;

import swagger.SwaggerUtils.ApiAction;


public class StoreApiController extends Controller {

    private StoreApiControllerImp imp;
    private ObjectMapper mapper;

    @Inject
    private StoreApiController(StoreApiControllerImp imp) {
        this.imp = imp;
        mapper = new ObjectMapper();
    }


    @ApiAction
    public Result deleteOrder(String orderId) throws Exception {
        imp.deleteOrder(orderId);
        
        return ok();
    }

    @ApiAction
    public Result getInventory() throws Exception {
        Map<String, Integer> obj = imp.getInventory();
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
        
    }

    @ApiAction
    public Result getOrderById(String orderId) throws Exception {
        Order obj = imp.getOrderById(orderId);
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
        
    }

    @ApiAction
    public Result placeOrder() throws Exception {
        JsonNode nodebody = request().body().asJson();
        Order body;
        if (nodebody != null) {
            body = mapper.readValue(nodebody.toString(), Order.class);
        
        } else {
            body = null;
        }
        Order obj = imp.placeOrder(body);
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
        
    }
}
