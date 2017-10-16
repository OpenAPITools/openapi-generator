package controllers;

import java.util.Map;
import apimodels.Order;

import play.mvc.Controller;
import play.mvc.Result;
import play.mvc.Http;
import java.util.List;
import java.util.Map;
import java.util.ArrayList;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.JsonNode;
import com.google.inject.Inject;
import java.io.File;
import swagger.SwaggerUtils;
import com.fasterxml.jackson.core.type.TypeReference;

import javax.validation.constraints.*;



public class StoreApiController extends Controller {

    private final StoreApiControllerImpInterface imp;
    private final ObjectMapper mapper;

    @Inject
    private StoreApiController(StoreApiControllerImpInterface imp) {
        this.imp = imp;
        mapper = new ObjectMapper();
    }


    
    public Result deleteOrder(String orderId) throws Exception {
        imp.deleteOrder(orderId);
        return ok();
    }

    
    public Result getInventory() throws Exception {
        Map<String, Integer> obj = imp.getInventory();
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
    }

    
    public Result getOrderById( @Min(1) @Max(5)Long orderId) throws Exception {
        Order obj = imp.getOrderById(orderId);
        obj.validate();
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
    }

    
    public Result placeOrder() throws Exception {
        JsonNode nodebody = request().body().asJson();
        Order body;
        if (nodebody != null) {
            body = mapper.readValue(nodebody.toString(), Order.class);
            body.validate();
        } else {
            throw new IllegalArgumentException("'body' parameter is required");
        }
        Order obj = imp.placeOrder(body);
        obj.validate();
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
    }
}
