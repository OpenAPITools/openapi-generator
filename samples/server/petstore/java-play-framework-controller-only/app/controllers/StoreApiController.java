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
import java.io.IOException;
import swagger.SwaggerUtils;
import com.fasterxml.jackson.core.type.TypeReference;

import javax.validation.constraints.*;

import swagger.SwaggerUtils.ApiAction;


public class StoreApiController extends Controller {

    
    private final ObjectMapper mapper;

    @Inject
    private StoreApiController() {
        
        mapper = new ObjectMapper();
    }


    @ApiAction
    public Result deleteOrder(String orderId) throws Exception {

        return ok();
    }

    @ApiAction
    public Result getInventory() throws Exception {

        return ok();
    }

    @ApiAction
    public Result getOrderById( @Min(1) @Max(5)Long orderId) throws Exception {

        return ok();
    }

    @ApiAction
    public Result placeOrder() throws Exception {
        JsonNode nodebody = request().body().asJson();
        Order body;

        body = mapper.readValue(nodebody.toString(), Order.class);
        body.validate();


        return ok();
    }
}
