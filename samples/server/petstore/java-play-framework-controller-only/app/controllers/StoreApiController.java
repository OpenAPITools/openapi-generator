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
import openapitools.OpenAPIUtils;
import com.fasterxml.jackson.core.type.TypeReference;

import javax.validation.constraints.*;
import play.Configuration;

import openapitools.OpenAPIUtils.ApiAction;


public class StoreApiController extends Controller {

    private final ObjectMapper mapper;
    private final Configuration configuration;

    @Inject
    private StoreApiController(Configuration configuration) {
        mapper = new ObjectMapper();
        this.configuration = configuration;
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
        JsonNode nodeorder = request().body().asJson();
        Order order;
        if (nodeorder != null) {
            order = mapper.readValue(nodeorder.toString(), Order.class);
            if (configuration.getBoolean("useInputBeanValidation")) {
                OpenAPIUtils.validate(order);
            }
        } else {
            throw new IllegalArgumentException("'Order' parameter is required");
        }
        return ok();
    }
}
