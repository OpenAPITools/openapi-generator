package controllers;

import java.util.Map;
import apimodels.Order;

import play.mvc.Controller;
import play.mvc.Result;
import play.mvc.Http;
import java.util.List;
import java.util.Map;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.JsonNode;
import com.google.inject.Inject;
import java.io.File;
import java.io.IOException;
import openapitools.OpenAPIUtils;
import com.fasterxml.jackson.core.type.TypeReference;

import javax.validation.constraints.*;
import play.Configuration;

import openapitools.OpenAPIUtils.ApiAction;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaPlayFrameworkCodegen")
public class StoreApiController extends Controller {

    private final StoreApiControllerImpInterface imp;
    private final ObjectMapper mapper;
    private final Configuration configuration;

    @Inject
    private StoreApiController(Configuration configuration, StoreApiControllerImpInterface imp) {
        this.imp = imp;
        mapper = new ObjectMapper();
        this.configuration = configuration;
    }


    @ApiAction
    public Result deleteOrder(String orderId)  {
        imp.deleteOrder(orderId);
        return ok();
    }

    @ApiAction
    public Result getInventory()  {
        Map<String, Integer> obj = imp.getInventory();
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
    }

    @ApiAction
    public Result getOrderById( @Min(1) @Max(5)Long orderId)  {
        Order obj = imp.getOrderById(orderId);
        if (configuration.getBoolean("useOutputBeanValidation")) {
            OpenAPIUtils.validate(obj);
        }
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
    }

    @ApiAction
    public Result placeOrder() throws IOException {
        JsonNode nodebody = request().body().asJson();
        Order body;
        if (nodebody != null) {
            body = mapper.readValue(nodebody.toString(), Order.class);
            if (configuration.getBoolean("useInputBeanValidation")) {
                OpenAPIUtils.validate(body);
            }
        } else {
            throw new IllegalArgumentException("'body' parameter is required");
        }
        Order obj = imp.placeOrder(body);
        if (configuration.getBoolean("useOutputBeanValidation")) {
            OpenAPIUtils.validate(obj);
        }
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
    }
}
