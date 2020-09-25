package controllers;

import java.util.Map;
import apimodels.Order;

import com.typesafe.config.Config;
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
import openapitools.OpenAPIUtils;
import com.fasterxml.jackson.core.type.TypeReference;


import openapitools.OpenAPIUtils.ApiAction;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaPlayFrameworkCodegen")
public class StoreApiController extends Controller {

    private final StoreApiControllerImpInterface imp;
    private final ObjectMapper mapper;

    @Inject
    private StoreApiController(StoreApiControllerImpInterface imp) {
        this.imp = imp;
        mapper = new ObjectMapper();
    }


    @ApiAction
    public Result deleteOrder(Http.Request request, String orderId) throws Exception {
        imp.deleteOrder(request, orderId);
        return ok();
    }

    @ApiAction
    public Result getInventory(Http.Request request) throws Exception {
        Map<String, Integer> obj = imp.getInventory(request);
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
    }

    @ApiAction
    public Result getOrderById(Http.Request request, Long orderId) throws Exception {
        Order obj = imp.getOrderById(request, orderId);
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
    }

    @ApiAction
    public Result placeOrder(Http.Request request) throws Exception {
        JsonNode nodebody = request.body().asJson();
        Order body;
        if (nodebody != null) {
            body = mapper.readValue(nodebody.toString(), Order.class);
        } else {
            throw new IllegalArgumentException("'body' parameter is required");
        }
        Order obj = imp.placeOrder(request, body);
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
    }
}
