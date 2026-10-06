package controllers;

import apimodels.Order;

import com.typesafe.config.Config;
import play.mvc.Controller;
import play.mvc.Result;
import play.mvc.Http;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.JsonNode;
import com.google.inject.Inject;
import openapitools.OpenAPIUtils;

import javax.validation.constraints.*;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaPlayFrameworkCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
public class StoreApiController extends Controller {
    private final StoreApiControllerImpInterface imp;
    private final ObjectMapper mapper;
    private final Config configuration;

    @Inject
    private StoreApiController(Config configuration, StoreApiControllerImpInterface imp) {
        this.imp = imp;
        mapper = new ObjectMapper();
        this.configuration = configuration;
    }

    
    public Result deleteOrder(Http.Request request, String orderId) throws Exception {
        return imp.deleteOrderHttp(request, orderId);
    }

    
    public Result getInventory(Http.Request request) throws Exception {
        return imp.getInventoryHttp(request);
    }

    
    public Result getOrderById(Http.Request request,  @Min(1) @Max(5)Long orderId) throws Exception {
        return imp.getOrderByIdHttp(request, orderId);
    }

    
    public Result placeOrder(Http.Request request) throws Exception {
        JsonNode nodebody = request.body().asJson();
        Order body;
        if (nodebody != null) {
            body = mapper.readValue(nodebody.toString(), Order.class);
            if (configuration.getBoolean("useInputBeanValidation")) {
                OpenAPIUtils.validate(body);
            }
        } else {
            throw new IllegalArgumentException("'body' parameter is required");
        }
        return imp.placeOrderHttp(request, body);
    }

}
