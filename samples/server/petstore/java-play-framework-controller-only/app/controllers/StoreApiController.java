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

import javax.validation.constraints.*;
import com.typesafe.config.Config;

import openapitools.OpenAPIUtils.ApiAction;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaPlayFrameworkCodegen")
public class StoreApiController extends Controller {

    private final ObjectMapper mapper;
    private final Config configuration;

    @Inject
    private StoreApiController(Config configuration) {
        mapper = new ObjectMapper();
        this.configuration = configuration;
    }


    @ApiAction
    public Result deleteOrder(Http.Request request, String orderId) throws Exception {
        return ok();
    }

    @ApiAction
    public Result getInventory(Http.Request request) throws Exception {
        return ok();
    }

    @ApiAction
    public Result getOrderById(Http.Request request,  @Min(1) @Max(5)Long orderId) throws Exception {
        return ok();
    }

    @ApiAction
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
        return ok();
    }
}
