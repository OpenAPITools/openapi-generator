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
import play.libs.Files.TemporaryFile;
import openapitools.OpenAPIUtils;
import openapitools.SecurityAPIUtils;
import com.fasterxml.jackson.core.type.TypeReference;

import javax.validation.constraints.*;
import com.typesafe.config.Config;

import openapitools.OpenAPIUtils.ApiAction;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaPlayFrameworkCodegen")
public class StoreApiController extends Controller {
    private final StoreApiControllerImp imp;
    private final ObjectMapper mapper;
    private final Config configuration;
    private final SecurityAPIUtils securityAPIUtils;

    @Inject
    private StoreApiController(Config configuration, StoreApiControllerImp imp, SecurityAPIUtils securityAPIUtils) {
        this.imp = imp;
        mapper = new ObjectMapper();
        this.configuration = configuration;
        this.securityAPIUtils = securityAPIUtils;
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
    public Result getOrderById(Http.Request request,  @Min(1) @Max(5)Long orderId) throws Exception {
                Order obj = imp.getOrderById(request, orderId);

        if (configuration.getBoolean("useOutputBeanValidation")) {
            OpenAPIUtils.validate(obj);
        }

        JsonNode result = mapper.valueToTree(obj);

        return ok(result);

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
                Order obj = imp.placeOrder(request, body);

        if (configuration.getBoolean("useOutputBeanValidation")) {
            OpenAPIUtils.validate(obj);
        }

        JsonNode result = mapper.valueToTree(obj);

        return ok(result);

    }

}
