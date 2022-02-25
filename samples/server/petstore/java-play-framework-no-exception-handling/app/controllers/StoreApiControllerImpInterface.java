package controllers;

import java.util.Map;
import apimodels.Order;

import com.google.inject.Inject;
import com.typesafe.config.Config;
import play.mvc.Controller;
import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;
import play.mvc.Result;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.JsonNode;
import openapitools.OpenAPIUtils;
import openapitools.SecurityAPIUtils;
import static play.mvc.Results.ok;
import static play.mvc.Results.unauthorized;
import play.libs.Files.TemporaryFile;

import javax.validation.constraints.*;

@SuppressWarnings("RedundantThrows")
public abstract class StoreApiControllerImpInterface {
    @Inject private Config configuration;
    @Inject private SecurityAPIUtils securityAPIUtils;
    private ObjectMapper mapper = new ObjectMapper();

    public Result deleteOrderHttp(Http.Request request, String orderId)  {
        deleteOrder(request, orderId);
        return ok();

    }

    public abstract void deleteOrder(Http.Request request, String orderId) ;

    public Result getInventoryHttp(Http.Request request)  {
        Map<String, Integer> obj = getInventory(request);
        JsonNode result = mapper.valueToTree(obj);

        return ok(result);

    }

    public abstract Map<String, Integer> getInventory(Http.Request request) ;

    public Result getOrderByIdHttp(Http.Request request,  @Min(1) @Max(5)Long orderId)  {
        Order obj = getOrderById(request, orderId);

        if (configuration.getBoolean("useOutputBeanValidation")) {
            OpenAPIUtils.validate(obj);
        }

        JsonNode result = mapper.valueToTree(obj);

        return ok(result);

    }

    public abstract Order getOrderById(Http.Request request,  @Min(1) @Max(5)Long orderId) ;

    public Result placeOrderHttp(Http.Request request, Order body)  {
        Order obj = placeOrder(request, body);

        if (configuration.getBoolean("useOutputBeanValidation")) {
            OpenAPIUtils.validate(obj);
        }

        JsonNode result = mapper.valueToTree(obj);

        return ok(result);

    }

    public abstract Order placeOrder(Http.Request request, Order body) ;

}
