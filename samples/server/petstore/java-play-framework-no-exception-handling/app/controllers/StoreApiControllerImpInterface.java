package controllers;

import java.util.Map;
import apimodels.Order;

import com.google.inject.Inject;
import com.google.inject.Singleton;
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
import static play.mvc.Results.ok;

import javax.validation.constraints.*;

@Singleton
@SuppressWarnings("RedundantThrows")
public abstract class StoreApiControllerImpInterface {
    @Inject private Config configuration;
    private ObjectMapper mapper = new ObjectMapper();

    Result deleteOrderHttp(Http.Request request, String orderId)  {
        deleteOrder(request, orderId);
return ok();

    }

    abstract void deleteOrder(Http.Request request, String orderId) ;

    Result getInventoryHttp(Http.Request request)  {
        Map<String, Integer> obj = getInventory(request);
JsonNode result = mapper.valueToTree(obj);
return ok(result);

    }

    abstract Map<String, Integer> getInventory(Http.Request request) ;

    Result getOrderByIdHttp(Http.Request request,  @Min(1) @Max(5)Long orderId)  {
        Order obj = getOrderById(request, orderId);
    if (configuration.getBoolean("useOutputBeanValidation")) {
            OpenAPIUtils.validate(obj);
    }
JsonNode result = mapper.valueToTree(obj);
return ok(result);

    }

    abstract Order getOrderById(Http.Request request,  @Min(1) @Max(5)Long orderId) ;

    Result placeOrderHttp(Http.Request request, Order body)  {
        Order obj = placeOrder(request, body);
    if (configuration.getBoolean("useOutputBeanValidation")) {
            OpenAPIUtils.validate(obj);
    }
JsonNode result = mapper.valueToTree(obj);
return ok(result);

    }

    abstract Order placeOrder(Http.Request request, Order body) ;

}
