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


@SuppressWarnings("RedundantThrows")
public abstract class StoreApiControllerImpInterface {
    private ObjectMapper mapper = new ObjectMapper();

    Result deleteOrderHttp(Http.Request request, String orderId) throws Exception {
        deleteOrder(request, orderId);
return ok();

    }

    abstract void deleteOrder(Http.Request request, String orderId) throws Exception;

    Result getInventoryHttp(Http.Request request) throws Exception {
        Map<String, Integer> obj = getInventory(request);
JsonNode result = mapper.valueToTree(obj);
return ok(result);

    }

    abstract Map<String, Integer> getInventory(Http.Request request) throws Exception;

    Result getOrderByIdHttp(Http.Request request, Long orderId) throws Exception {
        Order obj = getOrderById(request, orderId);
JsonNode result = mapper.valueToTree(obj);
return ok(result);

    }

    abstract Order getOrderById(Http.Request request, Long orderId) throws Exception;

    Result placeOrderHttp(Http.Request request, Order body) throws Exception {
        Order obj = placeOrder(request, body);
JsonNode result = mapper.valueToTree(obj);
return ok(result);

    }

    abstract Order placeOrder(Http.Request request, Order body) throws Exception;

}
