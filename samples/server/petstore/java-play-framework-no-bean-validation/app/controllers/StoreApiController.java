package controllers;

import apimodels.Order;

import play.mvc.Controller;
import play.mvc.Result;
import play.mvc.Http;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.JsonNode;
import com.google.inject.Inject;

import openapitools.OpenAPIUtils.ApiAction;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaPlayFrameworkCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
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
        return imp.deleteOrderHttp(request, orderId);
    }

    @ApiAction
    public Result getInventory(Http.Request request) throws Exception {
        return imp.getInventoryHttp(request);
    }

    @ApiAction
    public Result getOrderById(Http.Request request, Long orderId) throws Exception {
        return imp.getOrderByIdHttp(request, orderId);
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
        return imp.placeOrderHttp(request, body);
    }

}
