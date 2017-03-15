package controllers;

import java.util.Map;
import apimodels.Order;

import io.swagger.annotations.*;
import play.mvc.Controller;
import play.mvc.Result;
import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.JsonNode;
import com.google.inject.Inject;
import java.io.IOException;
import swagger.SwaggerUtils;
import javafx.util.Pair;
import com.fasterxml.jackson.core.type.TypeReference;

import javax.validation.constraints.*;


@Api(value = "Store", description = "the Store API")
public class StoreApiController extends Controller {

    private StoreApiControllerImp imp;
    private ObjectMapper mapper;

    @Inject
    private StoreApiController(StoreApiControllerImp imp) {
        this.imp = imp;
        mapper = new ObjectMapper();
    }


    @ApiOperation(value = "Delete purchase order by ID", notes = "For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors", tags={  })
    @ApiResponses(value = { 
    @ApiResponse(code = 400, message = "Invalid ID supplied"), 
    @ApiResponse(code = 404, message = "Order not found") })
    @ApiImplicitParams({
        
    })
    public Result deleteOrder(@ApiParam(value = "ID of the order that needs to be deleted", required = true ) String orderId)  {
        imp.deleteOrder(orderId);
        
        return ok();
    }

    @ApiOperation(value = "Returns pet inventories by status", notes = "Returns a map of status codes to quantities", response = Integer.class, responseContainer = "Map", authorizations = {
    @Authorization(value = "api_key")
    }, tags={  })
    @ApiResponses(value = { 
    @ApiResponse(code = 200, message = "successful operation", response = Integer.class) })
    public Result getInventory()  {
        Map<String, Integer> obj = imp.getInventory();
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
        
    }

    @ApiOperation(value = "Find purchase order by ID", notes = "For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions", response = Order.class, tags={  })
    @ApiResponses(value = { 
    @ApiResponse(code = 200, message = "successful operation", response = Order.class), 
    @ApiResponse(code = 400, message = "Invalid ID supplied", response = Order.class), 
    @ApiResponse(code = 404, message = "Order not found", response = Order.class) })
    @ApiImplicitParams({
        
    })
    public Result getOrderById(@ApiParam(value = "ID of pet that needs to be fetched", required = true ) String orderId)  {
        Order obj = imp.getOrderById(orderId);
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
        
    }

    @ApiOperation(value = "Place an order for a pet", notes = "", response = Order.class, tags={  })
    @ApiResponses(value = { 
    @ApiResponse(code = 200, message = "successful operation", response = Order.class), 
    @ApiResponse(code = 400, message = "Invalid Order", response = Order.class) })
    @ApiImplicitParams({
        @ApiImplicitParam(name = "body", value = "order placed for purchasing the pet", dataType = "apimodels.Order", paramType = "body")
    })
    public Result placeOrder() throws IOException {
        JsonNode nodebody = request().body().asJson();
        Order body;
        if (nodebody != null) {
            body = mapper.readValue(nodebody.toString(), Order.class);
        
        } else {
            body = null;
        }
        Order obj = imp.placeOrder(body);
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
        
    }
}
