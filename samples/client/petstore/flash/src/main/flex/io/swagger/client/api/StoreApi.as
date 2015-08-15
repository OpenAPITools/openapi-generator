package io.swagger.client.api {

import io.swagger.common.ApiInvoker;
import io.swagger.exception.ApiErrorCodes;
import io.swagger.exception.ApiError;
import io.swagger.common.ApiUserCredentials;
import io.swagger.event.Response;
import io.swagger.common.SwaggerApi;
import io.swagger.client.model.Order;

import mx.rpc.AsyncToken;
import mx.utils.UIDUtil;
import flash.utils.Dictionary;
import flash.events.EventDispatcher;

public class StoreApi extends SwaggerApi {
    /**
    * Constructor for the StoreApi api client
    * @param apiCredentials Wrapper object for tokens and hostName required towards authentication
    * @param eventDispatcher Optional event dispatcher that when provided is used by the SDK to dispatch any Response
    */
    public function StoreApi(apiCredentials: ApiUserCredentials, eventDispatcher: EventDispatcher = null) {
        super(apiCredentials, eventDispatcher);
    }

        public static const event_get_inventory: String = "get_inventory";
        public static const event_place_order: String = "place_order";
        public static const event_get_order_by_id: String = "get_order_by_id";
        public static const event_delete_order: String = "delete_order";


    /*
     * Returns Dictionary 
     */
    public function get_inventory (): String {
        // create path and map variables
        var path: String = "/store/inventory".replace(/{format}/g,"xml");

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        

        

        

        var token:AsyncToken = getApiInvoker().invokeAPI(path, "GET", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "get_inventory";

        token.returnType = Dictionary;
        return requestId;

    }
    
    /*
     * Returns Order 
     */
    public function place_order (body: Order): String {
        // create path and map variables
        var path: String = "/store/order".replace(/{format}/g,"xml");

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        

        

        

        var token:AsyncToken = getApiInvoker().invokeAPI(path, "POST", queryParams, body, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "place_order";

        token.returnType = Order;
        return requestId;

    }
    
    /*
     * Returns Order 
     */
    public function get_order_by_id (orderId: String): String {
        // create path and map variables
        var path: String = "/store/order/{orderId}".replace(/{format}/g,"xml").replace("{" + "orderId" + "}", getApiInvoker().escapeString(orderId));

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        

        

        

        var token:AsyncToken = getApiInvoker().invokeAPI(path, "GET", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "get_order_by_id";

        token.returnType = Order;
        return requestId;

    }
    
    /*
     * Returns void 
     */
    public function delete_order (orderId: String): String {
        // create path and map variables
        var path: String = "/store/order/{orderId}".replace(/{format}/g,"xml").replace("{" + "orderId" + "}", getApiInvoker().escapeString(orderId));

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        

        

        

        var token:AsyncToken = getApiInvoker().invokeAPI(path, "DELETE", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "delete_order";

        token.returnType = null ;
        return requestId;

    }
    
}
        
}
