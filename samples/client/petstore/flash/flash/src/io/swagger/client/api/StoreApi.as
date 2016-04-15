package io.swagger.client.api {

import io.swagger.common.ApiInvoker;
import io.swagger.exception.ApiErrorCodes;
import io.swagger.exception.ApiError;
import io.swagger.common.ApiUserCredentials;
import io.swagger.event.Response;
import io.swagger.common.SwaggerApi;
import io.swagger.client.model.Order;
import io.swagger.client.model.Object;

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

        public static const event_delete_order: String = "delete_order";
        public static const event_find_orders_by_status: String = "find_orders_by_status";
        public static const event_get_inventory: String = "get_inventory";
        public static const event_get_inventory_in_object: String = "get_inventory_in_object";
        public static const event_get_order_by_id: String = "get_order_by_id";
        public static const event_place_order: String = "place_order";


    /*
     * Returns void 
     */
    public function delete_order (orderId: String): String {
        // create path and map variables
        var path: String = "/store/order/{orderId}".replace(/{format}/g,"xml").replace("{" + "orderId" + "}", getApiInvoker().escapeString(orderId));

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if() {
            throw new ApiError(400, "missing required params");
        }

        
        
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "DELETE", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "delete_order";

        token.returnType = null ;
        return requestId;

    }

    /*
     * Returns Array 
     */
    public function find_orders_by_status (status: String): String {
        // create path and map variables
        var path: String = "/store/findByStatus".replace(/{format}/g,"xml");

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if() {
            throw new ApiError(400, "missing required params");
        }

        if("null" != String(status))
            queryParams["status"] = toPathValue(status);

        
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "GET", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "find_orders_by_status";

        token.returnType = Array;
        return requestId;

    }

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
     * Returns Object 
     */
    public function get_inventory_in_object (): String {
        // create path and map variables
        var path: String = "/store/inventory?response&#x3D;arbitrary_object".replace(/{format}/g,"xml");

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();


        
        
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "GET", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "get_inventory_in_object";

        token.returnType = Object;
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

        // verify required params are set
        if() {
            throw new ApiError(400, "missing required params");
        }

        
        
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "GET", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "get_order_by_id";

        token.returnType = Order;
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

        // verify required params are set
        if() {
            throw new ApiError(400, "missing required params");
        }

        
        
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "POST", queryParams, body, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "place_order";

        token.returnType = Order;
        return requestId;

    }
}
}
