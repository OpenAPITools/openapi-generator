package org.openapitools.client.api {

import org.openapitools.common.ApiInvoker;
import org.openapitools.exception.ApiErrorCodes;
import org.openapitools.exception.ApiError;
import org.openapitools.common.ApiUserCredentials;
import org.openapitools.event.Response;
import org.openapitools.common.OpenApi;
import org.openapitools.client.model.Order;

import mx.rpc.AsyncToken;
import mx.utils.UIDUtil;
import flash.utils.Dictionary;
import flash.events.EventDispatcher;

public class StoreApi extends OpenApi {
    /**
    * Constructor for the StoreApi api client
    * @param apiCredentials Wrapper object for tokens and hostName required towards authentication
    * @param eventDispatcher Optional event dispatcher that when provided is used by the SDK to dispatch any Response
    */
    public function StoreApi(apiCredentials: ApiUserCredentials, eventDispatcher: EventDispatcher = null) {
        super(apiCredentials, eventDispatcher);
    }

        public static const event_delete_order: String = "delete_order";
        public static const event_get_inventory: String = "get_inventory";
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
    public function get_order_by_id (orderId: Number): String {
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
    public function place_order (order: Order): String {
        // create path and map variables
        var path: String = "/store/order".replace(/{format}/g,"xml");

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if() {
            throw new ApiError(400, "missing required params");
        }

        
        
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "POST", queryParams, order, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "place_order";

        token.returnType = Order;
        return requestId;

    }
}
}
