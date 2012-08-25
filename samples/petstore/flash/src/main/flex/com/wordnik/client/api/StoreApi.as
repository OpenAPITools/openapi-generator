package com.wordnik.client.api {

import com.wordnik.swagger.common.ApiInvoker;
import com.wordnik.swagger.exception.ApiErrorCodes;
import com.wordnik.swagger.exception.ApiError;
import com.wordnik.swagger.common.ApiUserCredentials;
import com.wordnik.swagger.event.Response;
import com.wordnik.swagger.common.SwaggerApi;
import com.wordnik.client.model.Order;
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

public static const event_getOrderById: String = "getOrderById";
public static const event_deleteOrder: String = "deleteOrder";
public static const event_placeOrder: String = "placeOrder";
/*
     * Returns Order */
    public function getOrderById (orderId: String): String {
        // create path and map variables
        var path: String = "/store.{format}/order/{orderId}".replace(/{format}/g,"xml").replace("{" + "orderId" + "}", getApiInvoker().escapeString(orderId));

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if(orderId == null ) {
            throw new ApiError(400, "missing required params");
        }
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "GET", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "getOrderById";

        token.returnType = Order;
        return requestId;

    }
    /*
     * Returns void */
    public function deleteOrder (orderId: String): String {
        // create path and map variables
        var path: String = "/store.{format}/order/{orderId}".replace(/{format}/g,"xml").replace("{" + "orderId" + "}", getApiInvoker().escapeString(orderId));

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if(orderId == null ) {
            throw new ApiError(400, "missing required params");
        }
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "DELETE", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "deleteOrder";

        token.returnType = null ;
        return requestId;

    }
    /*
     * Returns void */
    public function placeOrder (body: Order): String {
        // create path and map variables
        var path: String = "/store.{format}/order".replace(/{format}/g,"xml");

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if(body == null ) {
            throw new ApiError(400, "missing required params");
        }
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "POST", queryParams, body, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "placeOrder";

        token.returnType = null ;
        return requestId;

    }
    }
        }
