package org.openapitools.api;

import groovyx.net.http.*
import static groovyx.net.http.ContentType.*
import static groovyx.net.http.Method.*
import org.openapitools.api.ApiUtils

import org.openapitools.model.Order

import java.util.*;

@Mixin(ApiUtils)
class StoreApi {
    String basePath = "http://petstore.swagger.io/v2"
    String versionPath = "/api/v1"

    def deleteOrder ( String orderId, Closure onSuccess, Closure onFailure)  {
        // create path and map path parameters (TODO)
        String resourcePath = "/store/order/{orderId}"

        // query params
        def queryParams = [:]
        def headerParams = [:]

        // verify required params are set
        if (orderId == null) {
            throw new RuntimeException("missing required params orderId")
        }

        // TODO: form params, body param not yet support

        invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams,
                    "DELETE", "",
                    null )

    }

    def getInventory ( Closure onSuccess, Closure onFailure)  {
        // create path and map path parameters (TODO)
        String resourcePath = "/store/inventory"

        // query params
        def queryParams = [:]
        def headerParams = [:]

        // TODO: form params, body param not yet support

        invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams,
                    "GET", "map",
                    Integer.class )

    }

    def getOrderById ( Long orderId, Closure onSuccess, Closure onFailure)  {
        // create path and map path parameters (TODO)
        String resourcePath = "/store/order/{orderId}"

        // query params
        def queryParams = [:]
        def headerParams = [:]

        // verify required params are set
        if (orderId == null) {
            throw new RuntimeException("missing required params orderId")
        }

        // TODO: form params, body param not yet support

        invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams,
                    "GET", "",
                    Order.class )

    }

    def placeOrder ( Order body, Closure onSuccess, Closure onFailure)  {
        // create path and map path parameters (TODO)
        String resourcePath = "/store/order"

        // query params
        def queryParams = [:]
        def headerParams = [:]

        // verify required params are set
        if (body == null) {
            throw new RuntimeException("missing required params body")
        }

        // TODO: form params, body param not yet support

        invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams,
                    "POST", "",
                    Order.class )

    }

}
