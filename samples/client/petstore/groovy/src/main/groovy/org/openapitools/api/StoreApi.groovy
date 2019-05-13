package org.openapitools.api;

import org.openapitools.api.ApiUtils
import org.openapitools.model.Order

class StoreApi {
    String basePath = "http://petstore.swagger.io/v2"
    String versionPath = ""
    ApiUtils apiUtils = new ApiUtils();

    def deleteOrder ( String orderId, Closure onSuccess, Closure onFailure)  {
        String resourcePath = "/store/order/${orderId}"

        // params
        def queryParams = [:]
        def headerParams = [:]
        def bodyParams
        def contentType

        
        // verify required params are set
        if (orderId == null) {
            throw new RuntimeException("missing required params orderId")
        }
        





        apiUtils.invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams, bodyParams, contentType,
                    "DELETE", "",
                    null )

    }

    def getInventory ( Closure onSuccess, Closure onFailure)  {
        String resourcePath = "/store/inventory"

        // params
        def queryParams = [:]
        def headerParams = [:]
        def bodyParams
        def contentType

        





        apiUtils.invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams, bodyParams, contentType,
                    "GET", "map",
                    Integer.class )

    }

    def getOrderById ( Long orderId, Closure onSuccess, Closure onFailure)  {
        String resourcePath = "/store/order/${orderId}"

        // params
        def queryParams = [:]
        def headerParams = [:]
        def bodyParams
        def contentType

        
        // verify required params are set
        if (orderId == null) {
            throw new RuntimeException("missing required params orderId")
        }
        





        apiUtils.invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams, bodyParams, contentType,
                    "GET", "",
                    Order.class )

    }

    def placeOrder ( Order body, Closure onSuccess, Closure onFailure)  {
        String resourcePath = "/store/order"

        // params
        def queryParams = [:]
        def headerParams = [:]
        def bodyParams
        def contentType

        
        // verify required params are set
        if (body == null) {
            throw new RuntimeException("missing required params body")
        }
        



        contentType = 'application/json';
        // only one body parameter
        if (1 == 1) {
            bodyParams = body
        }
        // array of body parameters
        else {
            bodyParams.put("body", body)
        }


        apiUtils.invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams, bodyParams, contentType,
                    "POST", "",
                    Order.class )

    }

}
