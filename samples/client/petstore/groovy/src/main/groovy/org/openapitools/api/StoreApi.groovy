package org.openapitools.api;

import org.openapitools.api.ApiUtils
import org.openapitools.model.Order

class StoreApi {
    String basePath = "http://localhost/v2"
    String versionPath = ""
    ApiUtils apiUtils = new ApiUtils();

    def deleteOrder ( String orderId, Closure onSuccess, Closure onFailure)  {
        String resourcePath = "/store/order/${orderId}"

        // params
        def queryParams = [:]
        def headerParams = [:]
        def bodyParams
        def accept
        def contentType

        // verify required params are set
        if (orderId == null) {
            throw new RuntimeException("missing required params orderId")
        }





        accept = apiUtils.selectHeaderAccept([])

        apiUtils.invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams, bodyParams, accept, contentType,
                    "DELETE", "",
                    null )

    }

    def getInventory ( Closure onSuccess, Closure onFailure)  {
        String resourcePath = "/store/inventory"

        // params
        def queryParams = [:]
        def headerParams = [:]
        def bodyParams
        def accept
        def contentType






        accept = apiUtils.selectHeaderAccept(["application/json"])

        apiUtils.invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams, bodyParams, accept, contentType,
                    "GET", "map",
                    Integer.class )

    }

    def getOrderById ( Long orderId, Closure onSuccess, Closure onFailure)  {
        String resourcePath = "/store/order/${orderId}"

        // params
        def queryParams = [:]
        def headerParams = [:]
        def bodyParams
        def accept
        def contentType

        // verify required params are set
        if (orderId == null) {
            throw new RuntimeException("missing required params orderId")
        }





        accept = apiUtils.selectHeaderAccept(["application/xml", "application/json"])

        apiUtils.invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams, bodyParams, accept, contentType,
                    "GET", "",
                    Order.class )

    }

    def placeOrder ( Order order, Closure onSuccess, Closure onFailure)  {
        String resourcePath = "/store/order"

        // params
        def queryParams = [:]
        def headerParams = [:]
        def bodyParams
        def accept
        def contentType

        // verify required params are set
        if (order == null) {
            throw new RuntimeException("missing required params order")
        }



        contentType = 'application/json';
        bodyParams = order


        accept = apiUtils.selectHeaderAccept(["application/xml", "application/json"])

        apiUtils.invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams, bodyParams, accept, contentType,
                    "POST", "",
                    Order.class )

    }

}
