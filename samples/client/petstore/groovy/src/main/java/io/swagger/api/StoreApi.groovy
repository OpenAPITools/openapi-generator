package io.swagger.api;





import groovyx.net.http.*
import static groovyx.net.http.ContentType.*
import static groovyx.net.http.Method.*
import io.swagger.api.ApiUtils
//-------------

import java.util.Map
import io.swagger.model.Order

import java.util.*;

@Mixin(ApiUtils)
class StoreApi {
    String basePath = "http://petstore.swagger.io/v2"
    String versionPath = "/api/v1"


  def deleteOrder ( String orderId, Closure onSuccess, Closure onFailure)  {
    // create path and map variables
    String resourcePath = "/order/{orderId}"


    // query params
    def queryParams = [:]
    def headerParams = [:]

    // verify required params are set
    if() {
       throw new RuntimeException("missing required params")
    }

    
    
    invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams,
                    "DELETE", "",
                    null )

  }
  def getInventory ( Closure onSuccess, Closure onFailure)  {
    // create path and map variables
    String resourcePath = "/inventory"


    // query params
    def queryParams = [:]
    def headerParams = [:]


    
    
    invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams,
                    "GET", "Map",
                    Map.class )

  }
  def getOrderById ( Long orderId, Closure onSuccess, Closure onFailure)  {
    // create path and map variables
    String resourcePath = "/order/{orderId}"


    // query params
    def queryParams = [:]
    def headerParams = [:]

    // verify required params are set
    if() {
       throw new RuntimeException("missing required params")
    }

    
    
    invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams,
                    "GET", "",
                    Order.class )

  }
  def placeOrder ( Order body, Closure onSuccess, Closure onFailure)  {
    // create path and map variables
    String resourcePath = "/order"


    // query params
    def queryParams = [:]
    def headerParams = [:]

    // verify required params are set
    if() {
       throw new RuntimeException("missing required params")
    }

    
    
    invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams,
                    "POST", "",
                    Order.class )

  }
}
