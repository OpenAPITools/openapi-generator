package io.swagger.api;





import groovyx.net.http.*
import static groovyx.net.http.ContentType.*
import static groovyx.net.http.Method.*
import io.swagger.api.ApiUtils
//-------------

import io.swagger.model.User
import java.util.List

import java.util.*;

@Mixin(ApiUtils)
class UserApi {
    String basePath = "http://petstore.swagger.io/v2"
    String versionPath = "/api/v1"


  def createUser ( User body, Closure onSuccess, Closure onFailure)  {
    // create path and map variables
    String resourcePath = ""


    // query params
    def queryParams = [:]
    def headerParams = [:]

    // verify required params are set
    if() {
       throw new RuntimeException("missing required params")
    }

    
    
    invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams,
                    "POST", "",
                    null )

  }
  def createUsersWithArrayInput ( List<User> body, Closure onSuccess, Closure onFailure)  {
    // create path and map variables
    String resourcePath = "/createWithArray"


    // query params
    def queryParams = [:]
    def headerParams = [:]

    // verify required params are set
    if() {
       throw new RuntimeException("missing required params")
    }

    
    
    invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams,
                    "POST", "",
                    null )

  }
  def createUsersWithListInput ( List<User> body, Closure onSuccess, Closure onFailure)  {
    // create path and map variables
    String resourcePath = "/createWithList"


    // query params
    def queryParams = [:]
    def headerParams = [:]

    // verify required params are set
    if() {
       throw new RuntimeException("missing required params")
    }

    
    
    invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams,
                    "POST", "",
                    null )

  }
  def deleteUser ( String username, Closure onSuccess, Closure onFailure)  {
    // create path and map variables
    String resourcePath = "/{username}"


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
  def getUserByName ( String username, Closure onSuccess, Closure onFailure)  {
    // create path and map variables
    String resourcePath = "/{username}"


    // query params
    def queryParams = [:]
    def headerParams = [:]

    // verify required params are set
    if() {
       throw new RuntimeException("missing required params")
    }

    
    
    invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams,
                    "GET", "",
                    User.class )

  }
  def loginUser ( String username, String password, Closure onSuccess, Closure onFailure)  {
    // create path and map variables
    String resourcePath = "/login"


    // query params
    def queryParams = [:]
    def headerParams = [:]

    // verify required params are set
    if(    // verify required params are set
    if() {
       throw new RuntimeException("missing required params")
    }
) {
       throw new RuntimeException("missing required params")
    }

    if(!"null".equals(String.valueOf(username)))
      queryParams.put("username", String.valueOf(username))
if(!"null".equals(String.valueOf(password)))
      queryParams.put("password", String.valueOf(password))

    
    invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams,
                    "GET", "",
                    String.class )

  }
  def logoutUser ( Closure onSuccess, Closure onFailure)  {
    // create path and map variables
    String resourcePath = "/logout"


    // query params
    def queryParams = [:]
    def headerParams = [:]


    
    
    invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams,
                    "GET", "",
                    null )

  }
  def updateUser ( String username, User body, Closure onSuccess, Closure onFailure)  {
    // create path and map variables
    String resourcePath = "/{username}"


    // query params
    def queryParams = [:]
    def headerParams = [:]

    // verify required params are set
    if(    // verify required params are set
    if() {
       throw new RuntimeException("missing required params")
    }
) {
       throw new RuntimeException("missing required params")
    }

    
    
    invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams,
                    "PUT", "",
                    null )

  }
}
