package org.openapitools.api;

import groovyx.net.http.*
import static groovyx.net.http.ContentType.*
import static groovyx.net.http.Method.*
import org.openapitools.api.ApiUtils

import org.openapitools.model.User

import java.util.*;

@Mixin(ApiUtils)
class UserApi {
    String basePath = "http://petstore.swagger.io/v2"
    String versionPath = "/api/v1"

    def createUser ( User body, Closure onSuccess, Closure onFailure)  {
        // create path and map path parameters (TODO)
        String resourcePath = "/user"

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
                    null )

    }

    def createUsersWithArrayInput ( List<User> body, Closure onSuccess, Closure onFailure)  {
        // create path and map path parameters (TODO)
        String resourcePath = "/user/createWithArray"

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
                    null )

    }

    def createUsersWithListInput ( List<User> body, Closure onSuccess, Closure onFailure)  {
        // create path and map path parameters (TODO)
        String resourcePath = "/user/createWithList"

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
                    null )

    }

    def deleteUser ( String username, Closure onSuccess, Closure onFailure)  {
        // create path and map path parameters (TODO)
        String resourcePath = "/user/{username}"

        // query params
        def queryParams = [:]
        def headerParams = [:]

        // verify required params are set
        if (username == null) {
            throw new RuntimeException("missing required params username")
        }

        // TODO: form params, body param not yet support

        invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams,
                    "DELETE", "",
                    null )

    }

    def getUserByName ( String username, Closure onSuccess, Closure onFailure)  {
        // create path and map path parameters (TODO)
        String resourcePath = "/user/{username}"

        // query params
        def queryParams = [:]
        def headerParams = [:]

        // verify required params are set
        if (username == null) {
            throw new RuntimeException("missing required params username")
        }

        // TODO: form params, body param not yet support

        invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams,
                    "GET", "",
                    User.class )

    }

    def loginUser ( String username, String password, Closure onSuccess, Closure onFailure)  {
        // create path and map path parameters (TODO)
        String resourcePath = "/user/login"

        // query params
        def queryParams = [:]
        def headerParams = [:]

        // verify required params are set
        if (username == null) {
            throw new RuntimeException("missing required params username")
        }

        // verify required params are set
        if (password == null) {
            throw new RuntimeException("missing required params password")
        }

        if (!"null".equals(String.valueOf(username)))
            queryParams.put("username", String.valueOf(username))

        if (!"null".equals(String.valueOf(password)))
            queryParams.put("password", String.valueOf(password))

        // TODO: form params, body param not yet support

        invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams,
                    "GET", "",
                    String.class )

    }

    def logoutUser ( Closure onSuccess, Closure onFailure)  {
        // create path and map path parameters (TODO)
        String resourcePath = "/user/logout"

        // query params
        def queryParams = [:]
        def headerParams = [:]

        // TODO: form params, body param not yet support

        invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams,
                    "GET", "",
                    null )

    }

    def updateUser ( String username, User body, Closure onSuccess, Closure onFailure)  {
        // create path and map path parameters (TODO)
        String resourcePath = "/user/{username}"

        // query params
        def queryParams = [:]
        def headerParams = [:]

        // verify required params are set
        if (username == null) {
            throw new RuntimeException("missing required params username")
        }

        // verify required params are set
        if (body == null) {
            throw new RuntimeException("missing required params body")
        }

        // TODO: form params, body param not yet support

        invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams,
                    "PUT", "",
                    null )

    }

}
