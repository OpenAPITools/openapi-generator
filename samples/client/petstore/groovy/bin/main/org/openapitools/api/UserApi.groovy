package org.openapitools.api;

import org.openapitools.api.ApiUtils
import java.util.List
import org.openapitools.model.User

class UserApi {
    String basePath = "http://petstore.swagger.io/v2"
    String versionPath = ""
    ApiUtils apiUtils = new ApiUtils();

    def createUser ( User user, Closure onSuccess, Closure onFailure)  {
        String resourcePath = "/user"

        // params
        def queryParams = [:]
        def headerParams = [:]
        def bodyParams
        def contentType

        // verify required params are set
        if (user == null) {
            throw new RuntimeException("missing required params user")
        }



        contentType = 'application/json';
        bodyParams = user


        apiUtils.invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams, bodyParams, contentType,
                    "POST", "",
                    null )

    }

    def createUsersWithArrayInput ( List<User> user, Closure onSuccess, Closure onFailure)  {
        String resourcePath = "/user/createWithArray"

        // params
        def queryParams = [:]
        def headerParams = [:]
        def bodyParams
        def contentType

        // verify required params are set
        if (user == null) {
            throw new RuntimeException("missing required params user")
        }



        contentType = 'application/json';
        bodyParams = user


        apiUtils.invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams, bodyParams, contentType,
                    "POST", "",
                    null )

    }

    def createUsersWithListInput ( List<User> user, Closure onSuccess, Closure onFailure)  {
        String resourcePath = "/user/createWithList"

        // params
        def queryParams = [:]
        def headerParams = [:]
        def bodyParams
        def contentType

        // verify required params are set
        if (user == null) {
            throw new RuntimeException("missing required params user")
        }



        contentType = 'application/json';
        bodyParams = user


        apiUtils.invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams, bodyParams, contentType,
                    "POST", "",
                    null )

    }

    def deleteUser ( String username, Closure onSuccess, Closure onFailure)  {
        String resourcePath = "/user/${username}"

        // params
        def queryParams = [:]
        def headerParams = [:]
        def bodyParams
        def contentType

        // verify required params are set
        if (username == null) {
            throw new RuntimeException("missing required params username")
        }





        apiUtils.invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams, bodyParams, contentType,
                    "DELETE", "",
                    null )

    }

    def getUserByName ( String username, Closure onSuccess, Closure onFailure)  {
        String resourcePath = "/user/${username}"

        // params
        def queryParams = [:]
        def headerParams = [:]
        def bodyParams
        def contentType

        // verify required params are set
        if (username == null) {
            throw new RuntimeException("missing required params username")
        }





        apiUtils.invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams, bodyParams, contentType,
                    "GET", "",
                    User.class )

    }

    def loginUser ( String username, String password, Closure onSuccess, Closure onFailure)  {
        String resourcePath = "/user/login"

        // params
        def queryParams = [:]
        def headerParams = [:]
        def bodyParams
        def contentType

        // verify required params are set
        if (username == null) {
            throw new RuntimeException("missing required params username")
        }
        // verify required params are set
        if (password == null) {
            throw new RuntimeException("missing required params password")
        }

        if (username != null) {
            queryParams.put("username", username)
        }
        if (password != null) {
            queryParams.put("password", password)
        }




        apiUtils.invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams, bodyParams, contentType,
                    "GET", "",
                    String.class )

    }

    def logoutUser ( Closure onSuccess, Closure onFailure)  {
        String resourcePath = "/user/logout"

        // params
        def queryParams = [:]
        def headerParams = [:]
        def bodyParams
        def contentType






        apiUtils.invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams, bodyParams, contentType,
                    "GET", "",
                    null )

    }

    def updateUser ( String username, User user, Closure onSuccess, Closure onFailure)  {
        String resourcePath = "/user/${username}"

        // params
        def queryParams = [:]
        def headerParams = [:]
        def bodyParams
        def contentType

        // verify required params are set
        if (username == null) {
            throw new RuntimeException("missing required params username")
        }
        // verify required params are set
        if (user == null) {
            throw new RuntimeException("missing required params user")
        }



        contentType = 'application/json';
        bodyParams = user


        apiUtils.invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams, bodyParams, contentType,
                    "PUT", "",
                    null )

    }

}
