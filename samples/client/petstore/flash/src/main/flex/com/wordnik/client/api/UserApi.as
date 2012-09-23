package com.wordnik.client.api {

import com.wordnik.swagger.common.ApiInvoker;
import com.wordnik.swagger.exception.ApiErrorCodes;
import com.wordnik.swagger.exception.ApiError;
import com.wordnik.swagger.common.ApiUserCredentials;
import com.wordnik.swagger.event.Response;
import com.wordnik.swagger.common.SwaggerApi;
import com.wordnik.client.model.User;
import mx.rpc.AsyncToken;
import mx.utils.UIDUtil;
import flash.utils.Dictionary;
import flash.events.EventDispatcher;

public class UserApi extends SwaggerApi {
    /**
    * Constructor for the UserApi api client
    * @param apiCredentials Wrapper object for tokens and hostName required towards authentication
    * @param eventDispatcher Optional event dispatcher that when provided is used by the SDK to dispatch any Response
    */
    public function UserApi(apiCredentials: ApiUserCredentials, eventDispatcher: EventDispatcher = null) {
        super(apiCredentials, eventDispatcher);
    }

public static const event_createUsersWithArrayInput: String = "createUsersWithArrayInput";
public static const event_createUser: String = "createUser";
public static const event_createUsersWithListInput: String = "createUsersWithListInput";
public static const event_updateUser: String = "updateUser";
public static const event_deleteUser: String = "deleteUser";
public static const event_getUserByName: String = "getUserByName";
public static const event_loginUser: String = "loginUser";
public static const event_logoutUser: String = "logoutUser";
/*
     * Returns void */
    public function createUsersWithArrayInput (body: Array): String {
        // create path and map variables
        var path: String = "/user.{format}/createWithArray".replace(/{format}/g,"xml");

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
        token.completionEventType = "createUsersWithArrayInput";

        token.returnType = null ;
        return requestId;

    }
    /*
     * Returns void */
    public function createUser (body: User): String {
        // create path and map variables
        var path: String = "/user.{format}".replace(/{format}/g,"xml");

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
        token.completionEventType = "createUser";

        token.returnType = null ;
        return requestId;

    }
    /*
     * Returns void */
    public function createUsersWithListInput (body: Array): String {
        // create path and map variables
        var path: String = "/user.{format}/createWithList".replace(/{format}/g,"xml");

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
        token.completionEventType = "createUsersWithListInput";

        token.returnType = null ;
        return requestId;

    }
    /*
     * Returns void */
    public function updateUser (username: String, body: User): String {
        // create path and map variables
        var path: String = "/user.{format}/{username}".replace(/{format}/g,"xml").replace("{" + "username" + "}", getApiInvoker().escapeString(username));

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if(username == null || body == null ) {
            throw new ApiError(400, "missing required params");
        }
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "PUT", queryParams, body, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "updateUser";

        token.returnType = null ;
        return requestId;

    }
    /*
     * Returns void */
    public function deleteUser (username: String): String {
        // create path and map variables
        var path: String = "/user.{format}/{username}".replace(/{format}/g,"xml").replace("{" + "username" + "}", getApiInvoker().escapeString(username));

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if(username == null ) {
            throw new ApiError(400, "missing required params");
        }
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "DELETE", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "deleteUser";

        token.returnType = null ;
        return requestId;

    }
    /*
     * Returns User */
    public function getUserByName (username: String): String {
        // create path and map variables
        var path: String = "/user.{format}/{username}".replace(/{format}/g,"xml").replace("{" + "username" + "}", getApiInvoker().escapeString(username));

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if(username == null ) {
            throw new ApiError(400, "missing required params");
        }
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "GET", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "getUserByName";

        token.returnType = User;
        return requestId;

    }
    /*
     * Returns string */
    public function loginUser (username: String, password: String): String {
        // create path and map variables
        var path: String = "/user.{format}/login".replace(/{format}/g,"xml");

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if(username == null || password == null ) {
            throw new ApiError(400, "missing required params");
        }
        if("null" != String(username))
            queryParams["username"] = toPathValue(username);
        if("null" != String(password))
            queryParams["password"] = toPathValue(password);
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "GET", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "loginUser";

        token.returnType = string;
        return requestId;

    }
    /*
     * Returns void */
    public function logoutUser (): String {
        // create path and map variables
        var path: String = "/user.{format}/logout".replace(/{format}/g,"xml");

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        var token:AsyncToken = getApiInvoker().invokeAPI(path, "GET", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "logoutUser";

        token.returnType = null ;
        return requestId;

    }
    }
        }
