package org.openapitools.client.api {

import org.openapitools.common.ApiInvoker;
import org.openapitools.exception.ApiErrorCodes;
import org.openapitools.exception.ApiError;
import org.openapitools.common.ApiUserCredentials;
import org.openapitools.event.Response;
import org.openapitools.common.OpenApi;
import org.openapitools.client.model.User;

import mx.rpc.AsyncToken;
import mx.utils.UIDUtil;
import flash.utils.Dictionary;
import flash.events.EventDispatcher;

public class UserApi extends OpenApi {
    /**
    * Constructor for the UserApi api client
    * @param apiCredentials Wrapper object for tokens and hostName required towards authentication
    * @param eventDispatcher Optional event dispatcher that when provided is used by the SDK to dispatch any Response
    */
    public function UserApi(apiCredentials: ApiUserCredentials, eventDispatcher: EventDispatcher = null) {
        super(apiCredentials, eventDispatcher);
    }

        public static const event_create_user: String = "create_user";
        public static const event_create_users_with_array_input: String = "create_users_with_array_input";
        public static const event_create_users_with_list_input: String = "create_users_with_list_input";
        public static const event_delete_user: String = "delete_user";
        public static const event_get_user_by_name: String = "get_user_by_name";
        public static const event_login_user: String = "login_user";
        public static const event_logout_user: String = "logout_user";
        public static const event_update_user: String = "update_user";


    /*
     * Returns void 
     */
    public function create_user (user: User): String {
        // create path and map variables
        var path: String = "/user".replace(/{format}/g,"xml");

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if() {
            throw new ApiError(400, "missing required params");
        }

        
        
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "POST", queryParams, user, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "create_user";

        token.returnType = null ;
        return requestId;

    }

    /*
     * Returns void 
     */
    public function create_users_with_array_input (user: Array): String {
        // create path and map variables
        var path: String = "/user/createWithArray".replace(/{format}/g,"xml");

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if() {
            throw new ApiError(400, "missing required params");
        }

        
        
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "POST", queryParams, user, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "create_users_with_array_input";

        token.returnType = null ;
        return requestId;

    }

    /*
     * Returns void 
     */
    public function create_users_with_list_input (user: Array): String {
        // create path and map variables
        var path: String = "/user/createWithList".replace(/{format}/g,"xml");

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if() {
            throw new ApiError(400, "missing required params");
        }

        
        
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "POST", queryParams, user, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "create_users_with_list_input";

        token.returnType = null ;
        return requestId;

    }

    /*
     * Returns void 
     */
    public function delete_user (username: String): String {
        // create path and map variables
        var path: String = "/user/{username}".replace(/{format}/g,"xml").replace("{" + "username" + "}", getApiInvoker().escapeString(username));

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
        token.completionEventType = "delete_user";

        token.returnType = null ;
        return requestId;

    }

    /*
     * Returns User 
     */
    public function get_user_by_name (username: String): String {
        // create path and map variables
        var path: String = "/user/{username}".replace(/{format}/g,"xml").replace("{" + "username" + "}", getApiInvoker().escapeString(username));

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
        token.completionEventType = "get_user_by_name";

        token.returnType = User;
        return requestId;

    }

    /*
     * Returns String 
     */
    public function login_user (username: String, password: String): String {
        // create path and map variables
        var path: String = "/user/login".replace(/{format}/g,"xml");

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if(        // verify required params are set
        if() {
            throw new ApiError(400, "missing required params");
        }
) {
            throw new ApiError(400, "missing required params");
        }

        if("null" != String(username))
            queryParams["username"] = toPathValue(username);
if("null" != String(password))
            queryParams["password"] = toPathValue(password);

        
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "GET", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "login_user";

        token.returnType = String;
        return requestId;

    }

    /*
     * Returns void 
     */
    public function logout_user (): String {
        // create path and map variables
        var path: String = "/user/logout".replace(/{format}/g,"xml");

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();


        
        
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "GET", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "logout_user";

        token.returnType = null ;
        return requestId;

    }

    /*
     * Returns void 
     */
    public function update_user (username: String, user: User): String {
        // create path and map variables
        var path: String = "/user/{username}".replace(/{format}/g,"xml").replace("{" + "username" + "}", getApiInvoker().escapeString(username));

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if(        // verify required params are set
        if() {
            throw new ApiError(400, "missing required params");
        }
) {
            throw new ApiError(400, "missing required params");
        }

        
        
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "PUT", queryParams, user, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "update_user";

        token.returnType = null ;
        return requestId;

    }
}
}
