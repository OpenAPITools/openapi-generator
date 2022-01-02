package org.openapitools.client.api;

import java.time.OffsetDateTime;
import org.openapitools.client.model.User;

import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.MultiMap;
import io.vertx.core.json.JsonObject;

import com.fasterxml.jackson.core.type.TypeReference;

import java.util.*;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;

import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.Pair;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen")
public class UserApiImpl implements UserApi {

    private ApiClient apiClient;

    public UserApiImpl() {
        this(null);
    }

    public UserApiImpl(ApiClient apiClient) {
        this.apiClient = apiClient != null ? apiClient : Configuration.getDefaultApiClient();
    }

    public ApiClient getApiClient() {
        return apiClient;
    }

    public void setApiClient(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    /**
    * Create user
    * This can only be done by the logged in user.
        * @param body Created user object (required)
    * @param resultHandler Asynchronous result handler
    */
    public void createUser(User body, Handler<AsyncResult<Void>> resultHandler) {
        createUser(body, null, resultHandler);
    }

    /**
    * Create user
    * This can only be done by the logged in user.
    * @param body Created user object (required)
    * @param authInfo per call authentication override.
    * @param resultHandler Asynchronous result handler
    */
    public void createUser(User body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> resultHandler) {
        Object localVarBody = body;
        
        // verify the required parameter 'body' is set
        if (body == null) {
            resultHandler.handle(ApiException.fail(400, "Missing the required parameter 'body' when calling createUser"));
            return;
        }
        
        // create path and map variables
        String localVarPath = "/user";

        // query params
        List<Pair> localVarQueryParams = new ArrayList<>();

        // header params
        MultiMap localVarHeaderParams = MultiMap.caseInsensitiveMultiMap();
        
        // cookie params
        MultiMap localVarCookieParams = MultiMap.caseInsensitiveMultiMap();
        
        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        
        String[] localVarAccepts = {  };
        String[] localVarContentTypes = {  };
        String[] localVarAuthNames = new String[] {  };

        apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, authInfo, null, resultHandler);
    }
    /**
    * Creates list of users with given input array
    * 
        * @param body List of user object (required)
    * @param resultHandler Asynchronous result handler
    */
    public void createUsersWithArrayInput(List<User> body, Handler<AsyncResult<Void>> resultHandler) {
        createUsersWithArrayInput(body, null, resultHandler);
    }

    /**
    * Creates list of users with given input array
    * 
    * @param body List of user object (required)
    * @param authInfo per call authentication override.
    * @param resultHandler Asynchronous result handler
    */
    public void createUsersWithArrayInput(List<User> body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> resultHandler) {
        Object localVarBody = body;
        
        // verify the required parameter 'body' is set
        if (body == null) {
            resultHandler.handle(ApiException.fail(400, "Missing the required parameter 'body' when calling createUsersWithArrayInput"));
            return;
        }
        
        // create path and map variables
        String localVarPath = "/user/createWithArray";

        // query params
        List<Pair> localVarQueryParams = new ArrayList<>();

        // header params
        MultiMap localVarHeaderParams = MultiMap.caseInsensitiveMultiMap();
        
        // cookie params
        MultiMap localVarCookieParams = MultiMap.caseInsensitiveMultiMap();
        
        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        
        String[] localVarAccepts = {  };
        String[] localVarContentTypes = {  };
        String[] localVarAuthNames = new String[] {  };

        apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, authInfo, null, resultHandler);
    }
    /**
    * Creates list of users with given input array
    * 
        * @param body List of user object (required)
    * @param resultHandler Asynchronous result handler
    */
    public void createUsersWithListInput(List<User> body, Handler<AsyncResult<Void>> resultHandler) {
        createUsersWithListInput(body, null, resultHandler);
    }

    /**
    * Creates list of users with given input array
    * 
    * @param body List of user object (required)
    * @param authInfo per call authentication override.
    * @param resultHandler Asynchronous result handler
    */
    public void createUsersWithListInput(List<User> body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> resultHandler) {
        Object localVarBody = body;
        
        // verify the required parameter 'body' is set
        if (body == null) {
            resultHandler.handle(ApiException.fail(400, "Missing the required parameter 'body' when calling createUsersWithListInput"));
            return;
        }
        
        // create path and map variables
        String localVarPath = "/user/createWithList";

        // query params
        List<Pair> localVarQueryParams = new ArrayList<>();

        // header params
        MultiMap localVarHeaderParams = MultiMap.caseInsensitiveMultiMap();
        
        // cookie params
        MultiMap localVarCookieParams = MultiMap.caseInsensitiveMultiMap();
        
        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        
        String[] localVarAccepts = {  };
        String[] localVarContentTypes = {  };
        String[] localVarAuthNames = new String[] {  };

        apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, authInfo, null, resultHandler);
    }
    /**
    * Delete user
    * This can only be done by the logged in user.
        * @param username The name that needs to be deleted (required)
    * @param resultHandler Asynchronous result handler
    */
    public void deleteUser(String username, Handler<AsyncResult<Void>> resultHandler) {
        deleteUser(username, null, resultHandler);
    }

    /**
    * Delete user
    * This can only be done by the logged in user.
    * @param username The name that needs to be deleted (required)
    * @param authInfo per call authentication override.
    * @param resultHandler Asynchronous result handler
    */
    public void deleteUser(String username, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> resultHandler) {
        Object localVarBody = null;
        
        // verify the required parameter 'username' is set
        if (username == null) {
            resultHandler.handle(ApiException.fail(400, "Missing the required parameter 'username' when calling deleteUser"));
            return;
        }
        
        // create path and map variables
        String localVarPath = "/user/{username}".replaceAll("\\{" + "username" + "\\}", encodeParameter(username.toString()));

        // query params
        List<Pair> localVarQueryParams = new ArrayList<>();

        // header params
        MultiMap localVarHeaderParams = MultiMap.caseInsensitiveMultiMap();
        
        // cookie params
        MultiMap localVarCookieParams = MultiMap.caseInsensitiveMultiMap();
        
        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        
        String[] localVarAccepts = {  };
        String[] localVarContentTypes = {  };
        String[] localVarAuthNames = new String[] {  };

        apiClient.invokeAPI(localVarPath, "DELETE", localVarQueryParams, localVarBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, authInfo, null, resultHandler);
    }
    /**
    * Get user by user name
    * 
        * @param username The name that needs to be fetched. Use user1 for testing. (required)
    * @param resultHandler Asynchronous result handler
    */
    public void getUserByName(String username, Handler<AsyncResult<User>> resultHandler) {
        getUserByName(username, null, resultHandler);
    }

    /**
    * Get user by user name
    * 
    * @param username The name that needs to be fetched. Use user1 for testing. (required)
    * @param authInfo per call authentication override.
    * @param resultHandler Asynchronous result handler
    */
    public void getUserByName(String username, ApiClient.AuthInfo authInfo, Handler<AsyncResult<User>> resultHandler) {
        Object localVarBody = null;
        
        // verify the required parameter 'username' is set
        if (username == null) {
            resultHandler.handle(ApiException.fail(400, "Missing the required parameter 'username' when calling getUserByName"));
            return;
        }
        
        // create path and map variables
        String localVarPath = "/user/{username}".replaceAll("\\{" + "username" + "\\}", encodeParameter(username.toString()));

        // query params
        List<Pair> localVarQueryParams = new ArrayList<>();

        // header params
        MultiMap localVarHeaderParams = MultiMap.caseInsensitiveMultiMap();
        
        // cookie params
        MultiMap localVarCookieParams = MultiMap.caseInsensitiveMultiMap();
        
        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        
        String[] localVarAccepts = { "application/xml", "application/json" };
        String[] localVarContentTypes = {  };
        String[] localVarAuthNames = new String[] {  };
        TypeReference<User> localVarReturnType = new TypeReference<User>() {};
        apiClient.invokeAPI(localVarPath, "GET", localVarQueryParams, localVarBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, authInfo, localVarReturnType, resultHandler);
    }
    /**
    * Logs user into the system
    * 
        * @param username The user name for login (required)
        * @param password The password for login in clear text (required)
    * @param resultHandler Asynchronous result handler
    */
    public void loginUser(String username, String password, Handler<AsyncResult<String>> resultHandler) {
        loginUser(username, password, null, resultHandler);
    }

    /**
    * Logs user into the system
    * 
    * @param username The user name for login (required)
    * @param password The password for login in clear text (required)
    * @param authInfo per call authentication override.
    * @param resultHandler Asynchronous result handler
    */
    public void loginUser(String username, String password, ApiClient.AuthInfo authInfo, Handler<AsyncResult<String>> resultHandler) {
        Object localVarBody = null;
        
        // verify the required parameter 'username' is set
        if (username == null) {
            resultHandler.handle(ApiException.fail(400, "Missing the required parameter 'username' when calling loginUser"));
            return;
        }
        
        // verify the required parameter 'password' is set
        if (password == null) {
            resultHandler.handle(ApiException.fail(400, "Missing the required parameter 'password' when calling loginUser"));
            return;
        }
        
        // create path and map variables
        String localVarPath = "/user/login";

        // query params
        List<Pair> localVarQueryParams = new ArrayList<>();
        localVarQueryParams.addAll(apiClient.parameterToPairs("", "username", username));
        localVarQueryParams.addAll(apiClient.parameterToPairs("", "password", password));

        // header params
        MultiMap localVarHeaderParams = MultiMap.caseInsensitiveMultiMap();
        
        // cookie params
        MultiMap localVarCookieParams = MultiMap.caseInsensitiveMultiMap();
        
        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        
        String[] localVarAccepts = { "application/xml", "application/json" };
        String[] localVarContentTypes = {  };
        String[] localVarAuthNames = new String[] {  };
        TypeReference<String> localVarReturnType = new TypeReference<String>() {};
        apiClient.invokeAPI(localVarPath, "GET", localVarQueryParams, localVarBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, authInfo, localVarReturnType, resultHandler);
    }
    /**
    * Logs out current logged in user session
    * 
    * @param resultHandler Asynchronous result handler
    */
    public void logoutUser(Handler<AsyncResult<Void>> resultHandler) {
        logoutUser(null, resultHandler);
    }

    /**
    * Logs out current logged in user session
    * 
    * @param authInfo per call authentication override.
    * @param resultHandler Asynchronous result handler
    */
    public void logoutUser(ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> resultHandler) {
        Object localVarBody = null;
        
        // create path and map variables
        String localVarPath = "/user/logout";

        // query params
        List<Pair> localVarQueryParams = new ArrayList<>();

        // header params
        MultiMap localVarHeaderParams = MultiMap.caseInsensitiveMultiMap();
        
        // cookie params
        MultiMap localVarCookieParams = MultiMap.caseInsensitiveMultiMap();
        
        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        
        String[] localVarAccepts = {  };
        String[] localVarContentTypes = {  };
        String[] localVarAuthNames = new String[] {  };

        apiClient.invokeAPI(localVarPath, "GET", localVarQueryParams, localVarBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, authInfo, null, resultHandler);
    }
    /**
    * Updated user
    * This can only be done by the logged in user.
        * @param username name that need to be deleted (required)
        * @param body Updated user object (required)
    * @param resultHandler Asynchronous result handler
    */
    public void updateUser(String username, User body, Handler<AsyncResult<Void>> resultHandler) {
        updateUser(username, body, null, resultHandler);
    }

    /**
    * Updated user
    * This can only be done by the logged in user.
    * @param username name that need to be deleted (required)
    * @param body Updated user object (required)
    * @param authInfo per call authentication override.
    * @param resultHandler Asynchronous result handler
    */
    public void updateUser(String username, User body, ApiClient.AuthInfo authInfo, Handler<AsyncResult<Void>> resultHandler) {
        Object localVarBody = body;
        
        // verify the required parameter 'username' is set
        if (username == null) {
            resultHandler.handle(ApiException.fail(400, "Missing the required parameter 'username' when calling updateUser"));
            return;
        }
        
        // verify the required parameter 'body' is set
        if (body == null) {
            resultHandler.handle(ApiException.fail(400, "Missing the required parameter 'body' when calling updateUser"));
            return;
        }
        
        // create path and map variables
        String localVarPath = "/user/{username}".replaceAll("\\{" + "username" + "\\}", encodeParameter(username.toString()));

        // query params
        List<Pair> localVarQueryParams = new ArrayList<>();

        // header params
        MultiMap localVarHeaderParams = MultiMap.caseInsensitiveMultiMap();
        
        // cookie params
        MultiMap localVarCookieParams = MultiMap.caseInsensitiveMultiMap();
        
        // form params
        // TODO: sending files within multipart/form-data is not supported yet (because of vertx web-client)
        Map<String, Object> localVarFormParams = new HashMap<>();
        
        String[] localVarAccepts = {  };
        String[] localVarContentTypes = {  };
        String[] localVarAuthNames = new String[] {  };

        apiClient.invokeAPI(localVarPath, "PUT", localVarQueryParams, localVarBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccepts, localVarContentTypes, localVarAuthNames, authInfo, null, resultHandler);
    }

    private String encodeParameter(String parameter) {
        try {
            return URLEncoder.encode(parameter, StandardCharsets.UTF_8.name());
        } catch (UnsupportedEncodingException e) {
            return parameter;
        }
    }
}
