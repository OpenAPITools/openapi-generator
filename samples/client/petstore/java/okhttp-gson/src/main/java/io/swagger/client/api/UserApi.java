package io.swagger.client.api;

import io.swagger.client.ApiCallback;
import io.swagger.client.ApiClient;
import io.swagger.client.ApiException;
import io.swagger.client.Configuration;
import io.swagger.client.Pair;

import com.google.gson.reflect.TypeToken;

import com.squareup.okhttp.Call;

import io.swagger.client.model.User;
import java.util.*;

import java.lang.reflect.Type;
import java.util.*;

public class UserApi {
  private ApiClient apiClient;

  public UserApi() {
    this(Configuration.getDefaultApiClient());
  }

  public UserApi(ApiClient apiClient) {
    this.apiClient = apiClient;
  }

  public ApiClient getApiClient() {
    return apiClient;
  }

  public void setApiClient(ApiClient apiClient) {
    this.apiClient = apiClient;
  }

  
  /* Build call for createUser */
  private Call createUserCall(User body) throws ApiException {
    Object postBody = body;
    

    // create path and map variables
    String path = "/user".replaceAll("\\{format\\}","json");

    List<Pair> queryParams = new ArrayList<Pair>();

    Map<String, String> headerParams = new HashMap<String, String>();

    Map<String, Object> formParams = new HashMap<String, Object>();

    final String[] accepts = {
      "application/json", "application/xml"
    };
    final String accept = apiClient.selectHeaderAccept(accepts);
    if (accept != null) headerParams.put("Accept", accept);

    final String[] contentTypes = {
      
    };
    final String contentType = apiClient.selectHeaderContentType(contentTypes);
    headerParams.put("Content-Type", contentType);

    String[] authNames = new String[] {  };
    return apiClient.buildCall(path, "POST", queryParams, postBody, headerParams, formParams, authNames);
  }

  /**
   * Create user
   * This can only be done by the logged in user.
   * @param body Created user object
   */
  public void createUser(User body) throws ApiException {
    Call call = createUserCall(body);
    apiClient.execute(call);
  }

  /**
   * Create user (asynchronously)
   * This can only be done by the logged in user.
   * @param body Created user object
   * @param callback The callback to be executed when the API call finishes
   * @return The request call
   */
  public Call createUserAsync(User body, ApiCallback<Void> callback) throws ApiException {
    Call call = createUserCall(body);
    apiClient.executeAsync(call, callback);
    return call;
  }
  
  /* Build call for createUsersWithArrayInput */
  private Call createUsersWithArrayInputCall(List<User> body) throws ApiException {
    Object postBody = body;
    

    // create path and map variables
    String path = "/user/createWithArray".replaceAll("\\{format\\}","json");

    List<Pair> queryParams = new ArrayList<Pair>();

    Map<String, String> headerParams = new HashMap<String, String>();

    Map<String, Object> formParams = new HashMap<String, Object>();

    final String[] accepts = {
      "application/json", "application/xml"
    };
    final String accept = apiClient.selectHeaderAccept(accepts);
    if (accept != null) headerParams.put("Accept", accept);

    final String[] contentTypes = {
      
    };
    final String contentType = apiClient.selectHeaderContentType(contentTypes);
    headerParams.put("Content-Type", contentType);

    String[] authNames = new String[] {  };
    return apiClient.buildCall(path, "POST", queryParams, postBody, headerParams, formParams, authNames);
  }

  /**
   * Creates list of users with given input array
   * 
   * @param body List of user object
   */
  public void createUsersWithArrayInput(List<User> body) throws ApiException {
    Call call = createUsersWithArrayInputCall(body);
    apiClient.execute(call);
  }

  /**
   * Creates list of users with given input array (asynchronously)
   * 
   * @param body List of user object
   * @param callback The callback to be executed when the API call finishes
   * @return The request call
   */
  public Call createUsersWithArrayInputAsync(List<User> body, ApiCallback<Void> callback) throws ApiException {
    Call call = createUsersWithArrayInputCall(body);
    apiClient.executeAsync(call, callback);
    return call;
  }
  
  /* Build call for createUsersWithListInput */
  private Call createUsersWithListInputCall(List<User> body) throws ApiException {
    Object postBody = body;
    

    // create path and map variables
    String path = "/user/createWithList".replaceAll("\\{format\\}","json");

    List<Pair> queryParams = new ArrayList<Pair>();

    Map<String, String> headerParams = new HashMap<String, String>();

    Map<String, Object> formParams = new HashMap<String, Object>();

    final String[] accepts = {
      "application/json", "application/xml"
    };
    final String accept = apiClient.selectHeaderAccept(accepts);
    if (accept != null) headerParams.put("Accept", accept);

    final String[] contentTypes = {
      
    };
    final String contentType = apiClient.selectHeaderContentType(contentTypes);
    headerParams.put("Content-Type", contentType);

    String[] authNames = new String[] {  };
    return apiClient.buildCall(path, "POST", queryParams, postBody, headerParams, formParams, authNames);
  }

  /**
   * Creates list of users with given input array
   * 
   * @param body List of user object
   */
  public void createUsersWithListInput(List<User> body) throws ApiException {
    Call call = createUsersWithListInputCall(body);
    apiClient.execute(call);
  }

  /**
   * Creates list of users with given input array (asynchronously)
   * 
   * @param body List of user object
   * @param callback The callback to be executed when the API call finishes
   * @return The request call
   */
  public Call createUsersWithListInputAsync(List<User> body, ApiCallback<Void> callback) throws ApiException {
    Call call = createUsersWithListInputCall(body);
    apiClient.executeAsync(call, callback);
    return call;
  }
  
  /* Build call for loginUser */
  private Call loginUserCall(String username, String password) throws ApiException {
    Object postBody = null;
    

    // create path and map variables
    String path = "/user/login".replaceAll("\\{format\\}","json");

    List<Pair> queryParams = new ArrayList<Pair>();
    if (username != null)
      queryParams.addAll(apiClient.parameterToPairs("", "username", username));
    if (password != null)
      queryParams.addAll(apiClient.parameterToPairs("", "password", password));

    Map<String, String> headerParams = new HashMap<String, String>();

    Map<String, Object> formParams = new HashMap<String, Object>();

    final String[] accepts = {
      "application/json", "application/xml"
    };
    final String accept = apiClient.selectHeaderAccept(accepts);
    if (accept != null) headerParams.put("Accept", accept);

    final String[] contentTypes = {
      
    };
    final String contentType = apiClient.selectHeaderContentType(contentTypes);
    headerParams.put("Content-Type", contentType);

    String[] authNames = new String[] {  };
    return apiClient.buildCall(path, "GET", queryParams, postBody, headerParams, formParams, authNames);
  }

  /**
   * Logs user into the system
   * 
   * @param username The user name for login
   * @param password The password for login in clear text
   * @return String
   */
  public String loginUser(String username, String password) throws ApiException {
    Call call = loginUserCall(username, password);
    Type returnType = new TypeToken<String>(){}.getType();
    return apiClient.execute(call, returnType);
  }

  /**
   * Logs user into the system (asynchronously)
   * 
   * @param username The user name for login
   * @param password The password for login in clear text
   * @param callback The callback to be executed when the API call finishes
   * @return The request call
   */
  public Call loginUserAsync(String username, String password, ApiCallback<String> callback) throws ApiException {
    Call call = loginUserCall(username, password);
    Type returnType = new TypeToken<String>(){}.getType();
    apiClient.executeAsync(call, returnType, callback);
    return call;
  }
  
  /* Build call for logoutUser */
  private Call logoutUserCall() throws ApiException {
    Object postBody = null;
    

    // create path and map variables
    String path = "/user/logout".replaceAll("\\{format\\}","json");

    List<Pair> queryParams = new ArrayList<Pair>();

    Map<String, String> headerParams = new HashMap<String, String>();

    Map<String, Object> formParams = new HashMap<String, Object>();

    final String[] accepts = {
      "application/json", "application/xml"
    };
    final String accept = apiClient.selectHeaderAccept(accepts);
    if (accept != null) headerParams.put("Accept", accept);

    final String[] contentTypes = {
      
    };
    final String contentType = apiClient.selectHeaderContentType(contentTypes);
    headerParams.put("Content-Type", contentType);

    String[] authNames = new String[] {  };
    return apiClient.buildCall(path, "GET", queryParams, postBody, headerParams, formParams, authNames);
  }

  /**
   * Logs out current logged in user session
   * 
   */
  public void logoutUser() throws ApiException {
    Call call = logoutUserCall();
    apiClient.execute(call);
  }

  /**
   * Logs out current logged in user session (asynchronously)
   * 
   * @param callback The callback to be executed when the API call finishes
   * @return The request call
   */
  public Call logoutUserAsync(ApiCallback<Void> callback) throws ApiException {
    Call call = logoutUserCall();
    apiClient.executeAsync(call, callback);
    return call;
  }
  
  /* Build call for getUserByName */
  private Call getUserByNameCall(String username) throws ApiException {
    Object postBody = null;
    
    // verify the required parameter 'username' is set
    if (username == null) {
       throw new ApiException("Missing the required parameter 'username' when calling getUserByName(Async)");
    }
    

    // create path and map variables
    String path = "/user/{username}".replaceAll("\\{format\\}","json")
      .replaceAll("\\{" + "username" + "\\}", apiClient.escapeString(username.toString()));

    List<Pair> queryParams = new ArrayList<Pair>();

    Map<String, String> headerParams = new HashMap<String, String>();

    Map<String, Object> formParams = new HashMap<String, Object>();

    final String[] accepts = {
      "application/json", "application/xml"
    };
    final String accept = apiClient.selectHeaderAccept(accepts);
    if (accept != null) headerParams.put("Accept", accept);

    final String[] contentTypes = {
      
    };
    final String contentType = apiClient.selectHeaderContentType(contentTypes);
    headerParams.put("Content-Type", contentType);

    String[] authNames = new String[] {  };
    return apiClient.buildCall(path, "GET", queryParams, postBody, headerParams, formParams, authNames);
  }

  /**
   * Get user by user name
   * 
   * @param username The name that needs to be fetched. Use user1 for testing.
   * @return User
   */
  public User getUserByName(String username) throws ApiException {
    Call call = getUserByNameCall(username);
    Type returnType = new TypeToken<User>(){}.getType();
    return apiClient.execute(call, returnType);
  }

  /**
   * Get user by user name (asynchronously)
   * 
   * @param username The name that needs to be fetched. Use user1 for testing.
   * @param callback The callback to be executed when the API call finishes
   * @return The request call
   */
  public Call getUserByNameAsync(String username, ApiCallback<User> callback) throws ApiException {
    Call call = getUserByNameCall(username);
    Type returnType = new TypeToken<User>(){}.getType();
    apiClient.executeAsync(call, returnType, callback);
    return call;
  }
  
  /* Build call for updateUser */
  private Call updateUserCall(String username, User body) throws ApiException {
    Object postBody = body;
    
    // verify the required parameter 'username' is set
    if (username == null) {
       throw new ApiException("Missing the required parameter 'username' when calling updateUser(Async)");
    }
    

    // create path and map variables
    String path = "/user/{username}".replaceAll("\\{format\\}","json")
      .replaceAll("\\{" + "username" + "\\}", apiClient.escapeString(username.toString()));

    List<Pair> queryParams = new ArrayList<Pair>();

    Map<String, String> headerParams = new HashMap<String, String>();

    Map<String, Object> formParams = new HashMap<String, Object>();

    final String[] accepts = {
      "application/json", "application/xml"
    };
    final String accept = apiClient.selectHeaderAccept(accepts);
    if (accept != null) headerParams.put("Accept", accept);

    final String[] contentTypes = {
      
    };
    final String contentType = apiClient.selectHeaderContentType(contentTypes);
    headerParams.put("Content-Type", contentType);

    String[] authNames = new String[] {  };
    return apiClient.buildCall(path, "PUT", queryParams, postBody, headerParams, formParams, authNames);
  }

  /**
   * Updated user
   * This can only be done by the logged in user.
   * @param username name that need to be deleted
   * @param body Updated user object
   */
  public void updateUser(String username, User body) throws ApiException {
    Call call = updateUserCall(username, body);
    apiClient.execute(call);
  }

  /**
   * Updated user (asynchronously)
   * This can only be done by the logged in user.
   * @param username name that need to be deleted
   * @param body Updated user object
   * @param callback The callback to be executed when the API call finishes
   * @return The request call
   */
  public Call updateUserAsync(String username, User body, ApiCallback<Void> callback) throws ApiException {
    Call call = updateUserCall(username, body);
    apiClient.executeAsync(call, callback);
    return call;
  }
  
  /* Build call for deleteUser */
  private Call deleteUserCall(String username) throws ApiException {
    Object postBody = null;
    
    // verify the required parameter 'username' is set
    if (username == null) {
       throw new ApiException("Missing the required parameter 'username' when calling deleteUser(Async)");
    }
    

    // create path and map variables
    String path = "/user/{username}".replaceAll("\\{format\\}","json")
      .replaceAll("\\{" + "username" + "\\}", apiClient.escapeString(username.toString()));

    List<Pair> queryParams = new ArrayList<Pair>();

    Map<String, String> headerParams = new HashMap<String, String>();

    Map<String, Object> formParams = new HashMap<String, Object>();

    final String[] accepts = {
      "application/json", "application/xml"
    };
    final String accept = apiClient.selectHeaderAccept(accepts);
    if (accept != null) headerParams.put("Accept", accept);

    final String[] contentTypes = {
      
    };
    final String contentType = apiClient.selectHeaderContentType(contentTypes);
    headerParams.put("Content-Type", contentType);

    String[] authNames = new String[] {  };
    return apiClient.buildCall(path, "DELETE", queryParams, postBody, headerParams, formParams, authNames);
  }

  /**
   * Delete user
   * This can only be done by the logged in user.
   * @param username The name that needs to be deleted
   */
  public void deleteUser(String username) throws ApiException {
    Call call = deleteUserCall(username);
    apiClient.execute(call);
  }

  /**
   * Delete user (asynchronously)
   * This can only be done by the logged in user.
   * @param username The name that needs to be deleted
   * @param callback The callback to be executed when the API call finishes
   * @return The request call
   */
  public Call deleteUserAsync(String username, ApiCallback<Void> callback) throws ApiException {
    Call call = deleteUserCall(username);
    apiClient.executeAsync(call, callback);
    return call;
  }
  
}
