package org.openapitools.client.api;

import org.openapitools.client.ApiException;
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiResponse;
import org.openapitools.client.Configuration;
import org.openapitools.client.Pair;

import javax.ws.rs.core.GenericType;

import java.time.OffsetDateTime;
import org.openapitools.client.model.User;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen")
public class UserApi {
  private ApiClient apiClient;

  public UserApi() {
    this(Configuration.getDefaultApiClient());
  }

  public UserApi(ApiClient apiClient) {
    this.apiClient = apiClient;
  }

  /**
   * Get the API client
   *
   * @return API client
   */
  public ApiClient getApiClient() {
    return apiClient;
  }

  /**
   * Set the API client
   *
   * @param apiClient an instance of API client
   */
  public void setApiClient(ApiClient apiClient) {
    this.apiClient = apiClient;
  }

  /**
   * Create user
   * This can only be done by the logged in user.
   * @param user Created user object (required)
   * @throws ApiException if fails to make API call
   * @http.response.details
     <table summary="Response Details" border="1">
       <tr><td> Status Code </td><td> Description </td><td> Response Headers </td></tr>
       <tr><td> 0 </td><td> successful operation </td><td>  -  </td></tr>
     </table>
   */
  public void createUser(User user) throws ApiException {
    createUserWithHttpInfo(user);
  }

  /**
   * Create user
   * This can only be done by the logged in user.
   * @param user Created user object (required)
   * @return ApiResponse&lt;Void&gt;
   * @throws ApiException if fails to make API call
   * @http.response.details
     <table summary="Response Details" border="1">
       <tr><td> Status Code </td><td> Description </td><td> Response Headers </td></tr>
       <tr><td> 0 </td><td> successful operation </td><td>  -  </td></tr>
     </table>
   */
  public ApiResponse<Void> createUserWithHttpInfo(User user) throws ApiException {
    // Check required parameters
    if (user == null) {
      throw new ApiException(400, "Missing the required parameter 'user' when calling createUser");
    }

    String localVarAccept = apiClient.selectHeaderAccept();
    String localVarContentType = apiClient.selectHeaderContentType("application/json");
    return apiClient.invokeAPI("UserApi.createUser", "/user", "POST", new ArrayList<>(), user,
                               new LinkedHashMap<>(), new LinkedHashMap<>(), new LinkedHashMap<>(), localVarAccept, localVarContentType,
                               null, null, false);
  }
  /**
   * Creates list of users with given input array
   * 
   * @param user List of user object (required)
   * @throws ApiException if fails to make API call
   * @http.response.details
     <table summary="Response Details" border="1">
       <tr><td> Status Code </td><td> Description </td><td> Response Headers </td></tr>
       <tr><td> 0 </td><td> successful operation </td><td>  -  </td></tr>
     </table>
   */
  public void createUsersWithArrayInput(List<User> user) throws ApiException {
    createUsersWithArrayInputWithHttpInfo(user);
  }

  /**
   * Creates list of users with given input array
   * 
   * @param user List of user object (required)
   * @return ApiResponse&lt;Void&gt;
   * @throws ApiException if fails to make API call
   * @http.response.details
     <table summary="Response Details" border="1">
       <tr><td> Status Code </td><td> Description </td><td> Response Headers </td></tr>
       <tr><td> 0 </td><td> successful operation </td><td>  -  </td></tr>
     </table>
   */
  public ApiResponse<Void> createUsersWithArrayInputWithHttpInfo(List<User> user) throws ApiException {
    // Check required parameters
    if (user == null) {
      throw new ApiException(400, "Missing the required parameter 'user' when calling createUsersWithArrayInput");
    }

    String localVarAccept = apiClient.selectHeaderAccept();
    String localVarContentType = apiClient.selectHeaderContentType("application/json");
    return apiClient.invokeAPI("UserApi.createUsersWithArrayInput", "/user/createWithArray", "POST", new ArrayList<>(), user,
                               new LinkedHashMap<>(), new LinkedHashMap<>(), new LinkedHashMap<>(), localVarAccept, localVarContentType,
                               null, null, false);
  }
  /**
   * Creates list of users with given input array
   * 
   * @param user List of user object (required)
   * @throws ApiException if fails to make API call
   * @http.response.details
     <table summary="Response Details" border="1">
       <tr><td> Status Code </td><td> Description </td><td> Response Headers </td></tr>
       <tr><td> 0 </td><td> successful operation </td><td>  -  </td></tr>
     </table>
   */
  public void createUsersWithListInput(List<User> user) throws ApiException {
    createUsersWithListInputWithHttpInfo(user);
  }

  /**
   * Creates list of users with given input array
   * 
   * @param user List of user object (required)
   * @return ApiResponse&lt;Void&gt;
   * @throws ApiException if fails to make API call
   * @http.response.details
     <table summary="Response Details" border="1">
       <tr><td> Status Code </td><td> Description </td><td> Response Headers </td></tr>
       <tr><td> 0 </td><td> successful operation </td><td>  -  </td></tr>
     </table>
   */
  public ApiResponse<Void> createUsersWithListInputWithHttpInfo(List<User> user) throws ApiException {
    // Check required parameters
    if (user == null) {
      throw new ApiException(400, "Missing the required parameter 'user' when calling createUsersWithListInput");
    }

    String localVarAccept = apiClient.selectHeaderAccept();
    String localVarContentType = apiClient.selectHeaderContentType("application/json");
    return apiClient.invokeAPI("UserApi.createUsersWithListInput", "/user/createWithList", "POST", new ArrayList<>(), user,
                               new LinkedHashMap<>(), new LinkedHashMap<>(), new LinkedHashMap<>(), localVarAccept, localVarContentType,
                               null, null, false);
  }
  /**
   * Delete user
   * This can only be done by the logged in user.
   * @param username The name that needs to be deleted (required)
   * @throws ApiException if fails to make API call
   * @http.response.details
     <table summary="Response Details" border="1">
       <tr><td> Status Code </td><td> Description </td><td> Response Headers </td></tr>
       <tr><td> 400 </td><td> Invalid username supplied </td><td>  -  </td></tr>
       <tr><td> 404 </td><td> User not found </td><td>  -  </td></tr>
     </table>
   */
  public void deleteUser(String username) throws ApiException {
    deleteUserWithHttpInfo(username);
  }

  /**
   * Delete user
   * This can only be done by the logged in user.
   * @param username The name that needs to be deleted (required)
   * @return ApiResponse&lt;Void&gt;
   * @throws ApiException if fails to make API call
   * @http.response.details
     <table summary="Response Details" border="1">
       <tr><td> Status Code </td><td> Description </td><td> Response Headers </td></tr>
       <tr><td> 400 </td><td> Invalid username supplied </td><td>  -  </td></tr>
       <tr><td> 404 </td><td> User not found </td><td>  -  </td></tr>
     </table>
   */
  public ApiResponse<Void> deleteUserWithHttpInfo(String username) throws ApiException {
    // Check required parameters
    if (username == null) {
      throw new ApiException(400, "Missing the required parameter 'username' when calling deleteUser");
    }

    // Path parameters
    String localVarPath = "/user/{username}"
            .replaceAll("\\{username}", apiClient.escapeString(username));

    String localVarAccept = apiClient.selectHeaderAccept();
    String localVarContentType = apiClient.selectHeaderContentType();
    return apiClient.invokeAPI("UserApi.deleteUser", localVarPath, "DELETE", new ArrayList<>(), null,
                               new LinkedHashMap<>(), new LinkedHashMap<>(), new LinkedHashMap<>(), localVarAccept, localVarContentType,
                               null, null, false);
  }
  /**
   * Get user by user name
   * 
   * @param username The name that needs to be fetched. Use user1 for testing. (required)
   * @return User
   * @throws ApiException if fails to make API call
   * @http.response.details
     <table summary="Response Details" border="1">
       <tr><td> Status Code </td><td> Description </td><td> Response Headers </td></tr>
       <tr><td> 200 </td><td> successful operation </td><td>  -  </td></tr>
       <tr><td> 400 </td><td> Invalid username supplied </td><td>  -  </td></tr>
       <tr><td> 404 </td><td> User not found </td><td>  -  </td></tr>
     </table>
   */
  public User getUserByName(String username) throws ApiException {
    return getUserByNameWithHttpInfo(username).getData();
  }

  /**
   * Get user by user name
   * 
   * @param username The name that needs to be fetched. Use user1 for testing. (required)
   * @return ApiResponse&lt;User&gt;
   * @throws ApiException if fails to make API call
   * @http.response.details
     <table summary="Response Details" border="1">
       <tr><td> Status Code </td><td> Description </td><td> Response Headers </td></tr>
       <tr><td> 200 </td><td> successful operation </td><td>  -  </td></tr>
       <tr><td> 400 </td><td> Invalid username supplied </td><td>  -  </td></tr>
       <tr><td> 404 </td><td> User not found </td><td>  -  </td></tr>
     </table>
   */
  public ApiResponse<User> getUserByNameWithHttpInfo(String username) throws ApiException {
    // Check required parameters
    if (username == null) {
      throw new ApiException(400, "Missing the required parameter 'username' when calling getUserByName");
    }

    // Path parameters
    String localVarPath = "/user/{username}"
            .replaceAll("\\{username}", apiClient.escapeString(username));

    String localVarAccept = apiClient.selectHeaderAccept("application/xml", "application/json");
    String localVarContentType = apiClient.selectHeaderContentType();
    GenericType<User> localVarReturnType = new GenericType<User>() {};
    return apiClient.invokeAPI("UserApi.getUserByName", localVarPath, "GET", new ArrayList<>(), null,
                               new LinkedHashMap<>(), new LinkedHashMap<>(), new LinkedHashMap<>(), localVarAccept, localVarContentType,
                               null, localVarReturnType, false);
  }
  /**
   * Logs user into the system
   * 
   * @param username The user name for login (required)
   * @param password The password for login in clear text (required)
   * @return String
   * @throws ApiException if fails to make API call
   * @http.response.details
     <table summary="Response Details" border="1">
       <tr><td> Status Code </td><td> Description </td><td> Response Headers </td></tr>
       <tr><td> 200 </td><td> successful operation </td><td>  * X-Rate-Limit - calls per hour allowed by the user <br>  * X-Expires-After - date in UTC when token expires <br>  </td></tr>
       <tr><td> 400 </td><td> Invalid username/password supplied </td><td>  -  </td></tr>
     </table>
   */
  public String loginUser(String username, String password) throws ApiException {
    return loginUserWithHttpInfo(username, password).getData();
  }

  /**
   * Logs user into the system
   * 
   * @param username The user name for login (required)
   * @param password The password for login in clear text (required)
   * @return ApiResponse&lt;String&gt;
   * @throws ApiException if fails to make API call
   * @http.response.details
     <table summary="Response Details" border="1">
       <tr><td> Status Code </td><td> Description </td><td> Response Headers </td></tr>
       <tr><td> 200 </td><td> successful operation </td><td>  * X-Rate-Limit - calls per hour allowed by the user <br>  * X-Expires-After - date in UTC when token expires <br>  </td></tr>
       <tr><td> 400 </td><td> Invalid username/password supplied </td><td>  -  </td></tr>
     </table>
   */
  public ApiResponse<String> loginUserWithHttpInfo(String username, String password) throws ApiException {
    // Check required parameters
    if (username == null) {
      throw new ApiException(400, "Missing the required parameter 'username' when calling loginUser");
    }
    if (password == null) {
      throw new ApiException(400, "Missing the required parameter 'password' when calling loginUser");
    }

    // Query parameters
    List<Pair> localVarQueryParams = new ArrayList<>(
            apiClient.parameterToPairs("", "username", username)
    );
    localVarQueryParams.addAll(apiClient.parameterToPairs("", "password", password));

    String localVarAccept = apiClient.selectHeaderAccept("application/xml", "application/json");
    String localVarContentType = apiClient.selectHeaderContentType();
    GenericType<String> localVarReturnType = new GenericType<String>() {};
    return apiClient.invokeAPI("UserApi.loginUser", "/user/login", "GET", localVarQueryParams, null,
                               new LinkedHashMap<>(), new LinkedHashMap<>(), new LinkedHashMap<>(), localVarAccept, localVarContentType,
                               null, localVarReturnType, false);
  }
  /**
   * Logs out current logged in user session
   * 
   * @throws ApiException if fails to make API call
   * @http.response.details
     <table summary="Response Details" border="1">
       <tr><td> Status Code </td><td> Description </td><td> Response Headers </td></tr>
       <tr><td> 0 </td><td> successful operation </td><td>  -  </td></tr>
     </table>
   */
  public void logoutUser() throws ApiException {
    logoutUserWithHttpInfo();
  }

  /**
   * Logs out current logged in user session
   * 
   * @return ApiResponse&lt;Void&gt;
   * @throws ApiException if fails to make API call
   * @http.response.details
     <table summary="Response Details" border="1">
       <tr><td> Status Code </td><td> Description </td><td> Response Headers </td></tr>
       <tr><td> 0 </td><td> successful operation </td><td>  -  </td></tr>
     </table>
   */
  public ApiResponse<Void> logoutUserWithHttpInfo() throws ApiException {
    String localVarAccept = apiClient.selectHeaderAccept();
    String localVarContentType = apiClient.selectHeaderContentType();
    return apiClient.invokeAPI("UserApi.logoutUser", "/user/logout", "GET", new ArrayList<>(), null,
                               new LinkedHashMap<>(), new LinkedHashMap<>(), new LinkedHashMap<>(), localVarAccept, localVarContentType,
                               null, null, false);
  }
  /**
   * Updated user
   * This can only be done by the logged in user.
   * @param username name that need to be deleted (required)
   * @param user Updated user object (required)
   * @throws ApiException if fails to make API call
   * @http.response.details
     <table summary="Response Details" border="1">
       <tr><td> Status Code </td><td> Description </td><td> Response Headers </td></tr>
       <tr><td> 400 </td><td> Invalid user supplied </td><td>  -  </td></tr>
       <tr><td> 404 </td><td> User not found </td><td>  -  </td></tr>
     </table>
   */
  public void updateUser(String username, User user) throws ApiException {
    updateUserWithHttpInfo(username, user);
  }

  /**
   * Updated user
   * This can only be done by the logged in user.
   * @param username name that need to be deleted (required)
   * @param user Updated user object (required)
   * @return ApiResponse&lt;Void&gt;
   * @throws ApiException if fails to make API call
   * @http.response.details
     <table summary="Response Details" border="1">
       <tr><td> Status Code </td><td> Description </td><td> Response Headers </td></tr>
       <tr><td> 400 </td><td> Invalid user supplied </td><td>  -  </td></tr>
       <tr><td> 404 </td><td> User not found </td><td>  -  </td></tr>
     </table>
   */
  public ApiResponse<Void> updateUserWithHttpInfo(String username, User user) throws ApiException {
    // Check required parameters
    if (username == null) {
      throw new ApiException(400, "Missing the required parameter 'username' when calling updateUser");
    }
    if (user == null) {
      throw new ApiException(400, "Missing the required parameter 'user' when calling updateUser");
    }

    // Path parameters
    String localVarPath = "/user/{username}"
            .replaceAll("\\{username}", apiClient.escapeString(username));

    String localVarAccept = apiClient.selectHeaderAccept();
    String localVarContentType = apiClient.selectHeaderContentType("application/json");
    return apiClient.invokeAPI("UserApi.updateUser", localVarPath, "PUT", new ArrayList<>(), user,
                               new LinkedHashMap<>(), new LinkedHashMap<>(), new LinkedHashMap<>(), localVarAccept, localVarContentType,
                               null, null, false);
  }
}
