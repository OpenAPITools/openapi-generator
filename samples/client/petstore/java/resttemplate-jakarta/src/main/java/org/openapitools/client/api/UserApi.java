package org.openapitools.client.api;

import org.openapitools.client.ApiClient;

import java.time.OffsetDateTime;
import org.openapitools.client.model.User;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.core.io.FileSystemResource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;

@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen")
public class UserApi {
    private ApiClient apiClient;

    public UserApi() {
        this(new ApiClient());
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

    /**
     * Create user
     * This can only be done by the logged in user.
     * <p><b>0</b> - successful operation
     * @param user Created user object (required)
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public void createUser(User user) throws RestClientException {
        createUserWithHttpInfo(user);
    }

    /**
     * Create user
     * This can only be done by the logged in user.
     * <p><b>0</b> - successful operation
     * @param user Created user object (required)
     * @return ResponseEntity&lt;Void&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> createUserWithHttpInfo(User user) throws RestClientException {
        Object localVarPostBody = user;
        
        // verify the required parameter 'user' is set
        if (user == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'user' when calling createUser");
        }
        

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders localVarHeaderParams = new HttpHeaders();
        final MultiValueMap<String, String> localVarCookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> localVarFormParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = {  };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
         };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] { "api_key" };

        ParameterizedTypeReference<Void> localReturnType = new ParameterizedTypeReference<Void>() {};
        return apiClient.invokeAPI("/user", HttpMethod.POST, Collections.<String, Object>emptyMap(), localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localReturnType);
    }
    /**
     * Creates list of users with given input array
     * 
     * <p><b>0</b> - successful operation
     * @param user List of user object (required)
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public void createUsersWithArrayInput(List<User> user) throws RestClientException {
        createUsersWithArrayInputWithHttpInfo(user);
    }

    /**
     * Creates list of users with given input array
     * 
     * <p><b>0</b> - successful operation
     * @param user List of user object (required)
     * @return ResponseEntity&lt;Void&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> createUsersWithArrayInputWithHttpInfo(List<User> user) throws RestClientException {
        Object localVarPostBody = user;
        
        // verify the required parameter 'user' is set
        if (user == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'user' when calling createUsersWithArrayInput");
        }
        

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders localVarHeaderParams = new HttpHeaders();
        final MultiValueMap<String, String> localVarCookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> localVarFormParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = {  };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
         };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] { "api_key" };

        ParameterizedTypeReference<Void> localReturnType = new ParameterizedTypeReference<Void>() {};
        return apiClient.invokeAPI("/user/createWithArray", HttpMethod.POST, Collections.<String, Object>emptyMap(), localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localReturnType);
    }
    /**
     * Creates list of users with given input array
     * 
     * <p><b>0</b> - successful operation
     * @param user List of user object (required)
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public void createUsersWithListInput(List<User> user) throws RestClientException {
        createUsersWithListInputWithHttpInfo(user);
    }

    /**
     * Creates list of users with given input array
     * 
     * <p><b>0</b> - successful operation
     * @param user List of user object (required)
     * @return ResponseEntity&lt;Void&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> createUsersWithListInputWithHttpInfo(List<User> user) throws RestClientException {
        Object localVarPostBody = user;
        
        // verify the required parameter 'user' is set
        if (user == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'user' when calling createUsersWithListInput");
        }
        

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders localVarHeaderParams = new HttpHeaders();
        final MultiValueMap<String, String> localVarCookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> localVarFormParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = {  };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
         };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] { "api_key" };

        ParameterizedTypeReference<Void> localReturnType = new ParameterizedTypeReference<Void>() {};
        return apiClient.invokeAPI("/user/createWithList", HttpMethod.POST, Collections.<String, Object>emptyMap(), localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localReturnType);
    }
    /**
     * Delete user
     * This can only be done by the logged in user.
     * <p><b>400</b> - Invalid username supplied
     * <p><b>404</b> - User not found
     * @param username The name that needs to be deleted (required)
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public void deleteUser(String username) throws RestClientException {
        deleteUserWithHttpInfo(username);
    }

    /**
     * Delete user
     * This can only be done by the logged in user.
     * <p><b>400</b> - Invalid username supplied
     * <p><b>404</b> - User not found
     * @param username The name that needs to be deleted (required)
     * @return ResponseEntity&lt;Void&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> deleteUserWithHttpInfo(String username) throws RestClientException {
        Object localVarPostBody = null;
        
        // verify the required parameter 'username' is set
        if (username == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'username' when calling deleteUser");
        }
        
        // create path and map variables
        final Map<String, Object> uriVariables = new HashMap<String, Object>();
        uriVariables.put("username", username);

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders localVarHeaderParams = new HttpHeaders();
        final MultiValueMap<String, String> localVarCookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> localVarFormParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = {  };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = {  };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] { "api_key" };

        ParameterizedTypeReference<Void> localReturnType = new ParameterizedTypeReference<Void>() {};
        return apiClient.invokeAPI("/user/{username}", HttpMethod.DELETE, uriVariables, localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localReturnType);
    }
    /**
     * Get user by user name
     * 
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid username supplied
     * <p><b>404</b> - User not found
     * @param username The name that needs to be fetched. Use user1 for testing. (required)
     * @return User
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public User getUserByName(String username) throws RestClientException {
        return getUserByNameWithHttpInfo(username).getBody();
    }

    /**
     * Get user by user name
     * 
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid username supplied
     * <p><b>404</b> - User not found
     * @param username The name that needs to be fetched. Use user1 for testing. (required)
     * @return ResponseEntity&lt;User&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<User> getUserByNameWithHttpInfo(String username) throws RestClientException {
        Object localVarPostBody = null;
        
        // verify the required parameter 'username' is set
        if (username == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'username' when calling getUserByName");
        }
        
        // create path and map variables
        final Map<String, Object> uriVariables = new HashMap<String, Object>();
        uriVariables.put("username", username);

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders localVarHeaderParams = new HttpHeaders();
        final MultiValueMap<String, String> localVarCookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> localVarFormParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = { 
            "application/xml", "application/json"
         };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = {  };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<User> localReturnType = new ParameterizedTypeReference<User>() {};
        return apiClient.invokeAPI("/user/{username}", HttpMethod.GET, uriVariables, localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localReturnType);
    }
    /**
     * Logs user into the system
     * 
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid username/password supplied
     * @param username The user name for login (required)
     * @param password The password for login in clear text (required)
     * @return String
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public String loginUser(String username, String password) throws RestClientException {
        return loginUserWithHttpInfo(username, password).getBody();
    }

    /**
     * Logs user into the system
     * 
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid username/password supplied
     * @param username The user name for login (required)
     * @param password The password for login in clear text (required)
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> loginUserWithHttpInfo(String username, String password) throws RestClientException {
        Object localVarPostBody = null;
        
        // verify the required parameter 'username' is set
        if (username == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'username' when calling loginUser");
        }
        
        // verify the required parameter 'password' is set
        if (password == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'password' when calling loginUser");
        }
        

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders localVarHeaderParams = new HttpHeaders();
        final MultiValueMap<String, String> localVarCookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> localVarFormParams = new LinkedMultiValueMap<String, Object>();

        localVarQueryParams.putAll(apiClient.parameterToMultiValueMap(null, "username", username));
        localVarQueryParams.putAll(apiClient.parameterToMultiValueMap(null, "password", password));


        final String[] localVarAccepts = { 
            "application/xml", "application/json"
         };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = {  };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<String> localReturnType = new ParameterizedTypeReference<String>() {};
        return apiClient.invokeAPI("/user/login", HttpMethod.GET, Collections.<String, Object>emptyMap(), localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localReturnType);
    }
    /**
     * Logs out current logged in user session
     * 
     * <p><b>0</b> - successful operation
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public void logoutUser() throws RestClientException {
        logoutUserWithHttpInfo();
    }

    /**
     * Logs out current logged in user session
     * 
     * <p><b>0</b> - successful operation
     * @return ResponseEntity&lt;Void&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> logoutUserWithHttpInfo() throws RestClientException {
        Object localVarPostBody = null;
        

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders localVarHeaderParams = new HttpHeaders();
        final MultiValueMap<String, String> localVarCookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> localVarFormParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = {  };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = {  };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] { "api_key" };

        ParameterizedTypeReference<Void> localReturnType = new ParameterizedTypeReference<Void>() {};
        return apiClient.invokeAPI("/user/logout", HttpMethod.GET, Collections.<String, Object>emptyMap(), localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localReturnType);
    }
    /**
     * Updated user
     * This can only be done by the logged in user.
     * <p><b>400</b> - Invalid user supplied
     * <p><b>404</b> - User not found
     * @param username name that need to be deleted (required)
     * @param user Updated user object (required)
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public void updateUser(String username, User user) throws RestClientException {
        updateUserWithHttpInfo(username, user);
    }

    /**
     * Updated user
     * This can only be done by the logged in user.
     * <p><b>400</b> - Invalid user supplied
     * <p><b>404</b> - User not found
     * @param username name that need to be deleted (required)
     * @param user Updated user object (required)
     * @return ResponseEntity&lt;Void&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> updateUserWithHttpInfo(String username, User user) throws RestClientException {
        Object localVarPostBody = user;
        
        // verify the required parameter 'username' is set
        if (username == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'username' when calling updateUser");
        }
        
        // verify the required parameter 'user' is set
        if (user == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'user' when calling updateUser");
        }
        
        // create path and map variables
        final Map<String, Object> uriVariables = new HashMap<String, Object>();
        uriVariables.put("username", username);

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders localVarHeaderParams = new HttpHeaders();
        final MultiValueMap<String, String> localVarCookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> localVarFormParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = {  };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
         };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] { "api_key" };

        ParameterizedTypeReference<Void> localReturnType = new ParameterizedTypeReference<Void>() {};
        return apiClient.invokeAPI("/user/{username}", HttpMethod.PUT, uriVariables, localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localReturnType);
    }
}
