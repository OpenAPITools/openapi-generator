package org.openapitools.client.api;

import org.openapitools.client.ApiClient;

import java.time.OffsetDateTime;
import org.openapitools.client.model.User;

import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Arrays;
import java.util.stream.Collectors;

import org.springframework.core.io.FileSystemResource;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestClient.ResponseSpec;
import org.springframework.web.client.RestClientResponseException;

@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")
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
     * @param user Created user object
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec createUserRequestCreation(@jakarta.annotation.Nonnull User user) throws RestClientResponseException {
        Object postBody = user;
        // verify the required parameter 'user' is set
        if (user == null) {
            throw new RestClientResponseException("Missing the required parameter 'user' when calling createUser", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/user", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Create user
     * This can only be done by the logged in user.
     * <p><b>0</b> - successful operation
     * @param user Created user object
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public void createUser(@jakarta.annotation.Nonnull User user) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        createUserRequestCreation(user).body(localVarReturnType);
    }

    /**
     * Create user
     * This can only be done by the logged in user.
     * <p><b>0</b> - successful operation
     * @param user Created user object
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> createUserWithHttpInfo(@jakarta.annotation.Nonnull User user) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return createUserRequestCreation(user).toEntity(localVarReturnType);
    }

    /**
     * Create user
     * This can only be done by the logged in user.
     * <p><b>0</b> - successful operation
     * @param user Created user object
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec createUserWithResponseSpec(@jakarta.annotation.Nonnull User user) throws RestClientResponseException {
        return createUserRequestCreation(user);
    }

    /**
     * Creates list of users with given input array
     * 
     * <p><b>0</b> - successful operation
     * @param user List of user object
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec createUsersWithArrayInputRequestCreation(@jakarta.annotation.Nonnull List<User> user) throws RestClientResponseException {
        Object postBody = user;
        // verify the required parameter 'user' is set
        if (user == null) {
            throw new RestClientResponseException("Missing the required parameter 'user' when calling createUsersWithArrayInput", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/user/createWithArray", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Creates list of users with given input array
     * 
     * <p><b>0</b> - successful operation
     * @param user List of user object
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public void createUsersWithArrayInput(@jakarta.annotation.Nonnull List<User> user) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        createUsersWithArrayInputRequestCreation(user).body(localVarReturnType);
    }

    /**
     * Creates list of users with given input array
     * 
     * <p><b>0</b> - successful operation
     * @param user List of user object
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> createUsersWithArrayInputWithHttpInfo(@jakarta.annotation.Nonnull List<User> user) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return createUsersWithArrayInputRequestCreation(user).toEntity(localVarReturnType);
    }

    /**
     * Creates list of users with given input array
     * 
     * <p><b>0</b> - successful operation
     * @param user List of user object
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec createUsersWithArrayInputWithResponseSpec(@jakarta.annotation.Nonnull List<User> user) throws RestClientResponseException {
        return createUsersWithArrayInputRequestCreation(user);
    }

    /**
     * Creates list of users with given input array
     * 
     * <p><b>0</b> - successful operation
     * @param user List of user object
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec createUsersWithListInputRequestCreation(@jakarta.annotation.Nonnull List<User> user) throws RestClientResponseException {
        Object postBody = user;
        // verify the required parameter 'user' is set
        if (user == null) {
            throw new RestClientResponseException("Missing the required parameter 'user' when calling createUsersWithListInput", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/user/createWithList", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Creates list of users with given input array
     * 
     * <p><b>0</b> - successful operation
     * @param user List of user object
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public void createUsersWithListInput(@jakarta.annotation.Nonnull List<User> user) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        createUsersWithListInputRequestCreation(user).body(localVarReturnType);
    }

    /**
     * Creates list of users with given input array
     * 
     * <p><b>0</b> - successful operation
     * @param user List of user object
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> createUsersWithListInputWithHttpInfo(@jakarta.annotation.Nonnull List<User> user) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return createUsersWithListInputRequestCreation(user).toEntity(localVarReturnType);
    }

    /**
     * Creates list of users with given input array
     * 
     * <p><b>0</b> - successful operation
     * @param user List of user object
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec createUsersWithListInputWithResponseSpec(@jakarta.annotation.Nonnull List<User> user) throws RestClientResponseException {
        return createUsersWithListInputRequestCreation(user);
    }

    /**
     * Delete user
     * This can only be done by the logged in user.
     * <p><b>400</b> - Invalid username supplied
     * <p><b>404</b> - User not found
     * @param username The name that needs to be deleted
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec deleteUserRequestCreation(@jakarta.annotation.Nonnull String username) throws RestClientResponseException {
        Object postBody = null;
        // verify the required parameter 'username' is set
        if (username == null) {
            throw new RestClientResponseException("Missing the required parameter 'username' when calling deleteUser", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        pathParams.put("username", username);

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/user/{username}", HttpMethod.DELETE, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Delete user
     * This can only be done by the logged in user.
     * <p><b>400</b> - Invalid username supplied
     * <p><b>404</b> - User not found
     * @param username The name that needs to be deleted
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public void deleteUser(@jakarta.annotation.Nonnull String username) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        deleteUserRequestCreation(username).body(localVarReturnType);
    }

    /**
     * Delete user
     * This can only be done by the logged in user.
     * <p><b>400</b> - Invalid username supplied
     * <p><b>404</b> - User not found
     * @param username The name that needs to be deleted
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> deleteUserWithHttpInfo(@jakarta.annotation.Nonnull String username) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return deleteUserRequestCreation(username).toEntity(localVarReturnType);
    }

    /**
     * Delete user
     * This can only be done by the logged in user.
     * <p><b>400</b> - Invalid username supplied
     * <p><b>404</b> - User not found
     * @param username The name that needs to be deleted
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec deleteUserWithResponseSpec(@jakarta.annotation.Nonnull String username) throws RestClientResponseException {
        return deleteUserRequestCreation(username);
    }

    /**
     * Get user by user name
     * 
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid username supplied
     * <p><b>404</b> - User not found
     * @param username The name that needs to be fetched. Use user1 for testing.
     * @return User
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec getUserByNameRequestCreation(@jakarta.annotation.Nonnull String username) throws RestClientResponseException {
        Object postBody = null;
        // verify the required parameter 'username' is set
        if (username == null) {
            throw new RestClientResponseException("Missing the required parameter 'username' when calling getUserByName", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        pathParams.put("username", username);

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = { 
            "application/xml", "application/json"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<User> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/user/{username}", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Get user by user name
     * 
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid username supplied
     * <p><b>404</b> - User not found
     * @param username The name that needs to be fetched. Use user1 for testing.
     * @return User
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public User getUserByName(@jakarta.annotation.Nonnull String username) throws RestClientResponseException {
        ParameterizedTypeReference<User> localVarReturnType = new ParameterizedTypeReference<>() {};
        return getUserByNameRequestCreation(username).body(localVarReturnType);
    }

    /**
     * Get user by user name
     * 
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid username supplied
     * <p><b>404</b> - User not found
     * @param username The name that needs to be fetched. Use user1 for testing.
     * @return ResponseEntity&lt;User&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<User> getUserByNameWithHttpInfo(@jakarta.annotation.Nonnull String username) throws RestClientResponseException {
        ParameterizedTypeReference<User> localVarReturnType = new ParameterizedTypeReference<>() {};
        return getUserByNameRequestCreation(username).toEntity(localVarReturnType);
    }

    /**
     * Get user by user name
     * 
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid username supplied
     * <p><b>404</b> - User not found
     * @param username The name that needs to be fetched. Use user1 for testing.
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec getUserByNameWithResponseSpec(@jakarta.annotation.Nonnull String username) throws RestClientResponseException {
        return getUserByNameRequestCreation(username);
    }

    public static class LoginUserRequest {
        private @jakarta.annotation.Nonnull String username;
        private @jakarta.annotation.Nonnull String password;

        public LoginUserRequest() {}

        public LoginUserRequest(@jakarta.annotation.Nonnull String username, @jakarta.annotation.Nonnull String password) {
            this.username = username;
            this.password = password;
        }

        public @jakarta.annotation.Nonnull String username() {
            return this.username;
        }
        public LoginUserRequest username(@jakarta.annotation.Nonnull String username) {
            this.username = username;
            return this;
        }

        public @jakarta.annotation.Nonnull String password() {
            return this.password;
        }
        public LoginUserRequest password(@jakarta.annotation.Nonnull String password) {
            this.password = password;
            return this;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) {
                return true;
            }
            if (o == null || getClass() != o.getClass()) {
                return false;
            }
            LoginUserRequest request = (LoginUserRequest) o;
            return Objects.equals(this.username, request.username()) &&
                Objects.equals(this.password, request.password());
        }

        @Override
        public int hashCode() {
            return Objects.hash(username, password);
        }
    }

    /**
     * Logs user into the system
     * 
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid username/password supplied
     * @param requestParameters The loginUser request parameters as object
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public String loginUser(LoginUserRequest requestParameters) throws RestClientResponseException {
        return this.loginUser(requestParameters.username(), requestParameters.password());
    }

    /**
     * Logs user into the system
     * 
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid username/password supplied
     * @param requestParameters The loginUser request parameters as object
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> loginUserWithHttpInfo(LoginUserRequest requestParameters) throws RestClientResponseException {
        return this.loginUserWithHttpInfo(requestParameters.username(), requestParameters.password());
    }

    /**
     * Logs user into the system
     * 
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid username/password supplied
     * @param requestParameters The loginUser request parameters as object
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec loginUserWithResponseSpec(LoginUserRequest requestParameters) throws RestClientResponseException {
        return this.loginUserWithResponseSpec(requestParameters.username(), requestParameters.password());
    }

    /**
     * Logs user into the system
     * 
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid username/password supplied
     * @param username The user name for login
     * @param password The password for login in clear text
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec loginUserRequestCreation(@jakarta.annotation.Nonnull String username, @jakarta.annotation.Nonnull String password) throws RestClientResponseException {
        Object postBody = null;
        // verify the required parameter 'username' is set
        if (username == null) {
            throw new RestClientResponseException("Missing the required parameter 'username' when calling loginUser", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // verify the required parameter 'password' is set
        if (password == null) {
            throw new RestClientResponseException("Missing the required parameter 'password' when calling loginUser", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "username", username));
        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "password", password));

        final String[] localVarAccepts = { 
            "application/xml", "application/json"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/user/login", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Logs user into the system
     * 
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid username/password supplied
     * @param username The user name for login
     * @param password The password for login in clear text
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public String loginUser(@jakarta.annotation.Nonnull String username, @jakarta.annotation.Nonnull String password) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return loginUserRequestCreation(username, password).body(localVarReturnType);
    }

    /**
     * Logs user into the system
     * 
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid username/password supplied
     * @param username The user name for login
     * @param password The password for login in clear text
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> loginUserWithHttpInfo(@jakarta.annotation.Nonnull String username, @jakarta.annotation.Nonnull String password) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return loginUserRequestCreation(username, password).toEntity(localVarReturnType);
    }

    /**
     * Logs user into the system
     * 
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid username/password supplied
     * @param username The user name for login
     * @param password The password for login in clear text
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec loginUserWithResponseSpec(@jakarta.annotation.Nonnull String username, @jakarta.annotation.Nonnull String password) throws RestClientResponseException {
        return loginUserRequestCreation(username, password);
    }

    /**
     * Logs out current logged in user session
     * 
     * <p><b>0</b> - successful operation
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec logoutUserRequestCreation() throws RestClientResponseException {
        Object postBody = null;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/user/logout", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Logs out current logged in user session
     * 
     * <p><b>0</b> - successful operation
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public void logoutUser() throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        logoutUserRequestCreation().body(localVarReturnType);
    }

    /**
     * Logs out current logged in user session
     * 
     * <p><b>0</b> - successful operation
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> logoutUserWithHttpInfo() throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return logoutUserRequestCreation().toEntity(localVarReturnType);
    }

    /**
     * Logs out current logged in user session
     * 
     * <p><b>0</b> - successful operation
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec logoutUserWithResponseSpec() throws RestClientResponseException {
        return logoutUserRequestCreation();
    }

    public static class UpdateUserRequest {
        private @jakarta.annotation.Nonnull String username;
        private @jakarta.annotation.Nonnull User user;

        public UpdateUserRequest() {}

        public UpdateUserRequest(@jakarta.annotation.Nonnull String username, @jakarta.annotation.Nonnull User user) {
            this.username = username;
            this.user = user;
        }

        public @jakarta.annotation.Nonnull String username() {
            return this.username;
        }
        public UpdateUserRequest username(@jakarta.annotation.Nonnull String username) {
            this.username = username;
            return this;
        }

        public @jakarta.annotation.Nonnull User user() {
            return this.user;
        }
        public UpdateUserRequest user(@jakarta.annotation.Nonnull User user) {
            this.user = user;
            return this;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) {
                return true;
            }
            if (o == null || getClass() != o.getClass()) {
                return false;
            }
            UpdateUserRequest request = (UpdateUserRequest) o;
            return Objects.equals(this.username, request.username()) &&
                Objects.equals(this.user, request.user());
        }

        @Override
        public int hashCode() {
            return Objects.hash(username, user);
        }
    }

    /**
     * Updated user
     * This can only be done by the logged in user.
     * <p><b>400</b> - Invalid user supplied
     * <p><b>404</b> - User not found
     * @param requestParameters The updateUser request parameters as object
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public void updateUser(UpdateUserRequest requestParameters) throws RestClientResponseException {
        this.updateUser(requestParameters.username(), requestParameters.user());
    }

    /**
     * Updated user
     * This can only be done by the logged in user.
     * <p><b>400</b> - Invalid user supplied
     * <p><b>404</b> - User not found
     * @param requestParameters The updateUser request parameters as object
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> updateUserWithHttpInfo(UpdateUserRequest requestParameters) throws RestClientResponseException {
        return this.updateUserWithHttpInfo(requestParameters.username(), requestParameters.user());
    }

    /**
     * Updated user
     * This can only be done by the logged in user.
     * <p><b>400</b> - Invalid user supplied
     * <p><b>404</b> - User not found
     * @param requestParameters The updateUser request parameters as object
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec updateUserWithResponseSpec(UpdateUserRequest requestParameters) throws RestClientResponseException {
        return this.updateUserWithResponseSpec(requestParameters.username(), requestParameters.user());
    }

    /**
     * Updated user
     * This can only be done by the logged in user.
     * <p><b>400</b> - Invalid user supplied
     * <p><b>404</b> - User not found
     * @param username name that need to be deleted
     * @param user Updated user object
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec updateUserRequestCreation(@jakarta.annotation.Nonnull String username, @jakarta.annotation.Nonnull User user) throws RestClientResponseException {
        Object postBody = user;
        // verify the required parameter 'username' is set
        if (username == null) {
            throw new RestClientResponseException("Missing the required parameter 'username' when calling updateUser", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // verify the required parameter 'user' is set
        if (user == null) {
            throw new RestClientResponseException("Missing the required parameter 'user' when calling updateUser", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        pathParams.put("username", username);

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/user/{username}", HttpMethod.PUT, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Updated user
     * This can only be done by the logged in user.
     * <p><b>400</b> - Invalid user supplied
     * <p><b>404</b> - User not found
     * @param username name that need to be deleted
     * @param user Updated user object
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public void updateUser(@jakarta.annotation.Nonnull String username, @jakarta.annotation.Nonnull User user) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        updateUserRequestCreation(username, user).body(localVarReturnType);
    }

    /**
     * Updated user
     * This can only be done by the logged in user.
     * <p><b>400</b> - Invalid user supplied
     * <p><b>404</b> - User not found
     * @param username name that need to be deleted
     * @param user Updated user object
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> updateUserWithHttpInfo(@jakarta.annotation.Nonnull String username, @jakarta.annotation.Nonnull User user) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return updateUserRequestCreation(username, user).toEntity(localVarReturnType);
    }

    /**
     * Updated user
     * This can only be done by the logged in user.
     * <p><b>400</b> - Invalid user supplied
     * <p><b>404</b> - User not found
     * @param username name that need to be deleted
     * @param user Updated user object
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec updateUserWithResponseSpec(@jakarta.annotation.Nonnull String username, @jakarta.annotation.Nonnull User user) throws RestClientResponseException {
        return updateUserRequestCreation(username, user);
    }
}
