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
import org.springframework.web.reactive.function.client.WebClient.ResponseSpec;
import org.springframework.web.reactive.function.client.WebClientResponseException;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Flux;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")
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
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec createUserRequestCreation(@javax.annotation.Nonnull User user) throws WebClientResponseException {
        Object postBody = user;
        // verify the required parameter 'user' is set
        if (user == null) {
            throw new WebClientResponseException("Missing the required parameter 'user' when calling createUser", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return apiClient.invokeAPI("/user", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Create user
     * This can only be done by the logged in user.
     * <p><b>0</b> - successful operation
     * @param user Created user object
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Void> createUser(@javax.annotation.Nonnull User user) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return createUserRequestCreation(user).bodyToMono(localVarReturnType);
    }

    /**
     * Create user
     * This can only be done by the logged in user.
     * <p><b>0</b> - successful operation
     * @param user Created user object
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<Void>> createUserWithHttpInfo(@javax.annotation.Nonnull User user) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return createUserRequestCreation(user).toEntity(localVarReturnType);
    }

    /**
     * Create user
     * This can only be done by the logged in user.
     * <p><b>0</b> - successful operation
     * @param user Created user object
     * @return ResponseSpec
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec createUserWithResponseSpec(@javax.annotation.Nonnull User user) throws WebClientResponseException {
        return createUserRequestCreation(user);
    }

    /**
     * Creates list of users with given input array
     * 
     * <p><b>0</b> - successful operation
     * @param user List of user object
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec createUsersWithArrayInputRequestCreation(@javax.annotation.Nonnull List<User> user) throws WebClientResponseException {
        Object postBody = user;
        // verify the required parameter 'user' is set
        if (user == null) {
            throw new WebClientResponseException("Missing the required parameter 'user' when calling createUsersWithArrayInput", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return apiClient.invokeAPI("/user/createWithArray", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Creates list of users with given input array
     * 
     * <p><b>0</b> - successful operation
     * @param user List of user object
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Void> createUsersWithArrayInput(@javax.annotation.Nonnull List<User> user) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return createUsersWithArrayInputRequestCreation(user).bodyToMono(localVarReturnType);
    }

    /**
     * Creates list of users with given input array
     * 
     * <p><b>0</b> - successful operation
     * @param user List of user object
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<Void>> createUsersWithArrayInputWithHttpInfo(@javax.annotation.Nonnull List<User> user) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return createUsersWithArrayInputRequestCreation(user).toEntity(localVarReturnType);
    }

    /**
     * Creates list of users with given input array
     * 
     * <p><b>0</b> - successful operation
     * @param user List of user object
     * @return ResponseSpec
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec createUsersWithArrayInputWithResponseSpec(@javax.annotation.Nonnull List<User> user) throws WebClientResponseException {
        return createUsersWithArrayInputRequestCreation(user);
    }

    /**
     * Creates list of users with given input array
     * 
     * <p><b>0</b> - successful operation
     * @param user List of user object
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec createUsersWithListInputRequestCreation(@javax.annotation.Nonnull List<User> user) throws WebClientResponseException {
        Object postBody = user;
        // verify the required parameter 'user' is set
        if (user == null) {
            throw new WebClientResponseException("Missing the required parameter 'user' when calling createUsersWithListInput", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return apiClient.invokeAPI("/user/createWithList", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Creates list of users with given input array
     * 
     * <p><b>0</b> - successful operation
     * @param user List of user object
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Void> createUsersWithListInput(@javax.annotation.Nonnull List<User> user) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return createUsersWithListInputRequestCreation(user).bodyToMono(localVarReturnType);
    }

    /**
     * Creates list of users with given input array
     * 
     * <p><b>0</b> - successful operation
     * @param user List of user object
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<Void>> createUsersWithListInputWithHttpInfo(@javax.annotation.Nonnull List<User> user) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return createUsersWithListInputRequestCreation(user).toEntity(localVarReturnType);
    }

    /**
     * Creates list of users with given input array
     * 
     * <p><b>0</b> - successful operation
     * @param user List of user object
     * @return ResponseSpec
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec createUsersWithListInputWithResponseSpec(@javax.annotation.Nonnull List<User> user) throws WebClientResponseException {
        return createUsersWithListInputRequestCreation(user);
    }

    /**
     * Delete user
     * This can only be done by the logged in user.
     * <p><b>400</b> - Invalid username supplied
     * <p><b>404</b> - User not found
     * @param username The name that needs to be deleted
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec deleteUserRequestCreation(@javax.annotation.Nonnull String username) throws WebClientResponseException {
        Object postBody = null;
        // verify the required parameter 'username' is set
        if (username == null) {
            throw new WebClientResponseException("Missing the required parameter 'username' when calling deleteUser", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        pathParams.put("username", username);

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return apiClient.invokeAPI("/user/{username}", HttpMethod.DELETE, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Delete user
     * This can only be done by the logged in user.
     * <p><b>400</b> - Invalid username supplied
     * <p><b>404</b> - User not found
     * @param username The name that needs to be deleted
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Void> deleteUser(@javax.annotation.Nonnull String username) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return deleteUserRequestCreation(username).bodyToMono(localVarReturnType);
    }

    /**
     * Delete user
     * This can only be done by the logged in user.
     * <p><b>400</b> - Invalid username supplied
     * <p><b>404</b> - User not found
     * @param username The name that needs to be deleted
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<Void>> deleteUserWithHttpInfo(@javax.annotation.Nonnull String username) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return deleteUserRequestCreation(username).toEntity(localVarReturnType);
    }

    /**
     * Delete user
     * This can only be done by the logged in user.
     * <p><b>400</b> - Invalid username supplied
     * <p><b>404</b> - User not found
     * @param username The name that needs to be deleted
     * @return ResponseSpec
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec deleteUserWithResponseSpec(@javax.annotation.Nonnull String username) throws WebClientResponseException {
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
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec getUserByNameRequestCreation(@javax.annotation.Nonnull String username) throws WebClientResponseException {
        Object postBody = null;
        // verify the required parameter 'username' is set
        if (username == null) {
            throw new WebClientResponseException("Missing the required parameter 'username' when calling getUserByName", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        pathParams.put("username", username);

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = { 
            "application/xml", "application/json"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<User> localVarReturnType = new ParameterizedTypeReference<User>() {};
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
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<User> getUserByName(@javax.annotation.Nonnull String username) throws WebClientResponseException {
        ParameterizedTypeReference<User> localVarReturnType = new ParameterizedTypeReference<User>() {};
        return getUserByNameRequestCreation(username).bodyToMono(localVarReturnType);
    }

    /**
     * Get user by user name
     * 
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid username supplied
     * <p><b>404</b> - User not found
     * @param username The name that needs to be fetched. Use user1 for testing.
     * @return ResponseEntity&lt;User&gt;
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<User>> getUserByNameWithHttpInfo(@javax.annotation.Nonnull String username) throws WebClientResponseException {
        ParameterizedTypeReference<User> localVarReturnType = new ParameterizedTypeReference<User>() {};
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
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec getUserByNameWithResponseSpec(@javax.annotation.Nonnull String username) throws WebClientResponseException {
        return getUserByNameRequestCreation(username);
    }

    public class LoginUserRequest {
        private @javax.annotation.Nonnull String username;
        private @javax.annotation.Nonnull String password;

        public LoginUserRequest() {}

        public LoginUserRequest(@javax.annotation.Nonnull String username, @javax.annotation.Nonnull String password) {
            this.username = username;
            this.password = password;
        }

        public @javax.annotation.Nonnull String username() {
            return this.username;
        }
        public LoginUserRequest username(@javax.annotation.Nonnull String username) {
            this.username = username;
            return this;
        }

        public @javax.annotation.Nonnull String password() {
            return this.password;
        }
        public LoginUserRequest password(@javax.annotation.Nonnull String password) {
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
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<String> loginUser(LoginUserRequest requestParameters) throws WebClientResponseException {
        return this.loginUser(requestParameters.username(), requestParameters.password());
    }

    /**
     * Logs user into the system
     * 
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid username/password supplied
     * @param requestParameters The loginUser request parameters as object
     * @return ResponseEntity&lt;String&gt;
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<String>> loginUserWithHttpInfo(LoginUserRequest requestParameters) throws WebClientResponseException {
        return this.loginUserWithHttpInfo(requestParameters.username(), requestParameters.password());
    }

    /**
     * Logs user into the system
     * 
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid username/password supplied
     * @param requestParameters The loginUser request parameters as object
     * @return ResponseSpec
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec loginUserWithResponseSpec(LoginUserRequest requestParameters) throws WebClientResponseException {
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
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec loginUserRequestCreation(@javax.annotation.Nonnull String username, @javax.annotation.Nonnull String password) throws WebClientResponseException {
        Object postBody = null;
        // verify the required parameter 'username' is set
        if (username == null) {
            throw new WebClientResponseException("Missing the required parameter 'username' when calling loginUser", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // verify the required parameter 'password' is set
        if (password == null) {
            throw new WebClientResponseException("Missing the required parameter 'password' when calling loginUser", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "username", username));
        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "password", password));

        final String[] localVarAccepts = { 
            "application/xml", "application/json"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<String>() {};
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
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<String> loginUser(@javax.annotation.Nonnull String username, @javax.annotation.Nonnull String password) throws WebClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<String>() {};
        return loginUserRequestCreation(username, password).bodyToMono(localVarReturnType);
    }

    /**
     * Logs user into the system
     * 
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid username/password supplied
     * @param username The user name for login
     * @param password The password for login in clear text
     * @return ResponseEntity&lt;String&gt;
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<String>> loginUserWithHttpInfo(@javax.annotation.Nonnull String username, @javax.annotation.Nonnull String password) throws WebClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<String>() {};
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
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec loginUserWithResponseSpec(@javax.annotation.Nonnull String username, @javax.annotation.Nonnull String password) throws WebClientResponseException {
        return loginUserRequestCreation(username, password);
    }

    /**
     * Logs out current logged in user session
     * 
     * <p><b>0</b> - successful operation
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec logoutUserRequestCreation() throws WebClientResponseException {
        Object postBody = null;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return apiClient.invokeAPI("/user/logout", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Logs out current logged in user session
     * 
     * <p><b>0</b> - successful operation
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Void> logoutUser() throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return logoutUserRequestCreation().bodyToMono(localVarReturnType);
    }

    /**
     * Logs out current logged in user session
     * 
     * <p><b>0</b> - successful operation
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<Void>> logoutUserWithHttpInfo() throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return logoutUserRequestCreation().toEntity(localVarReturnType);
    }

    /**
     * Logs out current logged in user session
     * 
     * <p><b>0</b> - successful operation
     * @return ResponseSpec
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec logoutUserWithResponseSpec() throws WebClientResponseException {
        return logoutUserRequestCreation();
    }

    public class UpdateUserRequest {
        private @javax.annotation.Nonnull String username;
        private @javax.annotation.Nonnull User user;

        public UpdateUserRequest() {}

        public UpdateUserRequest(@javax.annotation.Nonnull String username, @javax.annotation.Nonnull User user) {
            this.username = username;
            this.user = user;
        }

        public @javax.annotation.Nonnull String username() {
            return this.username;
        }
        public UpdateUserRequest username(@javax.annotation.Nonnull String username) {
            this.username = username;
            return this;
        }

        public @javax.annotation.Nonnull User user() {
            return this.user;
        }
        public UpdateUserRequest user(@javax.annotation.Nonnull User user) {
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
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Void> updateUser(UpdateUserRequest requestParameters) throws WebClientResponseException {
        return this.updateUser(requestParameters.username(), requestParameters.user());
    }

    /**
     * Updated user
     * This can only be done by the logged in user.
     * <p><b>400</b> - Invalid user supplied
     * <p><b>404</b> - User not found
     * @param requestParameters The updateUser request parameters as object
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<Void>> updateUserWithHttpInfo(UpdateUserRequest requestParameters) throws WebClientResponseException {
        return this.updateUserWithHttpInfo(requestParameters.username(), requestParameters.user());
    }

    /**
     * Updated user
     * This can only be done by the logged in user.
     * <p><b>400</b> - Invalid user supplied
     * <p><b>404</b> - User not found
     * @param requestParameters The updateUser request parameters as object
     * @return ResponseSpec
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec updateUserWithResponseSpec(UpdateUserRequest requestParameters) throws WebClientResponseException {
        return this.updateUserWithResponseSpec(requestParameters.username(), requestParameters.user());
    }


    /**
     * Updated user
     * This can only be done by the logged in user.
     * <p><b>400</b> - Invalid user supplied
     * <p><b>404</b> - User not found
     * @param username name that need to be deleted
     * @param user Updated user object
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec updateUserRequestCreation(@javax.annotation.Nonnull String username, @javax.annotation.Nonnull User user) throws WebClientResponseException {
        Object postBody = user;
        // verify the required parameter 'username' is set
        if (username == null) {
            throw new WebClientResponseException("Missing the required parameter 'username' when calling updateUser", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // verify the required parameter 'user' is set
        if (user == null) {
            throw new WebClientResponseException("Missing the required parameter 'user' when calling updateUser", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        pathParams.put("username", username);

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return apiClient.invokeAPI("/user/{username}", HttpMethod.PUT, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Updated user
     * This can only be done by the logged in user.
     * <p><b>400</b> - Invalid user supplied
     * <p><b>404</b> - User not found
     * @param username name that need to be deleted
     * @param user Updated user object
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Void> updateUser(@javax.annotation.Nonnull String username, @javax.annotation.Nonnull User user) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return updateUserRequestCreation(username, user).bodyToMono(localVarReturnType);
    }

    /**
     * Updated user
     * This can only be done by the logged in user.
     * <p><b>400</b> - Invalid user supplied
     * <p><b>404</b> - User not found
     * @param username name that need to be deleted
     * @param user Updated user object
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<Void>> updateUserWithHttpInfo(@javax.annotation.Nonnull String username, @javax.annotation.Nonnull User user) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
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
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec updateUserWithResponseSpec(@javax.annotation.Nonnull String username, @javax.annotation.Nonnull User user) throws WebClientResponseException {
        return updateUserRequestCreation(username, user);
    }
}
