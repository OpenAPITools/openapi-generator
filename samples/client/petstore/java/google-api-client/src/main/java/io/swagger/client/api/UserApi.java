package io.swagger.client.api;

import io.swagger.client.ApiClient;

import io.swagger.client.model.User;

import com.fasterxml.jackson.core.type.TypeReference;
import com.google.api.client.http.GenericUrl;
import com.google.api.client.http.HttpContent;
import com.google.api.client.http.HttpMethods;
import com.google.api.client.http.HttpResponse;

import javax.ws.rs.core.UriBuilder;
import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.List;
import java.util.ArrayList;


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
    * @param body Created user object
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public void createUser(User body) throws IOException {
        createUserForHttpResponse(body);
    }

  /**
    * Create user
    * This can only be done by the logged in user.
    * <p><b>0</b> - successful operation
    * @param body Created user object
    * @param params Map of query params. A collection will be interpreted as passing in multiple instances of the same query param.
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public void createUser(User body, Map<String, Object> params) throws IOException {
        createUserForHttpResponse(body, params);
    }

    public HttpResponse createUserForHttpResponse(User body) throws IOException {
        // verify the required parameter 'body' is set
        if (body == null) {
            throw new IllegalArgumentException("Missing the required parameter 'body' when calling createUser");
        }
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/user");

        String url = uriBuilder.build().toString();
        GenericUrl genericUrl = new GenericUrl(url);

        HttpContent content = body == null ? null : apiClient.new JacksonJsonHttpContent(body);
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.POST, genericUrl, content).execute();
    }

    public HttpResponse createUserForHttpResponse(User body, Map<String, Object> params) throws IOException {
        // verify the required parameter 'body' is set
        if (body == null) {
            throw new IllegalArgumentException("Missing the required parameter 'body' when calling createUser");
        }
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/user");

        // Copy the params argument if present, to allow passing in immutable maps
        Map<String, Object> allParams = params == null ? new HashMap<String, Object>() : new HashMap<String, Object>(params);

        for (Map.Entry<String, Object> entry: allParams.entrySet()) {
            String key = entry.getKey();
            Object value = entry.getValue();

            if (key != null && value != null) {
                if (value instanceof Collection) {
                    uriBuilder = uriBuilder.queryParam(key, ((Collection) value).toArray());
                } else {
                    uriBuilder = uriBuilder.queryParam(key, value);
                }
            }
        }

        String url = uriBuilder.build().toString();
        GenericUrl genericUrl = new GenericUrl(url);

        HttpContent content = body == null ? null : apiClient.new JacksonJsonHttpContent(body);
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.POST, genericUrl, content).execute();
    }


  /**
    * Creates list of users with given input array
    * 
    * <p><b>0</b> - successful operation
    * @param body List of user object
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public void createUsersWithArrayInput(List<User> body) throws IOException {
        createUsersWithArrayInputForHttpResponse(body);
    }

  /**
    * Creates list of users with given input array
    * 
    * <p><b>0</b> - successful operation
    * @param body List of user object
    * @param params Map of query params. A collection will be interpreted as passing in multiple instances of the same query param.
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public void createUsersWithArrayInput(List<User> body, Map<String, Object> params) throws IOException {
        createUsersWithArrayInputForHttpResponse(body, params);
    }

    public HttpResponse createUsersWithArrayInputForHttpResponse(List<User> body) throws IOException {
        // verify the required parameter 'body' is set
        if (body == null) {
            throw new IllegalArgumentException("Missing the required parameter 'body' when calling createUsersWithArrayInput");
        }
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/user/createWithArray");

        String url = uriBuilder.build().toString();
        GenericUrl genericUrl = new GenericUrl(url);

        HttpContent content = body == null ? null : apiClient.new JacksonJsonHttpContent(body);
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.POST, genericUrl, content).execute();
    }

    public HttpResponse createUsersWithArrayInputForHttpResponse(List<User> body, Map<String, Object> params) throws IOException {
        // verify the required parameter 'body' is set
        if (body == null) {
            throw new IllegalArgumentException("Missing the required parameter 'body' when calling createUsersWithArrayInput");
        }
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/user/createWithArray");

        // Copy the params argument if present, to allow passing in immutable maps
        Map<String, Object> allParams = params == null ? new HashMap<String, Object>() : new HashMap<String, Object>(params);

        for (Map.Entry<String, Object> entry: allParams.entrySet()) {
            String key = entry.getKey();
            Object value = entry.getValue();

            if (key != null && value != null) {
                if (value instanceof Collection) {
                    uriBuilder = uriBuilder.queryParam(key, ((Collection) value).toArray());
                } else {
                    uriBuilder = uriBuilder.queryParam(key, value);
                }
            }
        }

        String url = uriBuilder.build().toString();
        GenericUrl genericUrl = new GenericUrl(url);

        HttpContent content = body == null ? null : apiClient.new JacksonJsonHttpContent(body);
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.POST, genericUrl, content).execute();
    }


  /**
    * Creates list of users with given input array
    * 
    * <p><b>0</b> - successful operation
    * @param body List of user object
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public void createUsersWithListInput(List<User> body) throws IOException {
        createUsersWithListInputForHttpResponse(body);
    }

  /**
    * Creates list of users with given input array
    * 
    * <p><b>0</b> - successful operation
    * @param body List of user object
    * @param params Map of query params. A collection will be interpreted as passing in multiple instances of the same query param.
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public void createUsersWithListInput(List<User> body, Map<String, Object> params) throws IOException {
        createUsersWithListInputForHttpResponse(body, params);
    }

    public HttpResponse createUsersWithListInputForHttpResponse(List<User> body) throws IOException {
        // verify the required parameter 'body' is set
        if (body == null) {
            throw new IllegalArgumentException("Missing the required parameter 'body' when calling createUsersWithListInput");
        }
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/user/createWithList");

        String url = uriBuilder.build().toString();
        GenericUrl genericUrl = new GenericUrl(url);

        HttpContent content = body == null ? null : apiClient.new JacksonJsonHttpContent(body);
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.POST, genericUrl, content).execute();
    }

    public HttpResponse createUsersWithListInputForHttpResponse(List<User> body, Map<String, Object> params) throws IOException {
        // verify the required parameter 'body' is set
        if (body == null) {
            throw new IllegalArgumentException("Missing the required parameter 'body' when calling createUsersWithListInput");
        }
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/user/createWithList");

        // Copy the params argument if present, to allow passing in immutable maps
        Map<String, Object> allParams = params == null ? new HashMap<String, Object>() : new HashMap<String, Object>(params);

        for (Map.Entry<String, Object> entry: allParams.entrySet()) {
            String key = entry.getKey();
            Object value = entry.getValue();

            if (key != null && value != null) {
                if (value instanceof Collection) {
                    uriBuilder = uriBuilder.queryParam(key, ((Collection) value).toArray());
                } else {
                    uriBuilder = uriBuilder.queryParam(key, value);
                }
            }
        }

        String url = uriBuilder.build().toString();
        GenericUrl genericUrl = new GenericUrl(url);

        HttpContent content = body == null ? null : apiClient.new JacksonJsonHttpContent(body);
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.POST, genericUrl, content).execute();
    }


  /**
    * Delete user
    * This can only be done by the logged in user.
    * <p><b>400</b> - Invalid username supplied
    * <p><b>404</b> - User not found
    * @param username The name that needs to be deleted
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public void deleteUser(String username) throws IOException {
        deleteUserForHttpResponse(username);
    }

  /**
    * Delete user
    * This can only be done by the logged in user.
    * <p><b>400</b> - Invalid username supplied
    * <p><b>404</b> - User not found
    * @param username The name that needs to be deleted
    * @param params Map of query params. A collection will be interpreted as passing in multiple instances of the same query param.
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public void deleteUser(String username, Map<String, Object> params) throws IOException {
        deleteUserForHttpResponse(username, params);
    }

    public HttpResponse deleteUserForHttpResponse(String username) throws IOException {
        // verify the required parameter 'username' is set
        if (username == null) {
            throw new IllegalArgumentException("Missing the required parameter 'username' when calling deleteUser");
        }
        // create a map of path variables
        final Map<String, Object> uriVariables = new HashMap<String, Object>();
        uriVariables.put("username", username);
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/user/{username}");

        String url = uriBuilder.buildFromMap(uriVariables).toString();
        GenericUrl genericUrl = new GenericUrl(url);

        HttpContent content = null;
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.DELETE, genericUrl, content).execute();
    }

    public HttpResponse deleteUserForHttpResponse(String username, Map<String, Object> params) throws IOException {
        // verify the required parameter 'username' is set
        if (username == null) {
            throw new IllegalArgumentException("Missing the required parameter 'username' when calling deleteUser");
        }
        // create a map of path variables
        final Map<String, Object> uriVariables = new HashMap<String, Object>();
        uriVariables.put("username", username);
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/user/{username}");

        // Copy the params argument if present, to allow passing in immutable maps
        Map<String, Object> allParams = params == null ? new HashMap<String, Object>() : new HashMap<String, Object>(params);

        for (Map.Entry<String, Object> entry: allParams.entrySet()) {
            String key = entry.getKey();
            Object value = entry.getValue();

            if (key != null && value != null) {
                if (value instanceof Collection) {
                    uriBuilder = uriBuilder.queryParam(key, ((Collection) value).toArray());
                } else {
                    uriBuilder = uriBuilder.queryParam(key, value);
                }
            }
        }

        String url = uriBuilder.buildFromMap(uriVariables).toString();
        GenericUrl genericUrl = new GenericUrl(url);

        HttpContent content = null;
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.DELETE, genericUrl, content).execute();
    }


  /**
    * Get user by user name
    * 
    * <p><b>200</b> - successful operation
    * <p><b>400</b> - Invalid username supplied
    * <p><b>404</b> - User not found
    * @param username The name that needs to be fetched. Use user1 for testing. 
    * @return User
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public User getUserByName(String username) throws IOException {
        HttpResponse response = getUserByNameForHttpResponse(username);
        TypeReference typeRef = new TypeReference<User>() {};
        return apiClient.getObjectMapper().readValue(response.getContent(), typeRef);
    }

  /**
    * Get user by user name
    * 
    * <p><b>200</b> - successful operation
    * <p><b>400</b> - Invalid username supplied
    * <p><b>404</b> - User not found
    * @param username The name that needs to be fetched. Use user1 for testing. 
    * @param params Map of query params. A collection will be interpreted as passing in multiple instances of the same query param.
    * @return User
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public User getUserByName(String username, Map<String, Object> params) throws IOException {
        HttpResponse response = getUserByNameForHttpResponse(username, params);
        TypeReference typeRef = new TypeReference<User>() {};
        return apiClient.getObjectMapper().readValue(response.getContent(), typeRef);
    }

    public HttpResponse getUserByNameForHttpResponse(String username) throws IOException {
        // verify the required parameter 'username' is set
        if (username == null) {
            throw new IllegalArgumentException("Missing the required parameter 'username' when calling getUserByName");
        }
        // create a map of path variables
        final Map<String, Object> uriVariables = new HashMap<String, Object>();
        uriVariables.put("username", username);
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/user/{username}");

        String url = uriBuilder.buildFromMap(uriVariables).toString();
        GenericUrl genericUrl = new GenericUrl(url);

        HttpContent content = null;
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.GET, genericUrl, content).execute();
    }

    public HttpResponse getUserByNameForHttpResponse(String username, Map<String, Object> params) throws IOException {
        // verify the required parameter 'username' is set
        if (username == null) {
            throw new IllegalArgumentException("Missing the required parameter 'username' when calling getUserByName");
        }
        // create a map of path variables
        final Map<String, Object> uriVariables = new HashMap<String, Object>();
        uriVariables.put("username", username);
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/user/{username}");

        // Copy the params argument if present, to allow passing in immutable maps
        Map<String, Object> allParams = params == null ? new HashMap<String, Object>() : new HashMap<String, Object>(params);

        for (Map.Entry<String, Object> entry: allParams.entrySet()) {
            String key = entry.getKey();
            Object value = entry.getValue();

            if (key != null && value != null) {
                if (value instanceof Collection) {
                    uriBuilder = uriBuilder.queryParam(key, ((Collection) value).toArray());
                } else {
                    uriBuilder = uriBuilder.queryParam(key, value);
                }
            }
        }

        String url = uriBuilder.buildFromMap(uriVariables).toString();
        GenericUrl genericUrl = new GenericUrl(url);

        HttpContent content = null;
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.GET, genericUrl, content).execute();
    }


  /**
    * Logs user into the system
    * 
    * <p><b>200</b> - successful operation
    * <p><b>400</b> - Invalid username/password supplied
    * @param username The user name for login
    * @param password The password for login in clear text
    * @return String
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public String loginUser(String username, String password) throws IOException {
        HttpResponse response = loginUserForHttpResponse(username, password);
        TypeReference typeRef = new TypeReference<String>() {};
        return apiClient.getObjectMapper().readValue(response.getContent(), typeRef);
    }

  /**
    * Logs user into the system
    * 
    * <p><b>200</b> - successful operation
    * <p><b>400</b> - Invalid username/password supplied
    * @param username The user name for login
    * @param password The password for login in clear text
    * @param params Map of query params. A collection will be interpreted as passing in multiple instances of the same query param.
    * @return String
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public String loginUser(String username, String password, Map<String, Object> params) throws IOException {
        HttpResponse response = loginUserForHttpResponse(username, password, params);
        TypeReference typeRef = new TypeReference<String>() {};
        return apiClient.getObjectMapper().readValue(response.getContent(), typeRef);
    }

    public HttpResponse loginUserForHttpResponse(String username, String password) throws IOException {
        // verify the required parameter 'username' is set
        if (username == null) {
            throw new IllegalArgumentException("Missing the required parameter 'username' when calling loginUser");
        }// verify the required parameter 'password' is set
        if (password == null) {
            throw new IllegalArgumentException("Missing the required parameter 'password' when calling loginUser");
        }
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/user/login");
        if (username != null) {
            uriBuilder = uriBuilder.queryParam("username", username);
        }        if (password != null) {
            uriBuilder = uriBuilder.queryParam("password", password);
        }

        String url = uriBuilder.build().toString();
        GenericUrl genericUrl = new GenericUrl(url);

        HttpContent content = null;
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.GET, genericUrl, content).execute();
    }

    public HttpResponse loginUserForHttpResponse(String username, String password, Map<String, Object> params) throws IOException {
        // verify the required parameter 'username' is set
        if (username == null) {
            throw new IllegalArgumentException("Missing the required parameter 'username' when calling loginUser");
        }// verify the required parameter 'password' is set
        if (password == null) {
            throw new IllegalArgumentException("Missing the required parameter 'password' when calling loginUser");
        }
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/user/login");

        // Copy the params argument if present, to allow passing in immutable maps
        Map<String, Object> allParams = params == null ? new HashMap<String, Object>() : new HashMap<String, Object>(params);
        // Add the required query param 'username' to the map of query params
        allParams.put("username", username);
        // Add the required query param 'password' to the map of query params
        allParams.put("password", password);

        for (Map.Entry<String, Object> entry: allParams.entrySet()) {
            String key = entry.getKey();
            Object value = entry.getValue();

            if (key != null && value != null) {
                if (value instanceof Collection) {
                    uriBuilder = uriBuilder.queryParam(key, ((Collection) value).toArray());
                } else {
                    uriBuilder = uriBuilder.queryParam(key, value);
                }
            }
        }

        String url = uriBuilder.build().toString();
        GenericUrl genericUrl = new GenericUrl(url);

        HttpContent content = null;
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.GET, genericUrl, content).execute();
    }


  /**
    * Logs out current logged in user session
    * 
    * <p><b>0</b> - successful operation
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public void logoutUser() throws IOException {
        logoutUserForHttpResponse();
    }

  /**
    * Logs out current logged in user session
    * 
    * <p><b>0</b> - successful operation
    * @param params Map of query params. A collection will be interpreted as passing in multiple instances of the same query param.
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public void logoutUser(Map<String, Object> params) throws IOException {
        logoutUserForHttpResponse(params);
    }

    public HttpResponse logoutUserForHttpResponse() throws IOException {
        
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/user/logout");

        String url = uriBuilder.build().toString();
        GenericUrl genericUrl = new GenericUrl(url);

        HttpContent content = null;
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.GET, genericUrl, content).execute();
    }

    public HttpResponse logoutUserForHttpResponse(Map<String, Object> params) throws IOException {
        
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/user/logout");

        // Copy the params argument if present, to allow passing in immutable maps
        Map<String, Object> allParams = params == null ? new HashMap<String, Object>() : new HashMap<String, Object>(params);

        for (Map.Entry<String, Object> entry: allParams.entrySet()) {
            String key = entry.getKey();
            Object value = entry.getValue();

            if (key != null && value != null) {
                if (value instanceof Collection) {
                    uriBuilder = uriBuilder.queryParam(key, ((Collection) value).toArray());
                } else {
                    uriBuilder = uriBuilder.queryParam(key, value);
                }
            }
        }

        String url = uriBuilder.build().toString();
        GenericUrl genericUrl = new GenericUrl(url);

        HttpContent content = null;
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.GET, genericUrl, content).execute();
    }


  /**
    * Updated user
    * This can only be done by the logged in user.
    * <p><b>400</b> - Invalid user supplied
    * <p><b>404</b> - User not found
    * @param username name that need to be deleted
    * @param body Updated user object
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public void updateUser(String username, User body) throws IOException {
        updateUserForHttpResponse(username, body);
    }

  /**
    * Updated user
    * This can only be done by the logged in user.
    * <p><b>400</b> - Invalid user supplied
    * <p><b>404</b> - User not found
    * @param username name that need to be deleted
    * @param body Updated user object
    * @param params Map of query params. A collection will be interpreted as passing in multiple instances of the same query param.
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public void updateUser(String username, User body, Map<String, Object> params) throws IOException {
        updateUserForHttpResponse(username, body, params);
    }

    public HttpResponse updateUserForHttpResponse(String username, User body) throws IOException {
        // verify the required parameter 'username' is set
        if (username == null) {
            throw new IllegalArgumentException("Missing the required parameter 'username' when calling updateUser");
        }// verify the required parameter 'body' is set
        if (body == null) {
            throw new IllegalArgumentException("Missing the required parameter 'body' when calling updateUser");
        }
        // create a map of path variables
        final Map<String, Object> uriVariables = new HashMap<String, Object>();
        uriVariables.put("username", username);
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/user/{username}");

        String url = uriBuilder.buildFromMap(uriVariables).toString();
        GenericUrl genericUrl = new GenericUrl(url);

        HttpContent content = body == null ? null : apiClient.new JacksonJsonHttpContent(body);
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.PUT, genericUrl, content).execute();
    }

    public HttpResponse updateUserForHttpResponse(String username, User body, Map<String, Object> params) throws IOException {
        // verify the required parameter 'username' is set
        if (username == null) {
            throw new IllegalArgumentException("Missing the required parameter 'username' when calling updateUser");
        }// verify the required parameter 'body' is set
        if (body == null) {
            throw new IllegalArgumentException("Missing the required parameter 'body' when calling updateUser");
        }
        // create a map of path variables
        final Map<String, Object> uriVariables = new HashMap<String, Object>();
        uriVariables.put("username", username);
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/user/{username}");

        // Copy the params argument if present, to allow passing in immutable maps
        Map<String, Object> allParams = params == null ? new HashMap<String, Object>() : new HashMap<String, Object>(params);

        for (Map.Entry<String, Object> entry: allParams.entrySet()) {
            String key = entry.getKey();
            Object value = entry.getValue();

            if (key != null && value != null) {
                if (value instanceof Collection) {
                    uriBuilder = uriBuilder.queryParam(key, ((Collection) value).toArray());
                } else {
                    uriBuilder = uriBuilder.queryParam(key, value);
                }
            }
        }

        String url = uriBuilder.buildFromMap(uriVariables).toString();
        GenericUrl genericUrl = new GenericUrl(url);

        HttpContent content = body == null ? null : apiClient.new JacksonJsonHttpContent(body);
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.PUT, genericUrl, content).execute();
    }


}
