package io.swagger.client.api;

import io.swagger.client.ApiCallback;
import io.swagger.client.ApiClient;
import io.swagger.client.ApiException;
import io.swagger.client.ApiResponse;
import io.swagger.client.Configuration;
import io.swagger.client.Pair;
import io.swagger.client.ProgressRequestBody;
import io.swagger.client.ProgressResponseBody;

import com.google.gson.reflect.TypeToken;

import com.squareup.okhttp.Call;
import com.squareup.okhttp.Interceptor;
import com.squareup.okhttp.Response;

import java.io.IOException;

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
  private Call createUserCall(User body, final ProgressResponseBody.ProgressListener progressListener, final ProgressRequestBody.ProgressRequestListener progressRequestListener) throws ApiException {
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

    if(progressListener != null) {
      apiClient.getHttpClient().networkInterceptors().add(new Interceptor() {
      @Override
      public Response intercept(Interceptor.Chain chain) throws IOException {
        Response originalResponse = chain.proceed(chain.request());
        return originalResponse.newBuilder()
                .body(new ProgressResponseBody(originalResponse.body(), progressListener))
                .build();
        }
      });
    }

    String[] authNames = new String[] {  };
    return apiClient.buildCall(path, "POST", queryParams, postBody, headerParams, formParams, authNames, progressRequestListener);
  }

  /**
   * Create user
   * This can only be done by the logged in user.
   * @param body Created user object
   * @throws ApiException If fail to call the API, e.g. server error or cannot deserialize the response body
   */
  public void createUser(User body) throws ApiException {
    createUserWithHttpInfo(body);
  }

  /**
   * Create user
   * This can only be done by the logged in user.
   * @param body Created user object
   * @return ApiResponse<Void>
   * @throws ApiException If fail to call the API, e.g. server error or cannot deserialize the response body
   */
  public ApiResponse<Void> createUserWithHttpInfo(User body) throws ApiException {
    Call call = createUserCall(body, null, null);
    return apiClient.execute(call);
  }

  /**
   * Create user (asynchronously)
   * This can only be done by the logged in user.
   * @param body Created user object
   * @param callback The callback to be executed when the API call finishes
   * @return The request call
   * @throws ApiException If fail to process the API call, e.g. serializing the request body object
   */
  public Call createUserAsync(User body, final ApiCallback<Void> callback) throws ApiException {

    ProgressResponseBody.ProgressListener progressListener = null;
    ProgressRequestBody.ProgressRequestListener progressRequestListener = null;

    if (callback != null) {
      progressListener = new ProgressResponseBody.ProgressListener() {
        @Override
        public void update(long bytesRead, long contentLength, boolean done) {
          callback.onDownloadProgress(bytesRead, contentLength, done);
        } 
      };

      progressRequestListener = new ProgressRequestBody.ProgressRequestListener() {
        @Override
        public void onRequestProgress(long bytesWritten, long contentLength, boolean done) {
          callback.onUploadProgress(bytesWritten, contentLength, done);
        }
      };
    }

    Call call = createUserCall(body, progressListener, progressRequestListener);
    apiClient.executeAsync(call, callback);
    return call;
  }
  
  /* Build call for createUsersWithArrayInput */
  private Call createUsersWithArrayInputCall(List<User> body, final ProgressResponseBody.ProgressListener progressListener, final ProgressRequestBody.ProgressRequestListener progressRequestListener) throws ApiException {
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

    if(progressListener != null) {
      apiClient.getHttpClient().networkInterceptors().add(new Interceptor() {
      @Override
      public Response intercept(Interceptor.Chain chain) throws IOException {
        Response originalResponse = chain.proceed(chain.request());
        return originalResponse.newBuilder()
                .body(new ProgressResponseBody(originalResponse.body(), progressListener))
                .build();
        }
      });
    }

    String[] authNames = new String[] {  };
    return apiClient.buildCall(path, "POST", queryParams, postBody, headerParams, formParams, authNames, progressRequestListener);
  }

  /**
   * Creates list of users with given input array
   * 
   * @param body List of user object
   * @throws ApiException If fail to call the API, e.g. server error or cannot deserialize the response body
   */
  public void createUsersWithArrayInput(List<User> body) throws ApiException {
    createUsersWithArrayInputWithHttpInfo(body);
  }

  /**
   * Creates list of users with given input array
   * 
   * @param body List of user object
   * @return ApiResponse<Void>
   * @throws ApiException If fail to call the API, e.g. server error or cannot deserialize the response body
   */
  public ApiResponse<Void> createUsersWithArrayInputWithHttpInfo(List<User> body) throws ApiException {
    Call call = createUsersWithArrayInputCall(body, null, null);
    return apiClient.execute(call);
  }

  /**
   * Creates list of users with given input array (asynchronously)
   * 
   * @param body List of user object
   * @param callback The callback to be executed when the API call finishes
   * @return The request call
   * @throws ApiException If fail to process the API call, e.g. serializing the request body object
   */
  public Call createUsersWithArrayInputAsync(List<User> body, final ApiCallback<Void> callback) throws ApiException {

    ProgressResponseBody.ProgressListener progressListener = null;
    ProgressRequestBody.ProgressRequestListener progressRequestListener = null;

    if (callback != null) {
      progressListener = new ProgressResponseBody.ProgressListener() {
        @Override
        public void update(long bytesRead, long contentLength, boolean done) {
          callback.onDownloadProgress(bytesRead, contentLength, done);
        } 
      };

      progressRequestListener = new ProgressRequestBody.ProgressRequestListener() {
        @Override
        public void onRequestProgress(long bytesWritten, long contentLength, boolean done) {
          callback.onUploadProgress(bytesWritten, contentLength, done);
        }
      };
    }

    Call call = createUsersWithArrayInputCall(body, progressListener, progressRequestListener);
    apiClient.executeAsync(call, callback);
    return call;
  }
  
  /* Build call for createUsersWithListInput */
  private Call createUsersWithListInputCall(List<User> body, final ProgressResponseBody.ProgressListener progressListener, final ProgressRequestBody.ProgressRequestListener progressRequestListener) throws ApiException {
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

    if(progressListener != null) {
      apiClient.getHttpClient().networkInterceptors().add(new Interceptor() {
      @Override
      public Response intercept(Interceptor.Chain chain) throws IOException {
        Response originalResponse = chain.proceed(chain.request());
        return originalResponse.newBuilder()
                .body(new ProgressResponseBody(originalResponse.body(), progressListener))
                .build();
        }
      });
    }

    String[] authNames = new String[] {  };
    return apiClient.buildCall(path, "POST", queryParams, postBody, headerParams, formParams, authNames, progressRequestListener);
  }

  /**
   * Creates list of users with given input array
   * 
   * @param body List of user object
   * @throws ApiException If fail to call the API, e.g. server error or cannot deserialize the response body
   */
  public void createUsersWithListInput(List<User> body) throws ApiException {
    createUsersWithListInputWithHttpInfo(body);
  }

  /**
   * Creates list of users with given input array
   * 
   * @param body List of user object
   * @return ApiResponse<Void>
   * @throws ApiException If fail to call the API, e.g. server error or cannot deserialize the response body
   */
  public ApiResponse<Void> createUsersWithListInputWithHttpInfo(List<User> body) throws ApiException {
    Call call = createUsersWithListInputCall(body, null, null);
    return apiClient.execute(call);
  }

  /**
   * Creates list of users with given input array (asynchronously)
   * 
   * @param body List of user object
   * @param callback The callback to be executed when the API call finishes
   * @return The request call
   * @throws ApiException If fail to process the API call, e.g. serializing the request body object
   */
  public Call createUsersWithListInputAsync(List<User> body, final ApiCallback<Void> callback) throws ApiException {

    ProgressResponseBody.ProgressListener progressListener = null;
    ProgressRequestBody.ProgressRequestListener progressRequestListener = null;

    if (callback != null) {
      progressListener = new ProgressResponseBody.ProgressListener() {
        @Override
        public void update(long bytesRead, long contentLength, boolean done) {
          callback.onDownloadProgress(bytesRead, contentLength, done);
        } 
      };

      progressRequestListener = new ProgressRequestBody.ProgressRequestListener() {
        @Override
        public void onRequestProgress(long bytesWritten, long contentLength, boolean done) {
          callback.onUploadProgress(bytesWritten, contentLength, done);
        }
      };
    }

    Call call = createUsersWithListInputCall(body, progressListener, progressRequestListener);
    apiClient.executeAsync(call, callback);
    return call;
  }
  
  /* Build call for loginUser */
  private Call loginUserCall(String username, String password, final ProgressResponseBody.ProgressListener progressListener, final ProgressRequestBody.ProgressRequestListener progressRequestListener) throws ApiException {
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

    if(progressListener != null) {
      apiClient.getHttpClient().networkInterceptors().add(new Interceptor() {
      @Override
      public Response intercept(Interceptor.Chain chain) throws IOException {
        Response originalResponse = chain.proceed(chain.request());
        return originalResponse.newBuilder()
                .body(new ProgressResponseBody(originalResponse.body(), progressListener))
                .build();
        }
      });
    }

    String[] authNames = new String[] {  };
    return apiClient.buildCall(path, "GET", queryParams, postBody, headerParams, formParams, authNames, progressRequestListener);
  }

  /**
   * Logs user into the system
   * 
   * @param username The user name for login
   * @param password The password for login in clear text
   * @return String
   * @throws ApiException If fail to call the API, e.g. server error or cannot deserialize the response body
   */
  public String loginUser(String username, String password) throws ApiException {
    ApiResponse<String> resp = loginUserWithHttpInfo(username, password);
    return resp.getData();
  }

  /**
   * Logs user into the system
   * 
   * @param username The user name for login
   * @param password The password for login in clear text
   * @return ApiResponse<String>
   * @throws ApiException If fail to call the API, e.g. server error or cannot deserialize the response body
   */
  public ApiResponse<String> loginUserWithHttpInfo(String username, String password) throws ApiException {
    Call call = loginUserCall(username, password, null, null);
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
   * @throws ApiException If fail to process the API call, e.g. serializing the request body object
   */
  public Call loginUserAsync(String username, String password, final ApiCallback<String> callback) throws ApiException {

    ProgressResponseBody.ProgressListener progressListener = null;
    ProgressRequestBody.ProgressRequestListener progressRequestListener = null;

    if (callback != null) {
      progressListener = new ProgressResponseBody.ProgressListener() {
        @Override
        public void update(long bytesRead, long contentLength, boolean done) {
          callback.onDownloadProgress(bytesRead, contentLength, done);
        } 
      };

      progressRequestListener = new ProgressRequestBody.ProgressRequestListener() {
        @Override
        public void onRequestProgress(long bytesWritten, long contentLength, boolean done) {
          callback.onUploadProgress(bytesWritten, contentLength, done);
        }
      };
    }

    Call call = loginUserCall(username, password, progressListener, progressRequestListener);
    Type returnType = new TypeToken<String>(){}.getType();
    apiClient.executeAsync(call, returnType, callback);
    return call;
  }
  
  /* Build call for logoutUser */
  private Call logoutUserCall(final ProgressResponseBody.ProgressListener progressListener, final ProgressRequestBody.ProgressRequestListener progressRequestListener) throws ApiException {
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

    if(progressListener != null) {
      apiClient.getHttpClient().networkInterceptors().add(new Interceptor() {
      @Override
      public Response intercept(Interceptor.Chain chain) throws IOException {
        Response originalResponse = chain.proceed(chain.request());
        return originalResponse.newBuilder()
                .body(new ProgressResponseBody(originalResponse.body(), progressListener))
                .build();
        }
      });
    }

    String[] authNames = new String[] {  };
    return apiClient.buildCall(path, "GET", queryParams, postBody, headerParams, formParams, authNames, progressRequestListener);
  }

  /**
   * Logs out current logged in user session
   * 
   * @throws ApiException If fail to call the API, e.g. server error or cannot deserialize the response body
   */
  public void logoutUser() throws ApiException {
    logoutUserWithHttpInfo();
  }

  /**
   * Logs out current logged in user session
   * 
   * @return ApiResponse<Void>
   * @throws ApiException If fail to call the API, e.g. server error or cannot deserialize the response body
   */
  public ApiResponse<Void> logoutUserWithHttpInfo() throws ApiException {
    Call call = logoutUserCall(null, null);
    return apiClient.execute(call);
  }

  /**
   * Logs out current logged in user session (asynchronously)
   * 
   * @param callback The callback to be executed when the API call finishes
   * @return The request call
   * @throws ApiException If fail to process the API call, e.g. serializing the request body object
   */
  public Call logoutUserAsync(final ApiCallback<Void> callback) throws ApiException {

    ProgressResponseBody.ProgressListener progressListener = null;
    ProgressRequestBody.ProgressRequestListener progressRequestListener = null;

    if (callback != null) {
      progressListener = new ProgressResponseBody.ProgressListener() {
        @Override
        public void update(long bytesRead, long contentLength, boolean done) {
          callback.onDownloadProgress(bytesRead, contentLength, done);
        } 
      };

      progressRequestListener = new ProgressRequestBody.ProgressRequestListener() {
        @Override
        public void onRequestProgress(long bytesWritten, long contentLength, boolean done) {
          callback.onUploadProgress(bytesWritten, contentLength, done);
        }
      };
    }

    Call call = logoutUserCall(progressListener, progressRequestListener);
    apiClient.executeAsync(call, callback);
    return call;
  }
  
  /* Build call for getUserByName */
  private Call getUserByNameCall(String username, final ProgressResponseBody.ProgressListener progressListener, final ProgressRequestBody.ProgressRequestListener progressRequestListener) throws ApiException {
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

    if(progressListener != null) {
      apiClient.getHttpClient().networkInterceptors().add(new Interceptor() {
      @Override
      public Response intercept(Interceptor.Chain chain) throws IOException {
        Response originalResponse = chain.proceed(chain.request());
        return originalResponse.newBuilder()
                .body(new ProgressResponseBody(originalResponse.body(), progressListener))
                .build();
        }
      });
    }

    String[] authNames = new String[] {  };
    return apiClient.buildCall(path, "GET", queryParams, postBody, headerParams, formParams, authNames, progressRequestListener);
  }

  /**
   * Get user by user name
   * 
   * @param username The name that needs to be fetched. Use user1 for testing.
   * @return User
   * @throws ApiException If fail to call the API, e.g. server error or cannot deserialize the response body
   */
  public User getUserByName(String username) throws ApiException {
    ApiResponse<User> resp = getUserByNameWithHttpInfo(username);
    return resp.getData();
  }

  /**
   * Get user by user name
   * 
   * @param username The name that needs to be fetched. Use user1 for testing.
   * @return ApiResponse<User>
   * @throws ApiException If fail to call the API, e.g. server error or cannot deserialize the response body
   */
  public ApiResponse<User> getUserByNameWithHttpInfo(String username) throws ApiException {
    Call call = getUserByNameCall(username, null, null);
    Type returnType = new TypeToken<User>(){}.getType();
    return apiClient.execute(call, returnType);
  }

  /**
   * Get user by user name (asynchronously)
   * 
   * @param username The name that needs to be fetched. Use user1 for testing.
   * @param callback The callback to be executed when the API call finishes
   * @return The request call
   * @throws ApiException If fail to process the API call, e.g. serializing the request body object
   */
  public Call getUserByNameAsync(String username, final ApiCallback<User> callback) throws ApiException {

    ProgressResponseBody.ProgressListener progressListener = null;
    ProgressRequestBody.ProgressRequestListener progressRequestListener = null;

    if (callback != null) {
      progressListener = new ProgressResponseBody.ProgressListener() {
        @Override
        public void update(long bytesRead, long contentLength, boolean done) {
          callback.onDownloadProgress(bytesRead, contentLength, done);
        } 
      };

      progressRequestListener = new ProgressRequestBody.ProgressRequestListener() {
        @Override
        public void onRequestProgress(long bytesWritten, long contentLength, boolean done) {
          callback.onUploadProgress(bytesWritten, contentLength, done);
        }
      };
    }

    Call call = getUserByNameCall(username, progressListener, progressRequestListener);
    Type returnType = new TypeToken<User>(){}.getType();
    apiClient.executeAsync(call, returnType, callback);
    return call;
  }
  
  /* Build call for updateUser */
  private Call updateUserCall(String username, User body, final ProgressResponseBody.ProgressListener progressListener, final ProgressRequestBody.ProgressRequestListener progressRequestListener) throws ApiException {
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

    if(progressListener != null) {
      apiClient.getHttpClient().networkInterceptors().add(new Interceptor() {
      @Override
      public Response intercept(Interceptor.Chain chain) throws IOException {
        Response originalResponse = chain.proceed(chain.request());
        return originalResponse.newBuilder()
                .body(new ProgressResponseBody(originalResponse.body(), progressListener))
                .build();
        }
      });
    }

    String[] authNames = new String[] {  };
    return apiClient.buildCall(path, "PUT", queryParams, postBody, headerParams, formParams, authNames, progressRequestListener);
  }

  /**
   * Updated user
   * This can only be done by the logged in user.
   * @param username name that need to be deleted
   * @param body Updated user object
   * @throws ApiException If fail to call the API, e.g. server error or cannot deserialize the response body
   */
  public void updateUser(String username, User body) throws ApiException {
    updateUserWithHttpInfo(username, body);
  }

  /**
   * Updated user
   * This can only be done by the logged in user.
   * @param username name that need to be deleted
   * @param body Updated user object
   * @return ApiResponse<Void>
   * @throws ApiException If fail to call the API, e.g. server error or cannot deserialize the response body
   */
  public ApiResponse<Void> updateUserWithHttpInfo(String username, User body) throws ApiException {
    Call call = updateUserCall(username, body, null, null);
    return apiClient.execute(call);
  }

  /**
   * Updated user (asynchronously)
   * This can only be done by the logged in user.
   * @param username name that need to be deleted
   * @param body Updated user object
   * @param callback The callback to be executed when the API call finishes
   * @return The request call
   * @throws ApiException If fail to process the API call, e.g. serializing the request body object
   */
  public Call updateUserAsync(String username, User body, final ApiCallback<Void> callback) throws ApiException {

    ProgressResponseBody.ProgressListener progressListener = null;
    ProgressRequestBody.ProgressRequestListener progressRequestListener = null;

    if (callback != null) {
      progressListener = new ProgressResponseBody.ProgressListener() {
        @Override
        public void update(long bytesRead, long contentLength, boolean done) {
          callback.onDownloadProgress(bytesRead, contentLength, done);
        } 
      };

      progressRequestListener = new ProgressRequestBody.ProgressRequestListener() {
        @Override
        public void onRequestProgress(long bytesWritten, long contentLength, boolean done) {
          callback.onUploadProgress(bytesWritten, contentLength, done);
        }
      };
    }

    Call call = updateUserCall(username, body, progressListener, progressRequestListener);
    apiClient.executeAsync(call, callback);
    return call;
  }
  
  /* Build call for deleteUser */
  private Call deleteUserCall(String username, final ProgressResponseBody.ProgressListener progressListener, final ProgressRequestBody.ProgressRequestListener progressRequestListener) throws ApiException {
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

    if(progressListener != null) {
      apiClient.getHttpClient().networkInterceptors().add(new Interceptor() {
      @Override
      public Response intercept(Interceptor.Chain chain) throws IOException {
        Response originalResponse = chain.proceed(chain.request());
        return originalResponse.newBuilder()
                .body(new ProgressResponseBody(originalResponse.body(), progressListener))
                .build();
        }
      });
    }

    String[] authNames = new String[] {  };
    return apiClient.buildCall(path, "DELETE", queryParams, postBody, headerParams, formParams, authNames, progressRequestListener);
  }

  /**
   * Delete user
   * This can only be done by the logged in user.
   * @param username The name that needs to be deleted
   * @throws ApiException If fail to call the API, e.g. server error or cannot deserialize the response body
   */
  public void deleteUser(String username) throws ApiException {
    deleteUserWithHttpInfo(username);
  }

  /**
   * Delete user
   * This can only be done by the logged in user.
   * @param username The name that needs to be deleted
   * @return ApiResponse<Void>
   * @throws ApiException If fail to call the API, e.g. server error or cannot deserialize the response body
   */
  public ApiResponse<Void> deleteUserWithHttpInfo(String username) throws ApiException {
    Call call = deleteUserCall(username, null, null);
    return apiClient.execute(call);
  }

  /**
   * Delete user (asynchronously)
   * This can only be done by the logged in user.
   * @param username The name that needs to be deleted
   * @param callback The callback to be executed when the API call finishes
   * @return The request call
   * @throws ApiException If fail to process the API call, e.g. serializing the request body object
   */
  public Call deleteUserAsync(String username, final ApiCallback<Void> callback) throws ApiException {

    ProgressResponseBody.ProgressListener progressListener = null;
    ProgressRequestBody.ProgressRequestListener progressRequestListener = null;

    if (callback != null) {
      progressListener = new ProgressResponseBody.ProgressListener() {
        @Override
        public void update(long bytesRead, long contentLength, boolean done) {
          callback.onDownloadProgress(bytesRead, contentLength, done);
        } 
      };

      progressRequestListener = new ProgressRequestBody.ProgressRequestListener() {
        @Override
        public void onRequestProgress(long bytesWritten, long contentLength, boolean done) {
          callback.onUploadProgress(bytesWritten, contentLength, done);
        }
      };
    }

    Call call = deleteUserCall(username, progressListener, progressRequestListener);
    apiClient.executeAsync(call, callback);
    return call;
  }
  
}
