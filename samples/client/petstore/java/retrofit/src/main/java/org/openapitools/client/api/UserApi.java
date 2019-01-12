package org.openapitools.client.api;

import org.openapitools.client.CollectionFormats.*;

import retrofit.Callback;
import retrofit.http.*;
import retrofit.mime.*;

import org.openapitools.client.model.User;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public interface UserApi {
  /**
   * Create user
   * Sync method
   * This can only be done by the logged in user.
   * @param body Created user object (required)
   * @return Void
   */
  
  @POST("/user")
  Void createUser(
    @retrofit.http.Body User body
  );

  /**
   * Create user
   * Async method
   * @param body Created user object (required)
   * @param cb callback method
   */
  
  @POST("/user")
  void createUser(
    @retrofit.http.Body User body, Callback<Void> cb
  );
  /**
   * Creates list of users with given input array
   * Sync method
   * 
   * @param body List of user object (required)
   * @return Void
   */
  
  @POST("/user/createWithArray")
  Void createUsersWithArrayInput(
    @retrofit.http.Body List<User> body
  );

  /**
   * Creates list of users with given input array
   * Async method
   * @param body List of user object (required)
   * @param cb callback method
   */
  
  @POST("/user/createWithArray")
  void createUsersWithArrayInput(
    @retrofit.http.Body List<User> body, Callback<Void> cb
  );
  /**
   * Creates list of users with given input array
   * Sync method
   * 
   * @param body List of user object (required)
   * @return Void
   */
  
  @POST("/user/createWithList")
  Void createUsersWithListInput(
    @retrofit.http.Body List<User> body
  );

  /**
   * Creates list of users with given input array
   * Async method
   * @param body List of user object (required)
   * @param cb callback method
   */
  
  @POST("/user/createWithList")
  void createUsersWithListInput(
    @retrofit.http.Body List<User> body, Callback<Void> cb
  );
  /**
   * Delete user
   * Sync method
   * This can only be done by the logged in user.
   * @param username The name that needs to be deleted (required)
   * @return Void
   */
  
  @DELETE("/user/{username}")
  Void deleteUser(
    @retrofit.http.Path("username") String username
  );

  /**
   * Delete user
   * Async method
   * @param username The name that needs to be deleted (required)
   * @param cb callback method
   */
  
  @DELETE("/user/{username}")
  void deleteUser(
    @retrofit.http.Path("username") String username, Callback<Void> cb
  );
  /**
   * Get user by user name
   * Sync method
   * 
   * @param username The name that needs to be fetched. Use user1 for testing. (required)
   * @return User
   */
  
  @GET("/user/{username}")
  User getUserByName(
    @retrofit.http.Path("username") String username
  );

  /**
   * Get user by user name
   * Async method
   * @param username The name that needs to be fetched. Use user1 for testing. (required)
   * @param cb callback method
   */
  
  @GET("/user/{username}")
  void getUserByName(
    @retrofit.http.Path("username") String username, Callback<User> cb
  );
  /**
   * Logs user into the system
   * Sync method
   * 
   * @param username The user name for login (required)
   * @param password The password for login in clear text (required)
   * @return String
   */
  
  @GET("/user/login")
  String loginUser(
    @retrofit.http.Query("username") String username, @retrofit.http.Query("password") String password
  );

  /**
   * Logs user into the system
   * Async method
   * @param username The user name for login (required)
   * @param password The password for login in clear text (required)
   * @param cb callback method
   */
  
  @GET("/user/login")
  void loginUser(
    @retrofit.http.Query("username") String username, @retrofit.http.Query("password") String password, Callback<String> cb
  );
  /**
   * Logs out current logged in user session
   * Sync method
   * 
   * @return Void
   */
  
  @GET("/user/logout")
  Void logoutUser();
    

  /**
   * Logs out current logged in user session
   * Async method
   * @param cb callback method
   */
  
  @GET("/user/logout")
  void logoutUser(
    Callback<Void> cb
  );
  /**
   * Updated user
   * Sync method
   * This can only be done by the logged in user.
   * @param username name that need to be deleted (required)
   * @param body Updated user object (required)
   * @return Void
   */
  
  @PUT("/user/{username}")
  Void updateUser(
    @retrofit.http.Path("username") String username, @retrofit.http.Body User body
  );

  /**
   * Updated user
   * Async method
   * @param username name that need to be deleted (required)
   * @param body Updated user object (required)
   * @param cb callback method
   */
  
  @PUT("/user/{username}")
  void updateUser(
    @retrofit.http.Path("username") String username, @retrofit.http.Body User body, Callback<Void> cb
  );
}
