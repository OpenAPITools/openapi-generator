package io.swagger.client.api;

import io.swagger.client.CollectionFormats.*;

import retrofit.Callback;
import retrofit.http.*;
import retrofit.mime.*;

import io.swagger.client.model.User;
import java.util.*;

import java.util.*;

public interface UserApi {
  
  /**
   * Create user
   * Sync method
   * This can only be done by the logged in user.
   * @param body Created user object
   * @return Void
   */
  
  @POST("/user")
  Void createUser(
    @Body User body
  );

  /**
   * Create user
   * Async method
   * @param body Created user object
   * @param cb callback method
   * @return void
   */
  
  @POST("/user")
  void createUser(
    @Body User body, Callback<Void> cb
  );
  
  /**
   * Creates list of users with given input array
   * Sync method
   * 
   * @param body List of user object
   * @return Void
   */
  
  @POST("/user/createWithArray")
  Void createUsersWithArrayInput(
    @Body List<User> body
  );

  /**
   * Creates list of users with given input array
   * Async method
   * @param body List of user object
   * @param cb callback method
   * @return void
   */
  
  @POST("/user/createWithArray")
  void createUsersWithArrayInput(
    @Body List<User> body, Callback<Void> cb
  );
  
  /**
   * Creates list of users with given input array
   * Sync method
   * 
   * @param body List of user object
   * @return Void
   */
  
  @POST("/user/createWithList")
  Void createUsersWithListInput(
    @Body List<User> body
  );

  /**
   * Creates list of users with given input array
   * Async method
   * @param body List of user object
   * @param cb callback method
   * @return void
   */
  
  @POST("/user/createWithList")
  void createUsersWithListInput(
    @Body List<User> body, Callback<Void> cb
  );
  
  /**
   * Logs user into the system
   * Sync method
   * 
   * @param username The user name for login
   * @param password The password for login in clear text
   * @return String
   */
  
  @GET("/user/login")
  String loginUser(
    @Query("username") String username, @Query("password") String password
  );

  /**
   * Logs user into the system
   * Async method
   * @param username The user name for login
   * @param password The password for login in clear text
   * @param cb callback method
   * @return void
   */
  
  @GET("/user/login")
  void loginUser(
    @Query("username") String username, @Query("password") String password, Callback<String> cb
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
   * @return void
   */
  
  @GET("/user/logout")
  void logoutUser(
    Callback<Void> cb
  );
  
  /**
   * Get user by user name
   * Sync method
   * 
   * @param username The name that needs to be fetched. Use user1 for testing.
   * @return User
   */
  
  @GET("/user/{username}")
  User getUserByName(
    @Path("username") String username
  );

  /**
   * Get user by user name
   * Async method
   * @param username The name that needs to be fetched. Use user1 for testing.
   * @param cb callback method
   * @return void
   */
  
  @GET("/user/{username}")
  void getUserByName(
    @Path("username") String username, Callback<User> cb
  );
  
  /**
   * Updated user
   * Sync method
   * This can only be done by the logged in user.
   * @param username name that need to be deleted
   * @param body Updated user object
   * @return Void
   */
  
  @PUT("/user/{username}")
  Void updateUser(
    @Path("username") String username, @Body User body
  );

  /**
   * Updated user
   * Async method
   * @param username name that need to be deleted
   * @param body Updated user object
   * @param cb callback method
   * @return void
   */
  
  @PUT("/user/{username}")
  void updateUser(
    @Path("username") String username, @Body User body, Callback<Void> cb
  );
  
  /**
   * Delete user
   * Sync method
   * This can only be done by the logged in user.
   * @param username The name that needs to be deleted
   * @return Void
   */
  
  @DELETE("/user/{username}")
  Void deleteUser(
    @Path("username") String username
  );

  /**
   * Delete user
   * Async method
   * @param username The name that needs to be deleted
   * @param cb callback method
   * @return void
   */
  
  @DELETE("/user/{username}")
  void deleteUser(
    @Path("username") String username, Callback<Void> cb
  );
  
}
