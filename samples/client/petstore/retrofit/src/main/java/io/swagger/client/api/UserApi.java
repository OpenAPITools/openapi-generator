package io.swagger.client.api;

import io.swagger.client.model.*;

import retrofit.http.*;
import retrofit.mime.*;
import java.util.*;

import io.swagger.client.model.User;
import java.util.*;

public interface UserApi {
  
  /**
   * Create user
   * This can only be done by the logged in user.
   * @param body Created user object
   * @return Void
   */
  
  @POST("/user")  
  Void createUser(
    @Body User body
  );  
  
  /**
   * Creates list of users with given input array
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
   * 
   * @param body List of user object
   * @return Void
   */
  
  @POST("/user/createWithList")  
  Void createUsersWithListInput(
    @Body List<User> body
  );  
  
  /**
   * Logs user into the system
   * 
   * @param username The user name for login
   * @param password The password for login in clear text
   * @return String
   */
  
  @GET("/user/login")  
  String loginUser(
    @Query("username") String username,@Query("password") String password
  );  
  
  /**
   * Logs out current logged in user session
   * 
   * @return Void
   */
  
  @GET("/user/logout")  
  Void logoutUser();
      
  
  /**
   * Get user by user name
   * 
   * @param username The name that needs to be fetched. Use user1 for testing. 
   * @return User
   */
  
  @GET("/user/{username}")  
  User getUserByName(
    @Path("username") String username
  );  
  
  /**
   * Updated user
   * This can only be done by the logged in user.
   * @param username name that need to be deleted
   * @param body Updated user object
   * @return Void
   */
  
  @PUT("/user/{username}")  
  Void updateUser(
    @Path("username") String username,@Body User body
  );  
  
  /**
   * Delete user
   * This can only be done by the logged in user.
   * @param username The name that needs to be deleted
   * @return Void
   */
  
  @DELETE("/user/{username}")  
  Void deleteUser(
    @Path("username") String username
  );  
  
}
