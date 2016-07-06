package io.swagger.client.api;

import io.swagger.client.CollectionFormats.*;

import rx.Observable;

import retrofit2.http.*;

import okhttp3.RequestBody;

import io.swagger.client.model.User;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public interface UserApi {
  /**
   * Create user
   * This can only be done by the logged in user.
   * @param body Created user object (required)
   * @return Call&lt;Object&gt;
   */
  
  @POST("/user")
  Observable<Object> createUser(
    @Body User body
  );

  /**
   * Creates list of users with given input array
   * 
   * @param body List of user object (required)
   * @return Call&lt;Object&gt;
   */
  
  @POST("/user/createWithArray")
  Observable<Object> createUsersWithArrayInput(
    @Body List<User> body
  );

  /**
   * Creates list of users with given input array
   * 
   * @param body List of user object (required)
   * @return Call&lt;Object&gt;
   */
  
  @POST("/user/createWithList")
  Observable<Object> createUsersWithListInput(
    @Body List<User> body
  );

  /**
   * Delete user
   * This can only be done by the logged in user.
   * @param username The name that needs to be deleted (required)
   * @return Call&lt;Object&gt;
   */
  
  @DELETE("/user/{username}")
  Observable<Object> deleteUser(
    @Path("username") String username
  );

  /**
   * Get user by user name
   * 
   * @param username The name that needs to be fetched. Use user1 for testing.  (required)
   * @return Call&lt;User&gt;
   */
  
  @GET("/user/{username}")
  Observable<User> getUserByName(
    @Path("username") String username
  );

  /**
   * Logs user into the system
   * 
   * @param username The user name for login (required)
   * @param password The password for login in clear text (required)
   * @return Call&lt;String&gt;
   */
  
  @GET("/user/login")
  Observable<String> loginUser(
    @Query("username") String username, @Query("password") String password
  );

  /**
   * Logs out current logged in user session
   * 
   * @return Call&lt;Object&gt;
   */
  
  @GET("/user/logout")
  Observable<Object> logoutUser();
    

  /**
   * Updated user
   * This can only be done by the logged in user.
   * @param username name that need to be deleted (required)
   * @param body Updated user object (required)
   * @return Call&lt;Object&gt;
   */
  
  @PUT("/user/{username}")
  Observable<Object> updateUser(
    @Path("username") String username, @Body User body
  );

}
