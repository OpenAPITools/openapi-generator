package io.swagger.client.api;

import io.swagger.client.ApiException;
import io.swagger.client.ApiClient;
import io.swagger.client.Configuration;
import io.swagger.client.Pair;
import io.swagger.client.TypeRef;

import io.swagger.client.model.User;
import java.util.*;

import java.util.*;
import feign.*;

@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaClientCodegen", date = "2015-12-01T16:10:23.565+08:00")
public interface UserApi extends io.swagger.client.ApiClient.Api {


  /**
   * Create user
   * This can only be done by the logged in user.
   * @param body Created user object
   * @return void
   */
  @RequestLine("POST /user")
  @Headers({
    
  })
  void createUser (@Param("body") User body) throws ApiException;
  
  /**
   * Creates list of users with given input array
   * 
   * @param body List of user object
   * @return void
   */
  @RequestLine("POST /user/createWithArray")
  @Headers({
    
  })
  void createUsersWithArrayInput (@Param("body") List<User> body) throws ApiException;
  
  /**
   * Creates list of users with given input array
   * 
   * @param body List of user object
   * @return void
   */
  @RequestLine("POST /user/createWithList")
  @Headers({
    
  })
  void createUsersWithListInput (@Param("body") List<User> body) throws ApiException;
  
  /**
   * Logs user into the system
   * 
   * @param username The user name for login
   * @param password The password for login in clear text
   * @return String
   */
  @RequestLine("GET /user/login?username={username}&password={password}")
  @Headers({
    
  })
  String loginUser (@Param("username") String username, @Param("password") String password) throws ApiException;
  
  /**
   * Logs out current logged in user session
   * 
   * @return void
   */
  @RequestLine("GET /user/logout")
  @Headers({
    
  })
  void logoutUser () throws ApiException;
  
  /**
   * Get user by user name
   * 
   * @param username The name that needs to be fetched. Use user1 for testing.
   * @return User
   */
  @RequestLine("GET /user/{username}")
  @Headers({
    
  })
  User getUserByName (@Param("username") String username) throws ApiException;
  
  /**
   * Updated user
   * This can only be done by the logged in user.
   * @param username name that need to be deleted
   * @param body Updated user object
   * @return void
   */
  @RequestLine("PUT /user/{username}")
  @Headers({
    
  })
  void updateUser (@Param("username") String username, @Param("body") User body) throws ApiException;
  
  /**
   * Delete user
   * This can only be done by the logged in user.
   * @param username The name that needs to be deleted
   * @return void
   */
  @RequestLine("DELETE /user/{username}")
  @Headers({
    
  })
  void deleteUser (@Param("username") String username) throws ApiException;
  
}
