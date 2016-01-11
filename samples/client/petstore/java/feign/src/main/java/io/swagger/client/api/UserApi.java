package io.swagger.client.api;

import io.swagger.client.ApiClient;

import io.swagger.client.model.User;
import java.util.*;

import java.util.*;
import feign.*;

@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaClientCodegen", date = "2016-01-11T21:48:33.457Z")
public interface UserApi extends ApiClient.Api {


  /**
   * Create user
   * This can only be done by the logged in user.
   * @param body Created user object
   * @return void
   */
  @RequestLine("POST /user")
  @Headers({
    "Content-type: application/json",
    "Accepts: application/json",
  })
  void createUser(User body);
  
  /**
   * Creates list of users with given input array
   * 
   * @param body List of user object
   * @return void
   */
  @RequestLine("POST /user/createWithArray")
  @Headers({
    "Content-type: application/json",
    "Accepts: application/json",
  })
  void createUsersWithArrayInput(List<User> body);
  
  /**
   * Creates list of users with given input array
   * 
   * @param body List of user object
   * @return void
   */
  @RequestLine("POST /user/createWithList")
  @Headers({
    "Content-type: application/json",
    "Accepts: application/json",
  })
  void createUsersWithListInput(List<User> body);
  
  /**
   * Logs user into the system
   * 
   * @param username The user name for login
   * @param password The password for login in clear text
   * @return String
   */
  @RequestLine("GET /user/login?username={username}&password={password}")
  @Headers({
    "Content-type: application/json",
    "Accepts: application/json",
  })
  String loginUser(@Param("username") String username, @Param("password") String password);
  
  /**
   * Logs out current logged in user session
   * 
   * @return void
   */
  @RequestLine("GET /user/logout")
  @Headers({
    "Content-type: application/json",
    "Accepts: application/json",
  })
  void logoutUser();
  
  /**
   * Get user by user name
   * 
   * @param username The name that needs to be fetched. Use user1 for testing.
   * @return User
   */
  @RequestLine("GET /user/{username}")
  @Headers({
    "Content-type: application/json",
    "Accepts: application/json",
  })
  User getUserByName(@Param("username") String username);
  
  /**
   * Updated user
   * This can only be done by the logged in user.
   * @param username name that need to be deleted
   * @param body Updated user object
   * @return void
   */
  @RequestLine("PUT /user/{username}")
  @Headers({
    "Content-type: application/json",
    "Accepts: application/json",
  })
  void updateUser(@Param("username") String username, User body);
  
  /**
   * Delete user
   * This can only be done by the logged in user.
   * @param username The name that needs to be deleted
   * @return void
   */
  @RequestLine("DELETE /user/{username}")
  @Headers({
    "Content-type: application/json",
    "Accepts: application/json",
  })
  void deleteUser(@Param("username") String username);
  
}
