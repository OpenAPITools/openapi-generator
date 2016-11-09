package io.swagger.client.api;

import io.swagger.client.ApiClient;

import io.swagger.client.model.User;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import feign.*;


public interface UserApi extends ApiClient.Api {


  /**
   * Create user
   * This can only be done by the logged in user.
   * @param body Created user object (required)
   * @return void
   */
  @RequestLine("POST /user")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  void createUser(User body);

  /**
   * Creates list of users with given input array
   * 
   * @param body List of user object (required)
   * @return void
   */
  @RequestLine("POST /user/createWithArray")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  void createUsersWithArrayInput(List<User> body);

  /**
   * Creates list of users with given input array
   * 
   * @param body List of user object (required)
   * @return void
   */
  @RequestLine("POST /user/createWithList")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  void createUsersWithListInput(List<User> body);

  /**
   * Delete user
   * This can only be done by the logged in user.
   * @param username The name that needs to be deleted (required)
   * @return void
   */
  @RequestLine("DELETE /user/{username}")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  void deleteUser(@Param("username") String username);

  /**
   * Get user by user name
   * 
   * @param username The name that needs to be fetched. Use user1 for testing.  (required)
   * @return User
   */
  @RequestLine("GET /user/{username}")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  User getUserByName(@Param("username") String username);

  /**
   * Logs user into the system
   * 
   * @param username The user name for login (required)
   * @param password The password for login in clear text (required)
   * @return String
   */
  @RequestLine("GET /user/login?username={username}&password={password}")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  String loginUser(@Param("username") String username, @Param("password") String password);

  /**
   * Logs out current logged in user session
   * 
   * @return void
   */
  @RequestLine("GET /user/logout")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  void logoutUser();

  /**
   * Updated user
   * This can only be done by the logged in user.
   * @param username name that need to be deleted (required)
   * @param body Updated user object (required)
   * @return void
   */
  @RequestLine("PUT /user/{username}")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  void updateUser(@Param("username") String username, User body);
}
