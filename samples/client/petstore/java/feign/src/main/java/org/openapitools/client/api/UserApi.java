package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import org.openapitools.client.EncodingUtils;
import org.openapitools.client.model.ApiResponse;

import java.time.OffsetDateTime;
import org.openapitools.client.model.User;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import feign.*;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")
public interface UserApi extends ApiClient.Api {


  /**
   * Create user
   * This can only be done by the logged in user.
   * @param user Created user object (required)
   */
  @RequestLine("POST /user")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  void createUser(@javax.annotation.Nonnull User user);

  /**
   * Create user
   * Similar to <code>createUser</code> but it also returns the http response headers .
   * This can only be done by the logged in user.
   * @param user Created user object (required)
   */
  @RequestLine("POST /user")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  ApiResponse<Void> createUserWithHttpInfo(@javax.annotation.Nonnull User user);



  /**
   * Creates list of users with given input array
   * 
   * @param user List of user object (required)
   */
  @RequestLine("POST /user/createWithArray")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  void createUsersWithArrayInput(@javax.annotation.Nonnull List<User> user);

  /**
   * Creates list of users with given input array
   * Similar to <code>createUsersWithArrayInput</code> but it also returns the http response headers .
   * 
   * @param user List of user object (required)
   */
  @RequestLine("POST /user/createWithArray")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  ApiResponse<Void> createUsersWithArrayInputWithHttpInfo(@javax.annotation.Nonnull List<User> user);



  /**
   * Creates list of users with given input array
   * 
   * @param user List of user object (required)
   */
  @RequestLine("POST /user/createWithList")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  void createUsersWithListInput(@javax.annotation.Nonnull List<User> user);

  /**
   * Creates list of users with given input array
   * Similar to <code>createUsersWithListInput</code> but it also returns the http response headers .
   * 
   * @param user List of user object (required)
   */
  @RequestLine("POST /user/createWithList")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  ApiResponse<Void> createUsersWithListInputWithHttpInfo(@javax.annotation.Nonnull List<User> user);



  /**
   * Delete user
   * This can only be done by the logged in user.
   * @param username The name that needs to be deleted (required)
   */
  @RequestLine("DELETE /user/{username}")
  @Headers({
    "Accept: application/json",
  })
  void deleteUser(@Param("username") @javax.annotation.Nonnull String username);

  /**
   * Delete user
   * Similar to <code>deleteUser</code> but it also returns the http response headers .
   * This can only be done by the logged in user.
   * @param username The name that needs to be deleted (required)
   */
  @RequestLine("DELETE /user/{username}")
  @Headers({
    "Accept: application/json",
  })
  ApiResponse<Void> deleteUserWithHttpInfo(@Param("username") @javax.annotation.Nonnull String username);



  /**
   * Get user by user name
   * 
   * @param username The name that needs to be fetched. Use user1 for testing. (required)
   * @return User
   */
  @RequestLine("GET /user/{username}")
  @Headers({
    "Accept: application/json,application/xml",
  })
  User getUserByName(@Param("username") @javax.annotation.Nonnull String username);

  /**
   * Get user by user name
   * Similar to <code>getUserByName</code> but it also returns the http response headers .
   * 
   * @param username The name that needs to be fetched. Use user1 for testing. (required)
   * @return A ApiResponse that wraps the response boyd and the http headers.
   */
  @RequestLine("GET /user/{username}")
  @Headers({
    "Accept: application/json,application/xml",
  })
  ApiResponse<User> getUserByNameWithHttpInfo(@Param("username") @javax.annotation.Nonnull String username);



  /**
   * Logs user into the system
   * 
   * @param username The user name for login (required)
   * @param password The password for login in clear text (required)
   * @return String
   */
  @RequestLine("GET /user/login?username={username}&password={password}")
  @Headers({
    "Accept: application/json,application/xml",
  })
  String loginUser(@Param("username") @javax.annotation.Nonnull String username, @Param("password") @javax.annotation.Nonnull String password);

  /**
   * Logs user into the system
   * Similar to <code>loginUser</code> but it also returns the http response headers .
   * 
   * @param username The user name for login (required)
   * @param password The password for login in clear text (required)
   * @return A ApiResponse that wraps the response boyd and the http headers.
   */
  @RequestLine("GET /user/login?username={username}&password={password}")
  @Headers({
    "Accept: application/json,application/xml",
  })
  ApiResponse<String> loginUserWithHttpInfo(@Param("username") @javax.annotation.Nonnull String username, @Param("password") @javax.annotation.Nonnull String password);


  /**
   * Logs user into the system
   * 
   * Note, this is equivalent to the other <code>loginUser</code> method,
   * but with the query parameters collected into a single Map parameter. This
   * is convenient for services with optional query parameters, especially when
   * used with the {@link LoginUserQueryParams} class that allows for
   * building up this map in a fluent style.
   * @param queryParams Map of query parameters as name-value pairs
   *   <p>The following elements may be specified in the query map:</p>
   *   <ul>
   *   <li>username - The user name for login (required)</li>
   *   <li>password - The password for login in clear text (required)</li>
   *   </ul>
   * @return String
   */
  @RequestLine("GET /user/login?username={username}&password={password}")
  @Headers({
  "Accept: application/json,application/xml",
  })
  String loginUser(@QueryMap(encoded=true) LoginUserQueryParams queryParams);

  /**
  * Logs user into the system
  * 
  * Note, this is equivalent to the other <code>loginUser</code> that receives the query parameters as a map,
  * but this one also exposes the Http response headers
      * @param queryParams Map of query parameters as name-value pairs
      *   <p>The following elements may be specified in the query map:</p>
      *   <ul>
          *   <li>username - The user name for login (required)</li>
          *   <li>password - The password for login in clear text (required)</li>
      *   </ul>
          * @return String
      */
      @RequestLine("GET /user/login?username={username}&password={password}")
      @Headers({
    "Accept: application/json,application/xml",
      })
   ApiResponse<String> loginUserWithHttpInfo(@QueryMap(encoded=true) LoginUserQueryParams queryParams);


   /**
   * A convenience class for generating query parameters for the
   * <code>loginUser</code> method in a fluent style.
   */
  public static class LoginUserQueryParams extends HashMap<String, Object> {
    public LoginUserQueryParams username(@javax.annotation.Nonnull final String value) {
      put("username", EncodingUtils.encode(value));
      return this;
    }
    public LoginUserQueryParams password(@javax.annotation.Nonnull final String value) {
      put("password", EncodingUtils.encode(value));
      return this;
    }
  }

  /**
   * Logs out current logged in user session
   * 
   */
  @RequestLine("GET /user/logout")
  @Headers({
    "Accept: application/json",
  })
  void logoutUser();

  /**
   * Logs out current logged in user session
   * Similar to <code>logoutUser</code> but it also returns the http response headers .
   * 
   */
  @RequestLine("GET /user/logout")
  @Headers({
    "Accept: application/json",
  })
  ApiResponse<Void> logoutUserWithHttpInfo();



  /**
   * Updated user
   * This can only be done by the logged in user.
   * @param username name that need to be deleted (required)
   * @param user Updated user object (required)
   */
  @RequestLine("PUT /user/{username}")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  void updateUser(@Param("username") @javax.annotation.Nonnull String username, @javax.annotation.Nonnull User user);

  /**
   * Updated user
   * Similar to <code>updateUser</code> but it also returns the http response headers .
   * This can only be done by the logged in user.
   * @param username name that need to be deleted (required)
   * @param user Updated user object (required)
   */
  @RequestLine("PUT /user/{username}")
  @Headers({
    "Content-Type: application/json",
    "Accept: application/json",
  })
  ApiResponse<Void> updateUserWithHttpInfo(@Param("username") @javax.annotation.Nonnull String username, @javax.annotation.Nonnull User user);


}
