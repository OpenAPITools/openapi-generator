package org.openapitools.client.api;

import org.openapitools.client.CollectionFormats.*;

import io.reactivex.rxjava3.core.Observable;
import io.reactivex.rxjava3.core.Completable;
import retrofit2.http.*;

import okhttp3.RequestBody;
import okhttp3.ResponseBody;
import okhttp3.MultipartBody;

import java.time.OffsetDateTime;
import org.openapitools.client.model.User;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

public interface UserApi {
  /**
   * Create user
   * This can only be done by the logged in user.
   * @param body Created user object (required)
   * @return Completable
   */
  @HTTP(method = "POST", path = "user", hasBody = true)
  Completable createUser(
    @retrofit2.http.Body User body
  );

  /**
   * Creates list of users with given input array
   * 
   * @param body List of user object (required)
   * @return Completable
   */
  @HTTP(method = "POST", path = "user/createWithArray", hasBody = true)
  Completable createUsersWithArrayInput(
    @retrofit2.http.Body List<User> body
  );

  /**
   * Creates list of users with given input array
   * 
   * @param body List of user object (required)
   * @return Completable
   */
  @HTTP(method = "POST", path = "user/createWithList", hasBody = true)
  Completable createUsersWithListInput(
    @retrofit2.http.Body List<User> body
  );

  /**
   * Delete user
   * This can only be done by the logged in user.
   * @param username The name that needs to be deleted (required)
   * @return Completable
   */
  @HTTP(method = "DELETE", path = "user/{username}")
  Completable deleteUser(
    @retrofit2.http.Path("username") String username
  );

  /**
   * Get user by user name
   * 
   * @param username The name that needs to be fetched. Use user1 for testing. (required)
   * @return Observable&lt;User&gt;
   */
  @HTTP(method = "GET", path = "user/{username}")
  Observable<User> getUserByName(
    @retrofit2.http.Path("username") String username
  );

  /**
   * Logs user into the system
   * 
   * @param username The user name for login (required)
   * @param password The password for login in clear text (required)
   * @return Observable&lt;String&gt;
   */
  @HTTP(method = "GET", path = "user/login")
  Observable<String> loginUser(
    @retrofit2.http.Query("username") String username, @retrofit2.http.Query("password") String password
  );

  /**
   * Logs out current logged in user session
   * 
   * @return Completable
   */
  @HTTP(method = "GET", path = "user/logout")
  Completable logoutUser();
    

  /**
   * Updated user
   * This can only be done by the logged in user.
   * @param username name that need to be deleted (required)
   * @param body Updated user object (required)
   * @return Completable
   */
  @HTTP(method = "PUT", path = "user/{username}", hasBody = true)
  Completable updateUser(
    @retrofit2.http.Path("username") String username, @retrofit2.http.Body User body
  );

}
