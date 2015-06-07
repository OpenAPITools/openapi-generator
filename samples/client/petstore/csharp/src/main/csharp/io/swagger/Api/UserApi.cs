using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using RestSharp;
using IO.Swagger.Client;
using IO.Swagger.Model;

namespace IO.Swagger.Api {
  

  public interface IUserApi {
    
    /// <summary>
    /// Create user This can only be done by the logged in user.
    /// </summary>
    /// <param name="Body">Created user object</param>
    /// <returns></returns>
    void CreateUser (User Body);

    /// <summary>
    /// Create user This can only be done by the logged in user.
    /// </summary>
    /// <param name="Body">Created user object</param>
    /// <returns></returns>
    Task CreateUserAsync (User Body);
    
    /// <summary>
    /// Creates list of users with given input array 
    /// </summary>
    /// <param name="Body">List of user object</param>
    /// <returns></returns>
    void CreateUsersWithArrayInput (List<User> Body);

    /// <summary>
    /// Creates list of users with given input array 
    /// </summary>
    /// <param name="Body">List of user object</param>
    /// <returns></returns>
    Task CreateUsersWithArrayInputAsync (List<User> Body);
    
    /// <summary>
    /// Creates list of users with given input array 
    /// </summary>
    /// <param name="Body">List of user object</param>
    /// <returns></returns>
    void CreateUsersWithListInput (List<User> Body);

    /// <summary>
    /// Creates list of users with given input array 
    /// </summary>
    /// <param name="Body">List of user object</param>
    /// <returns></returns>
    Task CreateUsersWithListInputAsync (List<User> Body);
    
    /// <summary>
    /// Logs user into the system 
    /// </summary>
    /// <param name="Username">The user name for login</param>/// <param name="Password">The password for login in clear text</param>
    /// <returns>string</returns>
    string LoginUser (string Username, string Password);

    /// <summary>
    /// Logs user into the system 
    /// </summary>
    /// <param name="Username">The user name for login</param>/// <param name="Password">The password for login in clear text</param>
    /// <returns>string</returns>
    Task<string> LoginUserAsync (string Username, string Password);
    
    /// <summary>
    /// Logs out current logged in user session 
    /// </summary>
    
    /// <returns></returns>
    void LogoutUser ();

    /// <summary>
    /// Logs out current logged in user session 
    /// </summary>
    
    /// <returns></returns>
    Task LogoutUserAsync ();
    
    /// <summary>
    /// Get user by user name 
    /// </summary>
    /// <param name="Username">The name that needs to be fetched. Use user1 for testing. </param>
    /// <returns>User</returns>
    User GetUserByName (string Username);

    /// <summary>
    /// Get user by user name 
    /// </summary>
    /// <param name="Username">The name that needs to be fetched. Use user1 for testing. </param>
    /// <returns>User</returns>
    Task<User> GetUserByNameAsync (string Username);
    
    /// <summary>
    /// Updated user This can only be done by the logged in user.
    /// </summary>
    /// <param name="Username">name that need to be deleted</param>/// <param name="Body">Updated user object</param>
    /// <returns></returns>
    void UpdateUser (string Username, User Body);

    /// <summary>
    /// Updated user This can only be done by the logged in user.
    /// </summary>
    /// <param name="Username">name that need to be deleted</param>/// <param name="Body">Updated user object</param>
    /// <returns></returns>
    Task UpdateUserAsync (string Username, User Body);
    
    /// <summary>
    /// Delete user This can only be done by the logged in user.
    /// </summary>
    /// <param name="Username">The name that needs to be deleted</param>
    /// <returns></returns>
    void DeleteUser (string Username);

    /// <summary>
    /// Delete user This can only be done by the logged in user.
    /// </summary>
    /// <param name="Username">The name that needs to be deleted</param>
    /// <returns></returns>
    Task DeleteUserAsync (string Username);
    
  }

  /// <summary>
  /// Represents a collection of functions to interact with the API endpoints
  /// </summary>
  public class UserApi : IUserApi {

    /// <summary>
    /// Initializes a new instance of the <see cref="UserApi"/> class.
    /// </summary>
    /// <param name="apiClient"> an instance of ApiClient (optional)
    /// <returns></returns>
    public UserApi(ApiClient apiClient = null) {
      if (apiClient == null) { // use the default one in Configuration
        this.apiClient = Configuration.apiClient; 
      } else {
        this.apiClient = apiClient;
      }
    }

    /// <summary>
    /// Initializes a new instance of the <see cref="UserApi"/> class.
    /// </summary>
    /// <returns></returns>
    public UserApi(String basePath)
    {
      this.apiClient = new ApiClient(basePath);
    }

    /// <summary>
    /// Sets the base path of the API client.
    /// </summary>
    /// <value>The base path</value>
    public void SetBasePath(String basePath) {
      this.apiClient.basePath = basePath;
    }

    /// <summary>
    /// Gets the base path of the API client.
    /// </summary>
    /// <value>The base path</value>
    public String GetBasePath(String basePath) {
      return this.apiClient.basePath;
    }

    /// <summary>
    /// Gets or sets the API client.
    /// </summary>
    /// <value>The API client</value>
    public ApiClient apiClient {get; set;}


    
    /// <summary>
    /// Create user This can only be done by the logged in user.
    /// </summary>
    /// <param name="Body">Created user object</param>
    /// <returns></returns>
    public void CreateUser (User Body) {

      

      var path = "/user";
      path = path.Replace("{format}", "json");
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

      
      
      
      postBody = apiClient.Serialize(Body); // http body (model) parameter
      

      // authentication setting, if any
      String[] authSettings = new String[] {  };

      // make the HTTP request
      IRestResponse response = (IRestResponse) apiClient.CallApi(path, Method.POST, queryParams, postBody, headerParams, formParams, fileParams, authSettings);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling CreateUser: " + response.Content, response.Content);
      }
      
      return;
    }
	
	 /// <summary>
    /// Create user This can only be done by the logged in user.
    /// </summary>
    /// <param name="Body">Created user object</param>
    /// <returns></returns>
    public async Task CreateUserAsync (User Body) {

      

      var path = "/user";
      path = path.Replace("{format}", "json");
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

      
      
      
      postBody = apiClient.Serialize(Body); // http body (model) parameter
      

      // authentication setting, if any
      String[] authSettings = new String[] {  };

      // make the HTTP request
      IRestResponse response = (IRestResponse) await apiClient.CallApiAsync(path, Method.POST, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling CreateUser: " + response.Content, response.Content);
      }
      
      return;
    }
    
    /// <summary>
    /// Creates list of users with given input array 
    /// </summary>
    /// <param name="Body">List of user object</param>
    /// <returns></returns>
    public void CreateUsersWithArrayInput (List<User> Body) {

      

      var path = "/user/createWithArray";
      path = path.Replace("{format}", "json");
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

      
      
      
      postBody = apiClient.Serialize(Body); // http body (model) parameter
      

      // authentication setting, if any
      String[] authSettings = new String[] {  };

      // make the HTTP request
      IRestResponse response = (IRestResponse) apiClient.CallApi(path, Method.POST, queryParams, postBody, headerParams, formParams, fileParams, authSettings);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling CreateUsersWithArrayInput: " + response.Content, response.Content);
      }
      
      return;
    }
	
	 /// <summary>
    /// Creates list of users with given input array 
    /// </summary>
    /// <param name="Body">List of user object</param>
    /// <returns></returns>
    public async Task CreateUsersWithArrayInputAsync (List<User> Body) {

      

      var path = "/user/createWithArray";
      path = path.Replace("{format}", "json");
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

      
      
      
      postBody = apiClient.Serialize(Body); // http body (model) parameter
      

      // authentication setting, if any
      String[] authSettings = new String[] {  };

      // make the HTTP request
      IRestResponse response = (IRestResponse) await apiClient.CallApiAsync(path, Method.POST, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling CreateUsersWithArrayInput: " + response.Content, response.Content);
      }
      
      return;
    }
    
    /// <summary>
    /// Creates list of users with given input array 
    /// </summary>
    /// <param name="Body">List of user object</param>
    /// <returns></returns>
    public void CreateUsersWithListInput (List<User> Body) {

      

      var path = "/user/createWithList";
      path = path.Replace("{format}", "json");
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

      
      
      
      postBody = apiClient.Serialize(Body); // http body (model) parameter
      

      // authentication setting, if any
      String[] authSettings = new String[] {  };

      // make the HTTP request
      IRestResponse response = (IRestResponse) apiClient.CallApi(path, Method.POST, queryParams, postBody, headerParams, formParams, fileParams, authSettings);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling CreateUsersWithListInput: " + response.Content, response.Content);
      }
      
      return;
    }
	
	 /// <summary>
    /// Creates list of users with given input array 
    /// </summary>
    /// <param name="Body">List of user object</param>
    /// <returns></returns>
    public async Task CreateUsersWithListInputAsync (List<User> Body) {

      

      var path = "/user/createWithList";
      path = path.Replace("{format}", "json");
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

      
      
      
      postBody = apiClient.Serialize(Body); // http body (model) parameter
      

      // authentication setting, if any
      String[] authSettings = new String[] {  };

      // make the HTTP request
      IRestResponse response = (IRestResponse) await apiClient.CallApiAsync(path, Method.POST, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling CreateUsersWithListInput: " + response.Content, response.Content);
      }
      
      return;
    }
    
    /// <summary>
    /// Logs user into the system 
    /// </summary>
    /// <param name="Username">The user name for login</param>/// <param name="Password">The password for login in clear text</param>
    /// <returns>string</returns>
    public string LoginUser (string Username, string Password) {

      

      var path = "/user/login";
      path = path.Replace("{format}", "json");
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

       if (Username != null) queryParams.Add("username", apiClient.ParameterToString(Username)); // query parameter
       if (Password != null) queryParams.Add("password", apiClient.ParameterToString(Password)); // query parameter
      
      
      
      

      // authentication setting, if any
      String[] authSettings = new String[] {  };

      // make the HTTP request
      IRestResponse response = (IRestResponse) apiClient.CallApi(path, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, authSettings);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling LoginUser: " + response.Content, response.Content);
      }
      return (string) apiClient.Deserialize(response.Content, typeof(string));
    }
	
	 /// <summary>
    /// Logs user into the system 
    /// </summary>
    /// <param name="Username">The user name for login</param>/// <param name="Password">The password for login in clear text</param>
    /// <returns>string</returns>
    public async Task<string> LoginUserAsync (string Username, string Password) {

      

      var path = "/user/login";
      path = path.Replace("{format}", "json");
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

       if (Username != null) queryParams.Add("username", apiClient.ParameterToString(Username)); // query parameter
       if (Password != null) queryParams.Add("password", apiClient.ParameterToString(Password)); // query parameter
      
      
      
      

      // authentication setting, if any
      String[] authSettings = new String[] {  };

      // make the HTTP request
      IRestResponse response = (IRestResponse) await apiClient.CallApiAsync(path, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling LoginUser: " + response.Content, response.Content);
      }
      return (string) ApiInvoker.Deserialize(response.Content, typeof(string));
    }
    
    /// <summary>
    /// Logs out current logged in user session 
    /// </summary>
    
    /// <returns></returns>
    public void LogoutUser () {

      

      var path = "/user/logout";
      path = path.Replace("{format}", "json");
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

      
      
      
      

      // authentication setting, if any
      String[] authSettings = new String[] {  };

      // make the HTTP request
      IRestResponse response = (IRestResponse) apiClient.CallApi(path, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, authSettings);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling LogoutUser: " + response.Content, response.Content);
      }
      
      return;
    }
	
	 /// <summary>
    /// Logs out current logged in user session 
    /// </summary>
    
    /// <returns></returns>
    public async Task LogoutUserAsync () {

      

      var path = "/user/logout";
      path = path.Replace("{format}", "json");
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

      
      
      
      

      // authentication setting, if any
      String[] authSettings = new String[] {  };

      // make the HTTP request
      IRestResponse response = (IRestResponse) await apiClient.CallApiAsync(path, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling LogoutUser: " + response.Content, response.Content);
      }
      
      return;
    }
    
    /// <summary>
    /// Get user by user name 
    /// </summary>
    /// <param name="Username">The name that needs to be fetched. Use user1 for testing. </param>
    /// <returns>User</returns>
    public User GetUserByName (string Username) {

      
      // verify the required parameter 'Username' is set
      if (Username == null) throw new ApiException(400, "Missing required parameter 'Username' when calling GetUserByName");
      

      var path = "/user/{username}";
      path = path.Replace("{format}", "json");
      path = path.Replace("{" + "username" + "}", apiClient.ParameterToString(Username));
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

      
      
      
      

      // authentication setting, if any
      String[] authSettings = new String[] {  };

      // make the HTTP request
      IRestResponse response = (IRestResponse) apiClient.CallApi(path, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, authSettings);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling GetUserByName: " + response.Content, response.Content);
      }
      return (User) apiClient.Deserialize(response.Content, typeof(User));
    }
	
	 /// <summary>
    /// Get user by user name 
    /// </summary>
    /// <param name="Username">The name that needs to be fetched. Use user1 for testing. </param>
    /// <returns>User</returns>
    public async Task<User> GetUserByNameAsync (string Username) {

      
          // verify the required parameter 'Username' is set
          if (Username == null) throw new ApiException(400, "Missing required parameter 'Username' when calling GetUserByName");
      

      var path = "/user/{username}";
      path = path.Replace("{format}", "json");
      path = path.Replace("{" + "username" + "}", apiClient.ParameterToString(Username));
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

      
      
      
      

      // authentication setting, if any
      String[] authSettings = new String[] {  };

      // make the HTTP request
      IRestResponse response = (IRestResponse) await apiClient.CallApiAsync(path, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling GetUserByName: " + response.Content, response.Content);
      }
      return (User) ApiInvoker.Deserialize(response.Content, typeof(User));
    }
    
    /// <summary>
    /// Updated user This can only be done by the logged in user.
    /// </summary>
    /// <param name="Username">name that need to be deleted</param>/// <param name="Body">Updated user object</param>
    /// <returns></returns>
    public void UpdateUser (string Username, User Body) {

      
      // verify the required parameter 'Username' is set
      if (Username == null) throw new ApiException(400, "Missing required parameter 'Username' when calling UpdateUser");
      

      var path = "/user/{username}";
      path = path.Replace("{format}", "json");
      path = path.Replace("{" + "username" + "}", apiClient.ParameterToString(Username));
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

      
      
      
      postBody = apiClient.Serialize(Body); // http body (model) parameter
      

      // authentication setting, if any
      String[] authSettings = new String[] {  };

      // make the HTTP request
      IRestResponse response = (IRestResponse) apiClient.CallApi(path, Method.PUT, queryParams, postBody, headerParams, formParams, fileParams, authSettings);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling UpdateUser: " + response.Content, response.Content);
      }
      
      return;
    }
	
	 /// <summary>
    /// Updated user This can only be done by the logged in user.
    /// </summary>
    /// <param name="Username">name that need to be deleted</param>/// <param name="Body">Updated user object</param>
    /// <returns></returns>
    public async Task UpdateUserAsync (string Username, User Body) {

      
          // verify the required parameter 'Username' is set
          if (Username == null) throw new ApiException(400, "Missing required parameter 'Username' when calling UpdateUser");
      

      var path = "/user/{username}";
      path = path.Replace("{format}", "json");
      path = path.Replace("{" + "username" + "}", apiClient.ParameterToString(Username));
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

      
      
      
      postBody = apiClient.Serialize(Body); // http body (model) parameter
      

      // authentication setting, if any
      String[] authSettings = new String[] {  };

      // make the HTTP request
      IRestResponse response = (IRestResponse) await apiClient.CallApiAsync(path, Method.PUT, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling UpdateUser: " + response.Content, response.Content);
      }
      
      return;
    }
    
    /// <summary>
    /// Delete user This can only be done by the logged in user.
    /// </summary>
    /// <param name="Username">The name that needs to be deleted</param>
    /// <returns></returns>
    public void DeleteUser (string Username) {

      
      // verify the required parameter 'Username' is set
      if (Username == null) throw new ApiException(400, "Missing required parameter 'Username' when calling DeleteUser");
      

      var path = "/user/{username}";
      path = path.Replace("{format}", "json");
      path = path.Replace("{" + "username" + "}", apiClient.ParameterToString(Username));
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

      
      
      
      

      // authentication setting, if any
      String[] authSettings = new String[] {  };

      // make the HTTP request
      IRestResponse response = (IRestResponse) apiClient.CallApi(path, Method.DELETE, queryParams, postBody, headerParams, formParams, fileParams, authSettings);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling DeleteUser: " + response.Content, response.Content);
      }
      
      return;
    }
	
	 /// <summary>
    /// Delete user This can only be done by the logged in user.
    /// </summary>
    /// <param name="Username">The name that needs to be deleted</param>
    /// <returns></returns>
    public async Task DeleteUserAsync (string Username) {

      
          // verify the required parameter 'Username' is set
          if (Username == null) throw new ApiException(400, "Missing required parameter 'Username' when calling DeleteUser");
      

      var path = "/user/{username}";
      path = path.Replace("{format}", "json");
      path = path.Replace("{" + "username" + "}", apiClient.ParameterToString(Username));
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

      
      
      
      

      // authentication setting, if any
      String[] authSettings = new String[] {  };

      // make the HTTP request
      IRestResponse response = (IRestResponse) await apiClient.CallApiAsync(path, Method.DELETE, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling DeleteUser: " + response.Content, response.Content);
      }
      
      return;
    }
    
  }  
  
}
