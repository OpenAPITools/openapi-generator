using System;
using System.Collections.Generic;
using RestSharp;
using IO.Swagger.Client;
using IO.Swagger.Model;

namespace IO.Swagger.Api {
  
  public class UserApi {
    string basePath;
    public ApiClient apiClient {get; set;}

    public UserApi(String basePath = "http://petstore.swagger.io/v2")
    {
      this.basePath = basePath;
      this.apiClient = new ApiClient(basePath);
    }

    /// <summary>
    /// Create a new object 
    /// </summary>
    /// <param name="apiClient"> an instance of ApiClient
    /// <returns></returns>
    public UserApi(ApiClient apiClient = null) {
      if (apiClient == null) { // use the default one in Configuration
        this.apiClient = Configuration.apiClient; 
      } else {
        this.apiClient = apiClient;
      }
    }

    /// <summary>
    /// Sets the endpoint base url for the services being accessed
    /// </summary>
    /// <param name="basePath"> Base URL
    /// <returns></returns>
    public void SetBasePath(string basePath) {
      this.basePath = basePath;
    }

    /// <summary>
    /// Gets the endpoint base url for the services being accessed
    /// <returns>Base URL</returns>
    /// </summary>
    public String GetBasePath() {
      return this.basePath;
    }

    
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
      

      // make the HTTP request
      IRestResponse response = (IRestResponse) apiClient.CallApi(path, Method.POST, queryParams, postBody, headerParams, formParams, fileParams);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling CreateUser: " + response.Content);
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
      

      // make the HTTP request
      IRestResponse response = (IRestResponse) apiClient.CallApi(path, Method.POST, queryParams, postBody, headerParams, formParams, fileParams);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling CreateUsersWithArrayInput: " + response.Content);
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
      

      // make the HTTP request
      IRestResponse response = (IRestResponse) apiClient.CallApi(path, Method.POST, queryParams, postBody, headerParams, formParams, fileParams);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling CreateUsersWithListInput: " + response.Content);
      }
      
      return;
    }
    
    /// <summary>
    /// Logs user into the system 
    /// </summary>
    /// <param name="Username">The user name for login</param>
    /// <param name="Password">The password for login in clear text</param>
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
      
      
      
      

      // make the HTTP request
      IRestResponse response = (IRestResponse) apiClient.CallApi(path, Method.GET, queryParams, postBody, headerParams, formParams, fileParams);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling LoginUser: " + response.Content);
      }
      return (string) apiClient.Deserialize(response.Content, typeof(string));
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

      
      
      
      

      // make the HTTP request
      IRestResponse response = (IRestResponse) apiClient.CallApi(path, Method.GET, queryParams, postBody, headerParams, formParams, fileParams);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling LogoutUser: " + response.Content);
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

      
      
      
      

      // make the HTTP request
      IRestResponse response = (IRestResponse) apiClient.CallApi(path, Method.GET, queryParams, postBody, headerParams, formParams, fileParams);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling GetUserByName: " + response.Content);
      }
      return (User) apiClient.Deserialize(response.Content, typeof(User));
    }
    
    /// <summary>
    /// Updated user This can only be done by the logged in user.
    /// </summary>
    /// <param name="Username">name that need to be deleted</param>
    /// <param name="Body">Updated user object</param>
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
      

      // make the HTTP request
      IRestResponse response = (IRestResponse) apiClient.CallApi(path, Method.PUT, queryParams, postBody, headerParams, formParams, fileParams);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling UpdateUser: " + response.Content);
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

      
      
      
      

      // make the HTTP request
      IRestResponse response = (IRestResponse) apiClient.CallApi(path, Method.DELETE, queryParams, postBody, headerParams, formParams, fileParams);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling DeleteUser: " + response.Content);
      }
      
      return;
    }
    
  }
  
}
