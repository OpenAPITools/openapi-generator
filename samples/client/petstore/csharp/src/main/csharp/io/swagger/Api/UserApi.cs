using System;
using System.Collections.Generic;
using RestSharp;
using IO.Swagger.Client;
using IO.Swagger.Model;

namespace IO.Swagger.Api {
  
  public class UserApi {
    string basePath;
    protected RestClient restClient;

    public UserApi(String basePath = "http://petstore.swagger.io/v2")
    {
      this.basePath = basePath;
      this.restClient = new RestClient(basePath);
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

      var _request = new RestRequest("/user", Method.POST);

      

      // add default header, if any
      foreach(KeyValuePair<string, string> defaultHeader in ApiInvoker.GetDefaultHeader())
      {
        _request.AddHeader(defaultHeader.Key, defaultHeader.Value);
      }

      _request.AddUrlSegment("format", "json"); // set format to json by default
      
      
      
      
      _request.AddParameter("application/json", ApiInvoker.Serialize(Body), ParameterType.RequestBody); // http body (model) parameter
      

      // make the HTTP request
      IRestResponse response = restClient.Execute(_request);
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

      var _request = new RestRequest("/user/createWithArray", Method.POST);

      

      // add default header, if any
      foreach(KeyValuePair<string, string> defaultHeader in ApiInvoker.GetDefaultHeader())
      {
        _request.AddHeader(defaultHeader.Key, defaultHeader.Value);
      }

      _request.AddUrlSegment("format", "json"); // set format to json by default
      
      
      
      
      _request.AddParameter("application/json", ApiInvoker.Serialize(Body), ParameterType.RequestBody); // http body (model) parameter
      

      // make the HTTP request
      IRestResponse response = restClient.Execute(_request);
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

      var _request = new RestRequest("/user/createWithList", Method.POST);

      

      // add default header, if any
      foreach(KeyValuePair<string, string> defaultHeader in ApiInvoker.GetDefaultHeader())
      {
        _request.AddHeader(defaultHeader.Key, defaultHeader.Value);
      }

      _request.AddUrlSegment("format", "json"); // set format to json by default
      
      
      
      
      _request.AddParameter("application/json", ApiInvoker.Serialize(Body), ParameterType.RequestBody); // http body (model) parameter
      

      // make the HTTP request
      IRestResponse response = restClient.Execute(_request);
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

      var _request = new RestRequest("/user/login", Method.GET);

      

      // add default header, if any
      foreach(KeyValuePair<string, string> defaultHeader in ApiInvoker.GetDefaultHeader())
      {
        _request.AddHeader(defaultHeader.Key, defaultHeader.Value);
      }

      _request.AddUrlSegment("format", "json"); // set format to json by default
      
       if (Username != null) _request.AddParameter("username", ApiInvoker.ParameterToString(Username)); // query parameter
       if (Password != null) _request.AddParameter("password", ApiInvoker.ParameterToString(Password)); // query parameter
      
      
      
      

      // make the HTTP request
      IRestResponse response = restClient.Execute(_request);
      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling LoginUser: " + response.Content);
      }
      return (string) ApiInvoker.Deserialize(response.Content, typeof(string));
    }
    
    
    /// <summary>
    /// Logs out current logged in user session 
    /// </summary>
    /// <returns></returns>
    public void LogoutUser () {

      var _request = new RestRequest("/user/logout", Method.GET);

      

      // add default header, if any
      foreach(KeyValuePair<string, string> defaultHeader in ApiInvoker.GetDefaultHeader())
      {
        _request.AddHeader(defaultHeader.Key, defaultHeader.Value);
      }

      _request.AddUrlSegment("format", "json"); // set format to json by default
      
      
      
      
      

      // make the HTTP request
      IRestResponse response = restClient.Execute(_request);
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

      var _request = new RestRequest("/user/{username}", Method.GET);

      
      // verify the required parameter 'Username' is set
      if (Username == null) throw new ApiException(400, "Missing required parameter 'Username' when calling GetUserByName");
      

      // add default header, if any
      foreach(KeyValuePair<string, string> defaultHeader in ApiInvoker.GetDefaultHeader())
      {
        _request.AddHeader(defaultHeader.Key, defaultHeader.Value);
      }

      _request.AddUrlSegment("format", "json"); // set format to json by default
      _request.AddUrlSegment("username", ApiInvoker.ParameterToString(Username)); // path (url segment) parameter
      
      
      
      
      

      // make the HTTP request
      IRestResponse response = restClient.Execute(_request);
      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling GetUserByName: " + response.Content);
      }
      return (User) ApiInvoker.Deserialize(response.Content, typeof(User));
    }
    
    
    /// <summary>
    /// Updated user This can only be done by the logged in user.
    /// </summary>
    /// <param name="Username">name that need to be deleted</param>
    /// <param name="Body">Updated user object</param>
    /// <returns></returns>
    public void UpdateUser (string Username, User Body) {

      var _request = new RestRequest("/user/{username}", Method.PUT);

      
      // verify the required parameter 'Username' is set
      if (Username == null) throw new ApiException(400, "Missing required parameter 'Username' when calling UpdateUser");
      

      // add default header, if any
      foreach(KeyValuePair<string, string> defaultHeader in ApiInvoker.GetDefaultHeader())
      {
        _request.AddHeader(defaultHeader.Key, defaultHeader.Value);
      }

      _request.AddUrlSegment("format", "json"); // set format to json by default
      _request.AddUrlSegment("username", ApiInvoker.ParameterToString(Username)); // path (url segment) parameter
      
      
      
      
      _request.AddParameter("application/json", ApiInvoker.Serialize(Body), ParameterType.RequestBody); // http body (model) parameter
      

      // make the HTTP request
      IRestResponse response = restClient.Execute(_request);
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

      var _request = new RestRequest("/user/{username}", Method.DELETE);

      
      // verify the required parameter 'Username' is set
      if (Username == null) throw new ApiException(400, "Missing required parameter 'Username' when calling DeleteUser");
      

      // add default header, if any
      foreach(KeyValuePair<string, string> defaultHeader in ApiInvoker.GetDefaultHeader())
      {
        _request.AddHeader(defaultHeader.Key, defaultHeader.Value);
      }

      _request.AddUrlSegment("format", "json"); // set format to json by default
      _request.AddUrlSegment("username", ApiInvoker.ParameterToString(Username)); // path (url segment) parameter
      
      
      
      
      

      // make the HTTP request
      IRestResponse response = restClient.Execute(_request);
      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling DeleteUser: " + response.Content);
      }
      
      return;
    }
    
  }
  
}
