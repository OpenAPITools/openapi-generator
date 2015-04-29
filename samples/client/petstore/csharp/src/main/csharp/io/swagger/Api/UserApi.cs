using System;
using System.Collections.Generic;
using RestSharp;
using io.swagger.client;
using io.swagger.Model;

namespace io.swagger.Api {
  
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
    public void  CreateUser (User Body) {
      // create path and map variables
      var path = "/user".Replace("{format}","json");

      var _request = new RestRequest("/user", Method.POST);

      

      _request.AddUrlSegment("format", "json"); // set format to json by default
      
      
      
      
      
      _request.AddParameter("application/json", ApiInvoker.Serialize(Body), ParameterType.RequestBody); // HTTP request body (model)
      

      try {
        
        restClient.Execute(_request);
        return;
      } catch (Exception ex) {
        if(ex != null) {
          return ;
        }
        else {
          throw ex;
        }
      }
    }
    
    
    /// <summary>
    /// Creates list of users with given input array 
    /// </summary>
    /// <param name="Body">List of user object</param>
    /// <returns></returns>
    public void  CreateUsersWithArrayInput (List<User> Body) {
      // create path and map variables
      var path = "/user/createWithArray".Replace("{format}","json");

      var _request = new RestRequest("/user/createWithArray", Method.POST);

      

      _request.AddUrlSegment("format", "json"); // set format to json by default
      
      
      
      
      
      _request.AddParameter("application/json", ApiInvoker.Serialize(Body), ParameterType.RequestBody); // HTTP request body (model)
      

      try {
        
        restClient.Execute(_request);
        return;
      } catch (Exception ex) {
        if(ex != null) {
          return ;
        }
        else {
          throw ex;
        }
      }
    }
    
    
    /// <summary>
    /// Creates list of users with given input array 
    /// </summary>
    /// <param name="Body">List of user object</param>
    /// <returns></returns>
    public void  CreateUsersWithListInput (List<User> Body) {
      // create path and map variables
      var path = "/user/createWithList".Replace("{format}","json");

      var _request = new RestRequest("/user/createWithList", Method.POST);

      

      _request.AddUrlSegment("format", "json"); // set format to json by default
      
      
      
      
      
      _request.AddParameter("application/json", ApiInvoker.Serialize(Body), ParameterType.RequestBody); // HTTP request body (model)
      

      try {
        
        restClient.Execute(_request);
        return;
      } catch (Exception ex) {
        if(ex != null) {
          return ;
        }
        else {
          throw ex;
        }
      }
    }
    
    
    /// <summary>
    /// Logs user into the system 
    /// </summary>
    /// <param name="Username">The user name for login</param>
    /// <param name="Password">The password for login in clear text</param>
    
    /// <returns></returns>
    public string  LoginUser (string Username, string Password) {
      // create path and map variables
      var path = "/user/login".Replace("{format}","json");

      var _request = new RestRequest("/user/login", Method.GET);

      

      _request.AddUrlSegment("format", "json"); // set format to json by default
      
       if (Username != null) _request.AddParameter("username", ApiInvoker.ParameterToString(Username)); // query parameter
       if (Password != null) _request.AddParameter("password", ApiInvoker.ParameterToString(Password)); // query parameter
      
      
      
      

      try {
        IRestResponse response = restClient.Execute(_request);
        return (string) ApiInvoker.Deserialize(response.Content, typeof(string));
        //return ((object)response) as string;
        
      } catch (Exception ex) {
        if(ex != null) {
          return null;
        }
        else {
          throw ex;
        }
      }
    }
    
    
    /// <summary>
    /// Logs out current logged in user session 
    /// </summary>
    /// <returns></returns>
    public void  LogoutUser () {
      // create path and map variables
      var path = "/user/logout".Replace("{format}","json");

      var _request = new RestRequest("/user/logout", Method.GET);

      

      _request.AddUrlSegment("format", "json"); // set format to json by default
      
      
      
      
      

      try {
        
        restClient.Execute(_request);
        return;
      } catch (Exception ex) {
        if(ex != null) {
          return ;
        }
        else {
          throw ex;
        }
      }
    }
    
    
    /// <summary>
    /// Get user by user name 
    /// </summary>
    /// <param name="Username">The name that needs to be fetched. Use user1 for testing. </param>
    public User  GetUserByName (string Username) {
      // create path and map variables
      var path = "/user/{username}".Replace("{format}","json").Replace("{" + "username" + "}", apiInvoker.ParameterToString(Username));

      var _request = new RestRequest("/user/{username}", Method.GET);

      

      _request.AddUrlSegment("format", "json"); // set format to json by default
      _request.AddUrlSegment("username", ApiInvoker.ParameterToString(Username)); // path (url segment) parameter
      
      
      
      
      

      try {
        IRestResponse response = restClient.Execute(_request);
        return (User) ApiInvoker.Deserialize(response.Content, typeof(User));
        //return ((object)response) as User;
        
      } catch (Exception ex) {
        if(ex != null) {
          return null;
        }
        else {
          throw ex;
        }
      }
    }
    
    
    /// <summary>
    /// Updated user This can only be done by the logged in user.
    /// </summary>
    /// <param name="Username">name that need to be deleted</param>
    /// <param name="Body">Updated user object</param>
    /// <returns></returns>
    public void  UpdateUser (string Username, User Body) {
      // create path and map variables
      var path = "/user/{username}".Replace("{format}","json").Replace("{" + "username" + "}", apiInvoker.ParameterToString(Username));

      var _request = new RestRequest("/user/{username}", Method.PUT);

      

      _request.AddUrlSegment("format", "json"); // set format to json by default
      _request.AddUrlSegment("username", ApiInvoker.ParameterToString(Username)); // path (url segment) parameter
      
      
      
      
      
      _request.AddParameter("application/json", ApiInvoker.Serialize(Body), ParameterType.RequestBody); // HTTP request body (model)
      

      try {
        
        restClient.Execute(_request);
        return;
      } catch (Exception ex) {
        if(ex != null) {
          return ;
        }
        else {
          throw ex;
        }
      }
    }
    
    
    /// <summary>
    /// Delete user This can only be done by the logged in user.
    /// </summary>
    /// <param name="Username">The name that needs to be deleted</param>
    /// <returns></returns>
    public void  DeleteUser (string Username) {
      // create path and map variables
      var path = "/user/{username}".Replace("{format}","json").Replace("{" + "username" + "}", apiInvoker.ParameterToString(Username));

      var _request = new RestRequest("/user/{username}", Method.DELETE);

      

      _request.AddUrlSegment("format", "json"); // set format to json by default
      _request.AddUrlSegment("username", ApiInvoker.ParameterToString(Username)); // path (url segment) parameter
      
      
      
      
      

      try {
        
        restClient.Execute(_request);
        return;
      } catch (Exception ex) {
        if(ex != null) {
          return ;
        }
        else {
          throw ex;
        }
      }
    }
    
  }
  
}
