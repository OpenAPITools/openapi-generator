using System;
using System.Collections.Generic;
using RestSharp;
using io.swagger.client;
using io.swagger.Model;

namespace io.swagger.Api {
  
  public class UserApi {
    string basePath;
    private readonly ApiInvoker apiInvoker = ApiInvoker.GetInstance();
    protected RestClient _client;

    public UserApi(String basePath = "http://petstore.swagger.io/v2")
    {
      this.basePath = basePath;
      _client = new RestClient(basePath);
    }

    public ApiInvoker getInvoker() {
      return apiInvoker;
    }

    // Sets the endpoint base url for the services being accessed
    public void setBasePath(string basePath) {
      this.basePath = basePath;
    }

    // Gets the endpoint base url for the services being accessed
    public String getBasePath() {
      return basePath;
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

      

      // path (url segment) parameters
      _request.AddUrlSegment("format", "json"); // set format to json by default
      
      // query parameters, if any
      
      // header parameters, if any
      
      // form parameters, if any
      

      try {
        
        _client.Execute(_request);
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

      

      // path (url segment) parameters
      _request.AddUrlSegment("format", "json"); // set format to json by default
      
      // query parameters, if any
      
      // header parameters, if any
      
      // form parameters, if any
      

      try {
        
        _client.Execute(_request);
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

      

      // path (url segment) parameters
      _request.AddUrlSegment("format", "json"); // set format to json by default
      
      // query parameters, if any
      
      // header parameters, if any
      
      // form parameters, if any
      

      try {
        
        _client.Execute(_request);
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

      

      // path (url segment) parameters
      _request.AddUrlSegment("format", "json"); // set format to json by default
      
      // query parameters, if any
       if (Username != null) _request.AddParameter("username", Username); if (Password != null) _request.AddParameter("password", Password);
      // header parameters, if any
      
      // form parameters, if any
      

      try {
        IRestResponse response = _client.Execute(_request);
        return (string) ApiInvoker.deserialize(response.Content, typeof(string));
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

      

      // path (url segment) parameters
      _request.AddUrlSegment("format", "json"); // set format to json by default
      
      // query parameters, if any
      
      // header parameters, if any
      
      // form parameters, if any
      

      try {
        
        _client.Execute(_request);
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
    
    /// <returns></returns>
    public User  GetUserByName (string Username) {
      // create path and map variables
      var path = "/user/{username}".Replace("{format}","json").Replace("{" + "username" + "}", apiInvoker.ParameterToString(Username));

      var _request = new RestRequest("/user/{username}", Method.GET);

      

      // path (url segment) parameters
      _request.AddUrlSegment("format", "json"); // set format to json by default
      _request.AddUrlSegment("username", apiInvoker.ParameterToString(Username));
      // query parameters, if any
      
      // header parameters, if any
      
      // form parameters, if any
      

      try {
        IRestResponse response = _client.Execute(_request);
        return (User) ApiInvoker.deserialize(response.Content, typeof(User));
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

      

      // path (url segment) parameters
      _request.AddUrlSegment("format", "json"); // set format to json by default
      _request.AddUrlSegment("username", apiInvoker.ParameterToString(Username));
      // query parameters, if any
      
      // header parameters, if any
      
      // form parameters, if any
      

      try {
        
        _client.Execute(_request);
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

      

      // path (url segment) parameters
      _request.AddUrlSegment("format", "json"); // set format to json by default
      _request.AddUrlSegment("username", apiInvoker.ParameterToString(Username));
      // query parameters, if any
      
      // header parameters, if any
      
      // form parameters, if any
      

      try {
        
        _client.Execute(_request);
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
