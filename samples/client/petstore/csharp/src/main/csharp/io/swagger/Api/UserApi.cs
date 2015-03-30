  using System;
  using System.Collections.Generic;
  using io.swagger.client;
  using io.swagger.Model;
  
  
  
  

  namespace io.swagger.Api {
    
    public class UserApi {
      string basePath;
      private readonly ApiInvoker apiInvoker = ApiInvoker.GetInstance();

      public UserApi(String basePath = "http://petstore.swagger.io/v2")
      {
        this.basePath = basePath;
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
      public void  createUser (User Body) {
        // create path and map variables
        var path = "/user".Replace("{format}","json");

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();
        var formParams = new Dictionary<String, object>();

        

        

        

        

        try {
          if (typeof(void) == typeof(byte[])) {
            var response = apiInvoker.invokeBinaryAPI(basePath, path, "GET", queryParams, null, headerParams, formParams);
            return ;
          } else {
            var response = apiInvoker.invokeAPI(basePath, path, "POST", queryParams, Body, headerParams, formParams);
            if(response != null){
               return ;
            }
            else {
              return ;
            }
          }
        } catch (ApiException ex) {
          if(ex.ErrorCode == 404) {
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
      public void  createUsersWithArrayInput (List<User> Body) {
        // create path and map variables
        var path = "/user/createWithArray".Replace("{format}","json");

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();
        var formParams = new Dictionary<String, object>();

        

        

        

        

        try {
          if (typeof(void) == typeof(byte[])) {
            var response = apiInvoker.invokeBinaryAPI(basePath, path, "GET", queryParams, null, headerParams, formParams);
            return ;
          } else {
            var response = apiInvoker.invokeAPI(basePath, path, "POST", queryParams, Body, headerParams, formParams);
            if(response != null){
               return ;
            }
            else {
              return ;
            }
          }
        } catch (ApiException ex) {
          if(ex.ErrorCode == 404) {
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
      public void  createUsersWithListInput (List<User> Body) {
        // create path and map variables
        var path = "/user/createWithList".Replace("{format}","json");

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();
        var formParams = new Dictionary<String, object>();

        

        

        

        

        try {
          if (typeof(void) == typeof(byte[])) {
            var response = apiInvoker.invokeBinaryAPI(basePath, path, "GET", queryParams, null, headerParams, formParams);
            return ;
          } else {
            var response = apiInvoker.invokeAPI(basePath, path, "POST", queryParams, Body, headerParams, formParams);
            if(response != null){
               return ;
            }
            else {
              return ;
            }
          }
        } catch (ApiException ex) {
          if(ex.ErrorCode == 404) {
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
      public string  loginUser (string Username, string Password) {
        // create path and map variables
        var path = "/user/login".Replace("{format}","json");

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();
        var formParams = new Dictionary<String, object>();

        

        if (Username != null){
          string paramStr = (Username is DateTime) ? ((DateTime)(object)Username).ToString("u") : Convert.ToString(Username);
          queryParams.Add("username", paramStr);
		}
        if (Password != null){
          string paramStr = (Password is DateTime) ? ((DateTime)(object)Password).ToString("u") : Convert.ToString(Password);
          queryParams.Add("password", paramStr);
		}
        

        

        

        try {
          if (typeof(string) == typeof(byte[])) {
            var response = apiInvoker.invokeBinaryAPI(basePath, path, "GET", queryParams, null, headerParams, formParams);
            return ((object)response) as string;
          } else {
            var response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, null, headerParams, formParams);
            if(response != null){
               return (string) ApiInvoker.deserialize(response, typeof(string));
            }
            else {
              return null;
            }
          }
        } catch (ApiException ex) {
          if(ex.ErrorCode == 404) {
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
      public void  logoutUser () {
        // create path and map variables
        var path = "/user/logout".Replace("{format}","json");

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();
        var formParams = new Dictionary<String, object>();

        

        

        

        

        try {
          if (typeof(void) == typeof(byte[])) {
            var response = apiInvoker.invokeBinaryAPI(basePath, path, "GET", queryParams, null, headerParams, formParams);
            return ;
          } else {
            var response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, null, headerParams, formParams);
            if(response != null){
               return ;
            }
            else {
              return ;
            }
          }
        } catch (ApiException ex) {
          if(ex.ErrorCode == 404) {
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
      public User  getUserByName (string Username) {
        // create path and map variables
        var path = "/user/{username}".Replace("{format}","json").Replace("{" + "username" + "}", apiInvoker.escapeString(Username.ToString()));

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();
        var formParams = new Dictionary<String, object>();

        

        

        

        

        try {
          if (typeof(User) == typeof(byte[])) {
            var response = apiInvoker.invokeBinaryAPI(basePath, path, "GET", queryParams, null, headerParams, formParams);
            return ((object)response) as User;
          } else {
            var response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, null, headerParams, formParams);
            if(response != null){
               return (User) ApiInvoker.deserialize(response, typeof(User));
            }
            else {
              return null;
            }
          }
        } catch (ApiException ex) {
          if(ex.ErrorCode == 404) {
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
      public void  updateUser (string Username, User Body) {
        // create path and map variables
        var path = "/user/{username}".Replace("{format}","json").Replace("{" + "username" + "}", apiInvoker.escapeString(Username.ToString()));

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();
        var formParams = new Dictionary<String, object>();

        

        

        

        

        try {
          if (typeof(void) == typeof(byte[])) {
            var response = apiInvoker.invokeBinaryAPI(basePath, path, "GET", queryParams, null, headerParams, formParams);
            return ;
          } else {
            var response = apiInvoker.invokeAPI(basePath, path, "PUT", queryParams, Body, headerParams, formParams);
            if(response != null){
               return ;
            }
            else {
              return ;
            }
          }
        } catch (ApiException ex) {
          if(ex.ErrorCode == 404) {
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
      public void  deleteUser (string Username) {
        // create path and map variables
        var path = "/user/{username}".Replace("{format}","json").Replace("{" + "username" + "}", apiInvoker.escapeString(Username.ToString()));

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();
        var formParams = new Dictionary<String, object>();

        

        

        

        

        try {
          if (typeof(void) == typeof(byte[])) {
            var response = apiInvoker.invokeBinaryAPI(basePath, path, "GET", queryParams, null, headerParams, formParams);
            return ;
          } else {
            var response = apiInvoker.invokeAPI(basePath, path, "DELETE", queryParams, null, headerParams, formParams);
            if(response != null){
               return ;
            }
            else {
              return ;
            }
          }
        } catch (ApiException ex) {
          if(ex.ErrorCode == 404) {
          	return ;
          }
          else {
            throw ex;
          }
        }
      }
      
    }
    
  }
