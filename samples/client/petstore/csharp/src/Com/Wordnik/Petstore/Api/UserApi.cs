  using System;
  using System.Collections.Generic;
  using Com.Wordnik.Petstore;
  using Com.Wordnik.Petstore.Model;
  namespace Com.Wordnik.Petstore.Api {
    public class UserApi {
      string basePath = "http://petstore.swagger.wordnik.com/api";
      private readonly ApiInvoker apiInvoker = ApiInvoker.GetInstance();

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
      /// Delete user This can only be done by the logged in user.
      /// </summary>
      /// <param name="username">The name that needs to be deleted</param>
      /// <returns></returns>
      public void deleteUser (string username) {
        // create path and map variables
        var path = "/user/{username}".Replace("{format}","json").Replace("{" + "username" + "}", apiInvoker.escapeString(username.ToString()));

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();

        // verify required params are set
        if (username == null ) {
           throw new ApiException(400, "missing required params");
        }
        string paramStr = null;
        try {
          var response = apiInvoker.invokeAPI(basePath, path, "DELETE", queryParams, null, headerParams);
          if(response != null){
             return ;
          }
          else {
            return ;
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
      /// <param name="username">The name that needs to be fetched. Use user1 for testing.</param>
      /// <returns></returns>
      public User getUserByName (string username) {
        // create path and map variables
        var path = "/user/{username}".Replace("{format}","json").Replace("{" + "username" + "}", apiInvoker.escapeString(username.ToString()));

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();

        // verify required params are set
        if (username == null ) {
           throw new ApiException(400, "missing required params");
        }
        string paramStr = null;
        try {
          var response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, null, headerParams);
          if(response != null){
             return (User) ApiInvoker.deserialize(response, typeof(User));
          }
          else {
            return null;
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
      /// <param name="username">name that need to be deleted</param>
      /// <param name="body">Updated user object</param>
      /// <returns></returns>
      public void updateUser (string username, User body) {
        // create path and map variables
        var path = "/user/{username}".Replace("{format}","json").Replace("{" + "username" + "}", apiInvoker.escapeString(username.ToString()));

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();

        // verify required params are set
        if (username == null || body == null ) {
           throw new ApiException(400, "missing required params");
        }
        string paramStr = null;
        try {
          var response = apiInvoker.invokeAPI(basePath, path, "PUT", queryParams, body, headerParams);
          if(response != null){
             return ;
          }
          else {
            return ;
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
      /// <param name="username">The user name for login</param>
      /// <param name="password">The password for login in clear text</param>
      /// <returns></returns>
      public string loginUser (string username, string password) {
        // create path and map variables
        var path = "/user/login".Replace("{format}","json");

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();

        // verify required params are set
        if (username == null || password == null ) {
           throw new ApiException(400, "missing required params");
        }
        string paramStr = null;
        if (username != null){
          paramStr = (username != null && username is DateTime) ? ((DateTime)(object)username).ToString("u") : Convert.ToString(username);
          queryParams.Add("username", paramStr);
		}
        if (password != null){
          paramStr = (password != null && password is DateTime) ? ((DateTime)(object)password).ToString("u") : Convert.ToString(password);
          queryParams.Add("password", paramStr);
		}
        try {
          var response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, null, headerParams);
          if(response != null){
             return (string) ApiInvoker.deserialize(response, typeof(string));
          }
          else {
            return null;
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
      public void logoutUser () {
        // create path and map variables
        var path = "/user/logout".Replace("{format}","json");

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();

        string paramStr = null;
        try {
          var response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, null, headerParams);
          if(response != null){
             return ;
          }
          else {
            return ;
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
      /// Create user This can only be done by the logged in user.
      /// </summary>
      /// <param name="body">Created user object</param>
      /// <returns></returns>
      public void createUser (User body) {
        // create path and map variables
        var path = "/user".Replace("{format}","json");

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();

        // verify required params are set
        if (body == null ) {
           throw new ApiException(400, "missing required params");
        }
        string paramStr = null;
        try {
          var response = apiInvoker.invokeAPI(basePath, path, "POST", queryParams, body, headerParams);
          if(response != null){
             return ;
          }
          else {
            return ;
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
      /// <param name="body">List of user object</param>
      /// <returns></returns>
      public void createUsersWithArrayInput (List<User> body) {
        // create path and map variables
        var path = "/user/createWithArray".Replace("{format}","json");

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();

        // verify required params are set
        if (body == null ) {
           throw new ApiException(400, "missing required params");
        }
        string paramStr = null;
        try {
          var response = apiInvoker.invokeAPI(basePath, path, "POST", queryParams, body, headerParams);
          if(response != null){
             return ;
          }
          else {
            return ;
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
      /// Creates list of users with given list input 
      /// </summary>
      /// <param name="body">List of user object</param>
      /// <returns></returns>
      public void createUsersWithListInput (List<User> body) {
        // create path and map variables
        var path = "/user/createWithList".Replace("{format}","json");

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();

        // verify required params are set
        if (body == null ) {
           throw new ApiException(400, "missing required params");
        }
        string paramStr = null;
        try {
          var response = apiInvoker.invokeAPI(basePath, path, "POST", queryParams, body, headerParams);
          if(response != null){
             return ;
          }
          else {
            return ;
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
