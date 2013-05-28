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
      
      public void setBasePath(string basePath) {
        this.basePath = basePath;
      }
      
      public String getBasePath() {
        return basePath;
      }

      public void createUsersWithArrayInput (List<User> body) {
        // create path and map variables
        var path = "/user.{format}/createWithArray".Replace("{format}","json");

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();

        // verify required params are set
        if (body == null ) {
           throw new ApiException(400, "missing required params");
        }
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
      public void createUser (User body) {
        // create path and map variables
        var path = "/user.{format}".Replace("{format}","json");

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();

        // verify required params are set
        if (body == null ) {
           throw new ApiException(400, "missing required params");
        }
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
      public void createUsersWithListInput (List<User> body) {
        // create path and map variables
        var path = "/user.{format}/createWithList".Replace("{format}","json");

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();

        // verify required params are set
        if (body == null ) {
           throw new ApiException(400, "missing required params");
        }
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
      public void updateUser (string Username, User body) {
        // create path and map variables
        var path = "/user.{format}/{username}".Replace("{format}","json").Replace("{" + "Username" + "}", apiInvoker.escapeString(Username.ToString()));

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();

        // verify required params are set
        if (Username == null || body == null ) {
           throw new ApiException(400, "missing required params");
        }
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
      public void deleteUser (string Username) {
        // create path and map variables
        var path = "/user.{format}/{username}".Replace("{format}","json").Replace("{" + "Username" + "}", apiInvoker.escapeString(Username.ToString()));

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();

        // verify required params are set
        if (Username == null ) {
           throw new ApiException(400, "missing required params");
        }
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
      public User getUserByName (string Username) {
        // create path and map variables
        var path = "/user.{format}/{username}".Replace("{format}","json").Replace("{" + "Username" + "}", apiInvoker.escapeString(Username.ToString()));

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();

        // verify required params are set
        if (Username == null ) {
           throw new ApiException(400, "missing required params");
        }
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
      public string loginUser (string Username, string Password) {
        // create path and map variables
        var path = "/user.{format}/login".Replace("{format}","json");

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();

        // verify required params are set
        if (Username == null || Password == null ) {
           throw new ApiException(400, "missing required params");
        }
        if (Username != null)
          queryParams.Add("Username", Username);
        if (Password != null)
          queryParams.Add("Password", Password);
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
      public void logoutUser () {
        // create path and map variables
        var path = "/user.{format}/logout".Replace("{format}","json");

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();

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
      }
    }
