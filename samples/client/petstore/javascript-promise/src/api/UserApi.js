(function(root, factory) {
  if (typeof define === 'function' && define.amd) {
    // AMD. Register as an anonymous module.
    define(['../ApiClient', '../model/User'], factory);
  } else if (typeof module === 'object' && module.exports) {
    // CommonJS-like environments that support module.exports, like Node.
    module.exports = factory(require('../ApiClient'), require('../model/User'));
  } else {
    // Browser globals (root is window)
    if (!root.SwaggerPetstore) {
      root.SwaggerPetstore = {};
    }
    root.SwaggerPetstore.UserApi = factory(root.SwaggerPetstore.ApiClient, root.SwaggerPetstore.User);
  }
}(this, function(ApiClient, User) {
  'use strict';

  var UserApi = function UserApi(apiClient) {
    this.apiClient = apiClient || ApiClient.default;

    var self = this;
    
    
    /**
     * Create user
     * This can only be done by the logged in user.
     * @param {User}  body Created user object
     
     */
    self.createUser = function(body) {
      var postBody = body;
      

      
      var pathParams = {
      };
      var queryParams = {
      };
      var headerParams = {
      };
      var formParams = {
      };

      var contentTypes = [];
      var accepts = ['application/json', 'application/xml'];
      var returnType = null;

      return this.apiClient.callApi(
        '/user', 'POST',
        pathParams, queryParams, headerParams, formParams, postBody,
        contentTypes, accepts, returnType
      );
      
    }
    
    /**
     * Creates list of users with given input array
     * 
     * @param {[User]}  body List of user object
     
     */
    self.createUsersWithArrayInput = function(body) {
      var postBody = body;
      

      
      var pathParams = {
      };
      var queryParams = {
      };
      var headerParams = {
      };
      var formParams = {
      };

      var contentTypes = [];
      var accepts = ['application/json', 'application/xml'];
      var returnType = null;

      return this.apiClient.callApi(
        '/user/createWithArray', 'POST',
        pathParams, queryParams, headerParams, formParams, postBody,
        contentTypes, accepts, returnType
      );
      
    }
    
    /**
     * Creates list of users with given input array
     * 
     * @param {[User]}  body List of user object
     
     */
    self.createUsersWithListInput = function(body) {
      var postBody = body;
      

      
      var pathParams = {
      };
      var queryParams = {
      };
      var headerParams = {
      };
      var formParams = {
      };

      var contentTypes = [];
      var accepts = ['application/json', 'application/xml'];
      var returnType = null;

      return this.apiClient.callApi(
        '/user/createWithList', 'POST',
        pathParams, queryParams, headerParams, formParams, postBody,
        contentTypes, accepts, returnType
      );
      
    }
    
    /**
     * Logs user into the system
     * 
     * @param {String}  username The user name for login
     * @param {String}  password The password for login in clear text
     
     *   data is of type: 'String'
     */
    self.loginUser = function(username, password) {
      var postBody = null;
      

      
      var pathParams = {
      };
      var queryParams = {
        'username': username,
        'password': password
      };
      var headerParams = {
      };
      var formParams = {
      };

      var contentTypes = [];
      var accepts = ['application/json', 'application/xml'];
      var returnType = 'String';

      return this.apiClient.callApi(
        '/user/login', 'GET',
        pathParams, queryParams, headerParams, formParams, postBody,
        contentTypes, accepts, returnType
      );
      
    }
    
    /**
     * Logs out current logged in user session
     * 
     
     */
    self.logoutUser = function() {
      var postBody = null;
      

      
      var pathParams = {
      };
      var queryParams = {
      };
      var headerParams = {
      };
      var formParams = {
      };

      var contentTypes = [];
      var accepts = ['application/json', 'application/xml'];
      var returnType = null;

      return this.apiClient.callApi(
        '/user/logout', 'GET',
        pathParams, queryParams, headerParams, formParams, postBody,
        contentTypes, accepts, returnType
      );
      
    }
    
    /**
     * Get user by user name
     * 
     * @param {String}  username The name that needs to be fetched. Use user1 for testing.
     
     *   data is of type: User
     */
    self.getUserByName = function(username) {
      var postBody = null;
      
      // verify the required parameter 'username' is set
      if (username == null) {
        throw "Missing the required parameter 'username' when calling getUserByName";
      }
      

      
      var pathParams = {
        'username': username
      };
      var queryParams = {
      };
      var headerParams = {
      };
      var formParams = {
      };

      var contentTypes = [];
      var accepts = ['application/json', 'application/xml'];
      var returnType = User;

      return this.apiClient.callApi(
        '/user/{username}', 'GET',
        pathParams, queryParams, headerParams, formParams, postBody,
        contentTypes, accepts, returnType
      );
      
    }
    
    /**
     * Updated user
     * This can only be done by the logged in user.
     * @param {String}  username name that need to be deleted
     * @param {User}  body Updated user object
     
     */
    self.updateUser = function(username, body) {
      var postBody = body;
      
      // verify the required parameter 'username' is set
      if (username == null) {
        throw "Missing the required parameter 'username' when calling updateUser";
      }
      

      
      var pathParams = {
        'username': username
      };
      var queryParams = {
      };
      var headerParams = {
      };
      var formParams = {
      };

      var contentTypes = [];
      var accepts = ['application/json', 'application/xml'];
      var returnType = null;

      return this.apiClient.callApi(
        '/user/{username}', 'PUT',
        pathParams, queryParams, headerParams, formParams, postBody,
        contentTypes, accepts, returnType
      );
      
    }
    
    /**
     * Delete user
     * This can only be done by the logged in user.
     * @param {String}  username The name that needs to be deleted
     
     */
    self.deleteUser = function(username) {
      var postBody = null;
      
      // verify the required parameter 'username' is set
      if (username == null) {
        throw "Missing the required parameter 'username' when calling deleteUser";
      }
      

      
      var pathParams = {
        'username': username
      };
      var queryParams = {
      };
      var headerParams = {
      };
      var formParams = {
      };

      var contentTypes = [];
      var accepts = ['application/json', 'application/xml'];
      var returnType = null;

      return this.apiClient.callApi(
        '/user/{username}', 'DELETE',
        pathParams, queryParams, headerParams, formParams, postBody,
        contentTypes, accepts, returnType
      );
      
    }
    
    
  };

  return UserApi;
}));
