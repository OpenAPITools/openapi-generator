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
     * @param {function} callback the callback function, accepting three arguments: error, data, response
     */
    self.createUser = function(body, callback) {
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

      var handleResponse = null;
      if (callback) {
        handleResponse = function(error, data, response) {
          callback(error, data, response);
        };
      }

      return this.apiClient.callApi(
        '/user', 'POST',
        pathParams, queryParams, headerParams, formParams, postBody,
        contentTypes, accepts, handleResponse
      );
      
    }
    
    /**
     * Creates list of users with given input array
     * 
     * @param {Array}  body List of user object
     * @param {function} callback the callback function, accepting three arguments: error, data, response
     */
    self.createUsersWithArrayInput = function(body, callback) {
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

      var handleResponse = null;
      if (callback) {
        handleResponse = function(error, data, response) {
          callback(error, data, response);
        };
      }

      return this.apiClient.callApi(
        '/user/createWithArray', 'POST',
        pathParams, queryParams, headerParams, formParams, postBody,
        contentTypes, accepts, handleResponse
      );
      
    }
    
    /**
     * Creates list of users with given input array
     * 
     * @param {Array}  body List of user object
     * @param {function} callback the callback function, accepting three arguments: error, data, response
     */
    self.createUsersWithListInput = function(body, callback) {
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

      var handleResponse = null;
      if (callback) {
        handleResponse = function(error, data, response) {
          callback(error, data, response);
        };
      }

      return this.apiClient.callApi(
        '/user/createWithList', 'POST',
        pathParams, queryParams, headerParams, formParams, postBody,
        contentTypes, accepts, handleResponse
      );
      
    }
    
    /**
     * Logs user into the system
     * 
     * @param {String}  username The user name for login
     * @param {String}  password The password for login in clear text
     * @param {function} callback the callback function, accepting three arguments: error, data, response
     *   data is of type: String
     */
    self.loginUser = function(username, password, callback) {
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

      var handleResponse = null;
      if (callback) {
        handleResponse = function(error, data, response) {
          callback(error, data, response);
        };
      }

      return this.apiClient.callApi(
        '/user/login', 'GET',
        pathParams, queryParams, headerParams, formParams, postBody,
        contentTypes, accepts, handleResponse
      );
      
    }
    
    /**
     * Logs out current logged in user session
     * 
     * @param {function} callback the callback function, accepting three arguments: error, data, response
     */
    self.logoutUser = function(callback) {
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

      var handleResponse = null;
      if (callback) {
        handleResponse = function(error, data, response) {
          callback(error, data, response);
        };
      }

      return this.apiClient.callApi(
        '/user/logout', 'GET',
        pathParams, queryParams, headerParams, formParams, postBody,
        contentTypes, accepts, handleResponse
      );
      
    }
    
    /**
     * Get user by user name
     * 
     * @param {String}  username The name that needs to be fetched. Use user1 for testing.
     * @param {function} callback the callback function, accepting three arguments: error, data, response
     *   data is of type: User
     */
    self.getUserByName = function(username, callback) {
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

      var handleResponse = null;
      if (callback) {
        handleResponse = function(error, data, response) {
          if (!error && data) {
            var result = new User();
            result.constructFromObject(data);
            callback(error, result, response);
          } else {
            callback(error, data, response);
          }
        };
      }

      return this.apiClient.callApi(
        '/user/{username}', 'GET',
        pathParams, queryParams, headerParams, formParams, postBody,
        contentTypes, accepts, handleResponse
      );
      
    }
    
    /**
     * Updated user
     * This can only be done by the logged in user.
     * @param {String}  username name that need to be deleted
     * @param {User}  body Updated user object
     * @param {function} callback the callback function, accepting three arguments: error, data, response
     */
    self.updateUser = function(username, body, callback) {
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

      var handleResponse = null;
      if (callback) {
        handleResponse = function(error, data, response) {
          callback(error, data, response);
        };
      }

      return this.apiClient.callApi(
        '/user/{username}', 'PUT',
        pathParams, queryParams, headerParams, formParams, postBody,
        contentTypes, accepts, handleResponse
      );
      
    }
    
    /**
     * Delete user
     * This can only be done by the logged in user.
     * @param {String}  username The name that needs to be deleted
     * @param {function} callback the callback function, accepting three arguments: error, data, response
     */
    self.deleteUser = function(username, callback) {
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

      var handleResponse = null;
      if (callback) {
        handleResponse = function(error, data, response) {
          callback(error, data, response);
        };
      }

      return this.apiClient.callApi(
        '/user/{username}', 'DELETE',
        pathParams, queryParams, headerParams, formParams, postBody,
        contentTypes, accepts, handleResponse
      );
      
    }
    
    
  };

  return UserApi;
}));
