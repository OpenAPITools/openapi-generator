(function(root, factory) {
  if (typeof define === 'function' && define.amd) {
    // AMD. Register as an anonymous module.
    define(['ApiClient', 'model/User'], factory);
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

  /**
   * User service.
   * @module api/UserApi
   * @version 1.0.0
   */

  /**
   * Constructs a new UserApi. 
   * @alias module:api/UserApi
   * @class
   * @param {module:ApiClient} apiClient Optional API client implementation to use,
   * default to {@link module:ApiClient#instance} if unspecified.
   */
  var exports = function(apiClient) {
    this.apiClient = apiClient || ApiClient.instance;



    /**
     * Create user
     * This can only be done by the logged in user.
     * @param {module:model/User} body Created user object
     */
    this.createUser = function(body) {
      var postBody = body;

      // verify the required parameter 'body' is set
      if (body == undefined || body == null) {
        throw "Missing the required parameter 'body' when calling createUser";
      }


      var pathParams = {
      };
      var queryParams = {
      };
      var headerParams = {
      };
      var formParams = {
      };

      var authNames = [];
      var contentTypes = [];
      var accepts = ['application/xml', 'application/json'];
      var returnType = null;

      return this.apiClient.callApi(
        '/user', 'POST',
        pathParams, queryParams, headerParams, formParams, postBody,
        authNames, contentTypes, accepts, returnType
      );
    }


    /**
     * Creates list of users with given input array
     * 
     * @param {Array.<module:model/User>} body List of user object
     */
    this.createUsersWithArrayInput = function(body) {
      var postBody = body;

      // verify the required parameter 'body' is set
      if (body == undefined || body == null) {
        throw "Missing the required parameter 'body' when calling createUsersWithArrayInput";
      }


      var pathParams = {
      };
      var queryParams = {
      };
      var headerParams = {
      };
      var formParams = {
      };

      var authNames = [];
      var contentTypes = [];
      var accepts = ['application/xml', 'application/json'];
      var returnType = null;

      return this.apiClient.callApi(
        '/user/createWithArray', 'POST',
        pathParams, queryParams, headerParams, formParams, postBody,
        authNames, contentTypes, accepts, returnType
      );
    }


    /**
     * Creates list of users with given input array
     * 
     * @param {Array.<module:model/User>} body List of user object
     */
    this.createUsersWithListInput = function(body) {
      var postBody = body;

      // verify the required parameter 'body' is set
      if (body == undefined || body == null) {
        throw "Missing the required parameter 'body' when calling createUsersWithListInput";
      }


      var pathParams = {
      };
      var queryParams = {
      };
      var headerParams = {
      };
      var formParams = {
      };

      var authNames = [];
      var contentTypes = [];
      var accepts = ['application/xml', 'application/json'];
      var returnType = null;

      return this.apiClient.callApi(
        '/user/createWithList', 'POST',
        pathParams, queryParams, headerParams, formParams, postBody,
        authNames, contentTypes, accepts, returnType
      );
    }


    /**
     * Delete user
     * This can only be done by the logged in user.
     * @param {String} username The name that needs to be deleted
     */
    this.deleteUser = function(username) {
      var postBody = null;

      // verify the required parameter 'username' is set
      if (username == undefined || username == null) {
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

      var authNames = [];
      var contentTypes = [];
      var accepts = ['application/xml', 'application/json'];
      var returnType = null;

      return this.apiClient.callApi(
        '/user/{username}', 'DELETE',
        pathParams, queryParams, headerParams, formParams, postBody,
        authNames, contentTypes, accepts, returnType
      );
    }


    /**
     * Get user by user name
     * 
     * @param {String} username The name that needs to be fetched. Use user1 for testing. 
     * data is of type: {module:model/User}
     */
    this.getUserByName = function(username) {
      var postBody = null;

      // verify the required parameter 'username' is set
      if (username == undefined || username == null) {
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

      var authNames = [];
      var contentTypes = [];
      var accepts = ['application/xml', 'application/json'];
      var returnType = User;

      return this.apiClient.callApi(
        '/user/{username}', 'GET',
        pathParams, queryParams, headerParams, formParams, postBody,
        authNames, contentTypes, accepts, returnType
      );
    }


    /**
     * Logs user into the system
     * 
     * @param {String} username The user name for login
     * @param {String} password The password for login in clear text
     * data is of type: {'String'}
     */
    this.loginUser = function(username, password) {
      var postBody = null;

      // verify the required parameter 'username' is set
      if (username == undefined || username == null) {
        throw "Missing the required parameter 'username' when calling loginUser";
      }

      // verify the required parameter 'password' is set
      if (password == undefined || password == null) {
        throw "Missing the required parameter 'password' when calling loginUser";
      }


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

      var authNames = [];
      var contentTypes = [];
      var accepts = ['application/xml', 'application/json'];
      var returnType = 'String';

      return this.apiClient.callApi(
        '/user/login', 'GET',
        pathParams, queryParams, headerParams, formParams, postBody,
        authNames, contentTypes, accepts, returnType
      );
    }


    /**
     * Logs out current logged in user session
     * 
     */
    this.logoutUser = function() {
      var postBody = null;


      var pathParams = {
      };
      var queryParams = {
      };
      var headerParams = {
      };
      var formParams = {
      };

      var authNames = [];
      var contentTypes = [];
      var accepts = ['application/xml', 'application/json'];
      var returnType = null;

      return this.apiClient.callApi(
        '/user/logout', 'GET',
        pathParams, queryParams, headerParams, formParams, postBody,
        authNames, contentTypes, accepts, returnType
      );
    }


    /**
     * Updated user
     * This can only be done by the logged in user.
     * @param {String} username name that need to be deleted
     * @param {module:model/User} body Updated user object
     */
    this.updateUser = function(username, body) {
      var postBody = body;

      // verify the required parameter 'username' is set
      if (username == undefined || username == null) {
        throw "Missing the required parameter 'username' when calling updateUser";
      }

      // verify the required parameter 'body' is set
      if (body == undefined || body == null) {
        throw "Missing the required parameter 'body' when calling updateUser";
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

      var authNames = [];
      var contentTypes = [];
      var accepts = ['application/xml', 'application/json'];
      var returnType = null;

      return this.apiClient.callApi(
        '/user/{username}', 'PUT',
        pathParams, queryParams, headerParams, formParams, postBody,
        authNames, contentTypes, accepts, returnType
      );
    }
  };

  return exports;
}));
