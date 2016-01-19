(function(root, factory) {
  if (typeof define === 'function' && define.amd) {
    // AMD. Register as an anonymous module.
    define(['../ApiClient', '../model/Order'], factory);
  } else if (typeof module === 'object' && module.exports) {
    // CommonJS-like environments that support module.exports, like Node.
    module.exports = factory(require('../ApiClient'), require('../model/Order'));
  } else {
    // Browser globals (root is window)
    if (!root.SwaggerPetstore) {
      root.SwaggerPetstore = {};
    }
    root.SwaggerPetstore.StoreApi = factory(root.SwaggerPetstore.ApiClient, root.SwaggerPetstore.Order);
  }
}(this, function(ApiClient, Order) {
  'use strict';

  var StoreApi = function StoreApi(apiClient) {
    this.apiClient = apiClient || ApiClient.default;

    var self = this;
    
    
    /**
     * Returns pet inventories by status
     * Returns a map of status codes to quantities
     * @param {function} callback the callback function, accepting three arguments: error, data, response
     *   data is of type: Object<String, Integer>
     */
    self.getInventory = function(callback) {
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
        '/store/inventory', 'GET',
        pathParams, queryParams, headerParams, formParams, postBody,
        contentTypes, accepts, handleResponse
      );
      
    }
    
    /**
     * Place an order for a pet
     * 
     * @param {Order}  body order placed for purchasing the pet
     * @param {function} callback the callback function, accepting three arguments: error, data, response
     *   data is of type: Order
     */
    self.placeOrder = function(body, callback) {
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
          if (!error && data) {
            var result = new Order();
            result.constructFromObject(data);
            callback(error, result, response);
          } else {
            callback(error, data, response);
          }
        };
      }

      return this.apiClient.callApi(
        '/store/order', 'POST',
        pathParams, queryParams, headerParams, formParams, postBody,
        contentTypes, accepts, handleResponse
      );
      
    }
    
    /**
     * Find purchase order by ID
     * For valid response try integer IDs with value &lt;= 5 or &gt; 10. Other values will generated exceptions
     * @param {String}  orderId ID of pet that needs to be fetched
     * @param {function} callback the callback function, accepting three arguments: error, data, response
     *   data is of type: Order
     */
    self.getOrderById = function(orderId, callback) {
      var postBody = null;
      
      // verify the required parameter 'orderId' is set
      if (orderId == null) {
        throw "Missing the required parameter 'orderId' when calling getOrderById";
      }
      

      
      var pathParams = {
        'orderId': orderId
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
            var result = new Order();
            result.constructFromObject(data);
            callback(error, result, response);
          } else {
            callback(error, data, response);
          }
        };
      }

      return this.apiClient.callApi(
        '/store/order/{orderId}', 'GET',
        pathParams, queryParams, headerParams, formParams, postBody,
        contentTypes, accepts, handleResponse
      );
      
    }
    
    /**
     * Delete purchase order by ID
     * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
     * @param {String}  orderId ID of the order that needs to be deleted
     * @param {function} callback the callback function, accepting three arguments: error, data, response
     */
    self.deleteOrder = function(orderId, callback) {
      var postBody = null;
      
      // verify the required parameter 'orderId' is set
      if (orderId == null) {
        throw "Missing the required parameter 'orderId' when calling deleteOrder";
      }
      

      
      var pathParams = {
        'orderId': orderId
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
        '/store/order/{orderId}', 'DELETE',
        pathParams, queryParams, headerParams, formParams, postBody,
        contentTypes, accepts, handleResponse
      );
      
    }
    
    
  };

  return StoreApi;
}));
