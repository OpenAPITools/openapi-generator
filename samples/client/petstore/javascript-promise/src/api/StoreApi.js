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

  /**
   * Store service.
   * @module api/StoreApi
   * @version 1.0.0
   */

  /**
   * Constructs a new StoreApi. 
   * @alias module:api/StoreApi
   * @class
   * @param {module:ApiClient} apiClient Optional API client implementation to use, default to {@link module:ApiClient#instance}
   * if unspecified.
   */
  var exports = function(apiClient) {
    this.apiClient = apiClient || ApiClient.instance;



    /**
     * Delete purchase order by ID
     * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
     * @param {String} orderId ID of the order that needs to be deleted
     */
    this.deleteOrder = function(orderId) {
      var postBody = null;

      // verify the required parameter 'orderId' is set
      if (orderId == undefined || orderId == null) {
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

      var authNames = [];
      var contentTypes = [];
      var accepts = ['application/json', 'application/xml'];
      var returnType = null;

      return this.apiClient.callApi(
        '/store/order/{orderId}', 'DELETE',
        pathParams, queryParams, headerParams, formParams, postBody,
        authNames, contentTypes, accepts, returnType
      );
    }


    /**
     * Finds orders by status
     * A single status value can be provided as a string
     * @param {Object} opts Optional parameters
     * @param {module:model/String} opts.status Status value that needs to be considered for query (default to placed)
     * data is of type: {Array.<module:model/Order>}
     */
    this.findOrdersByStatus = function(opts) {
      opts = opts || {};
      var postBody = null;


      var pathParams = {
      };
      var queryParams = {
        'status': opts['status']
      };
      var headerParams = {
      };
      var formParams = {
      };

      var authNames = ['test_api_client_id', 'test_api_client_secret'];
      var contentTypes = [];
      var accepts = ['application/json', 'application/xml'];
      var returnType = [Order];

      return this.apiClient.callApi(
        '/store/findByStatus', 'GET',
        pathParams, queryParams, headerParams, formParams, postBody,
        authNames, contentTypes, accepts, returnType
      );
    }


    /**
     * Returns pet inventories by status
     * Returns a map of status codes to quantities
     * data is of type: {Object.<String, {'String': 'Integer'}>}
     */
    this.getInventory = function() {
      var postBody = null;


      var pathParams = {
      };
      var queryParams = {
      };
      var headerParams = {
      };
      var formParams = {
      };

      var authNames = ['api_key'];
      var contentTypes = [];
      var accepts = ['application/json', 'application/xml'];
      var returnType = {'String': 'Integer'};

      return this.apiClient.callApi(
        '/store/inventory', 'GET',
        pathParams, queryParams, headerParams, formParams, postBody,
        authNames, contentTypes, accepts, returnType
      );
    }


    /**
     * Fake endpoint to test arbitrary object return by &#39;Get inventory&#39;
     * Returns an arbitrary object which is actually a map of status codes to quantities
     * data is of type: {Object}
     */
    this.getInventoryInObject = function() {
      var postBody = null;


      var pathParams = {
      };
      var queryParams = {
      };
      var headerParams = {
      };
      var formParams = {
      };

      var authNames = ['api_key'];
      var contentTypes = [];
      var accepts = ['application/json', 'application/xml'];
      var returnType = Object;

      return this.apiClient.callApi(
        '/store/inventory?response=arbitrary_object', 'GET',
        pathParams, queryParams, headerParams, formParams, postBody,
        authNames, contentTypes, accepts, returnType
      );
    }


    /**
     * Find purchase order by ID
     * For valid response try integer IDs with value &lt;= 5 or &gt; 10. Other values will generated exceptions
     * @param {String} orderId ID of pet that needs to be fetched
     * data is of type: {module:model/Order}
     */
    this.getOrderById = function(orderId) {
      var postBody = null;

      // verify the required parameter 'orderId' is set
      if (orderId == undefined || orderId == null) {
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

      var authNames = ['test_api_key_header', 'test_api_key_query'];
      var contentTypes = [];
      var accepts = ['application/json', 'application/xml'];
      var returnType = Order;

      return this.apiClient.callApi(
        '/store/order/{orderId}', 'GET',
        pathParams, queryParams, headerParams, formParams, postBody,
        authNames, contentTypes, accepts, returnType
      );
    }


    /**
     * Place an order for a pet
     * 
     * @param {Object} opts Optional parameters
     * @param {module:model/Order} opts.body order placed for purchasing the pet
     * data is of type: {module:model/Order}
     */
    this.placeOrder = function(opts) {
      opts = opts || {};
      var postBody = opts['body'];


      var pathParams = {
      };
      var queryParams = {
      };
      var headerParams = {
      };
      var formParams = {
      };

      var authNames = ['test_api_client_id', 'test_api_client_secret'];
      var contentTypes = [];
      var accepts = ['application/json', 'application/xml'];
      var returnType = Order;

      return this.apiClient.callApi(
        '/store/order', 'POST',
        pathParams, queryParams, headerParams, formParams, postBody,
        authNames, contentTypes, accepts, returnType
      );
    }
  };

  return exports;
}));
