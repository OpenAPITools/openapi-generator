(function(root, factory) {
  if (typeof define === 'function' && define.amd) {
    // AMD. Register as an anonymous module.
    define(['../ApiClient', '../model/Pet', '../model/InlineResponse200'], factory);
  } else if (typeof module === 'object' && module.exports) {
    // CommonJS-like environments that support module.exports, like Node.
    module.exports = factory(require('../ApiClient'), require('../model/Pet'), require('../model/InlineResponse200'));
  } else {
    // Browser globals (root is window)
    if (!root.SwaggerPetstore) {
      root.SwaggerPetstore = {};
    }
    root.SwaggerPetstore.PetApi = factory(root.SwaggerPetstore.ApiClient, root.SwaggerPetstore.Pet, root.SwaggerPetstore.InlineResponse200);
  }
}(this, function(ApiClient, Pet, InlineResponse200) {
  'use strict';

  /**
   * Pet service.
   * @module api/PetApi
   * @version 1.0.0
   */

  /**
   * Constructs a new PetApi. 
   * @alias module:api/PetApi
   * @class
   * @param {module:ApiClient} apiClient Optional API client implementation to use, default to {@link module:ApiClient#instance}
   * if unspecified.
   */
  var exports = function(apiClient) {
    this.apiClient = apiClient || ApiClient.instance;



    /**
     * Add a new pet to the store
     * 
     * @param {Object} opts Optional parameters
     * @param {module:model/Pet} opts.body Pet object that needs to be added to the store
     */
    this.addPet = function(opts) {
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

      var authNames = ['petstore_auth'];
      var contentTypes = ['application/json', 'application/xml'];
      var accepts = ['application/json', 'application/xml'];
      var returnType = null;

      return this.apiClient.callApi(
        '/pet', 'POST',
        pathParams, queryParams, headerParams, formParams, postBody,
        authNames, contentTypes, accepts, returnType
      );
    }


    /**
     * Fake endpoint to test byte array in body parameter for adding a new pet to the store
     * 
     * @param {Object} opts Optional parameters
     * @param {String} opts.body Pet object in the form of byte array
     */
    this.addPetUsingByteArray = function(opts) {
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

      var authNames = ['petstore_auth'];
      var contentTypes = ['application/json', 'application/xml'];
      var accepts = ['application/json', 'application/xml'];
      var returnType = null;

      return this.apiClient.callApi(
        '/pet?testing_byte_array=true', 'POST',
        pathParams, queryParams, headerParams, formParams, postBody,
        authNames, contentTypes, accepts, returnType
      );
    }


    /**
     * Deletes a pet
     * 
     * @param {Integer} petId Pet id to delete
     * @param {Object} opts Optional parameters
     * @param {String} opts.apiKey 
     */
    this.deletePet = function(petId, opts) {
      opts = opts || {};
      var postBody = null;

      // verify the required parameter 'petId' is set
      if (petId == undefined || petId == null) {
        throw "Missing the required parameter 'petId' when calling deletePet";
      }


      var pathParams = {
        'petId': petId
      };
      var queryParams = {
      };
      var headerParams = {
        'api_key': opts['apiKey']
      };
      var formParams = {
      };

      var authNames = ['petstore_auth'];
      var contentTypes = [];
      var accepts = ['application/json', 'application/xml'];
      var returnType = null;

      return this.apiClient.callApi(
        '/pet/{petId}', 'DELETE',
        pathParams, queryParams, headerParams, formParams, postBody,
        authNames, contentTypes, accepts, returnType
      );
    }


    /**
     * Finds Pets by status
     * Multiple status values can be provided with comma separated strings
     * @param {Object} opts Optional parameters
     * @param {Array.<String>} opts.status Status values that need to be considered for query (default to available)
     * data is of type: {Array.<module:model/Pet>}
     */
    this.findPetsByStatus = function(opts) {
      opts = opts || {};
      var postBody = null;


      var pathParams = {
      };
      var queryParams = {
        'status': this.apiClient.buildCollectionParam(opts['status'], 'multi')
      };
      var headerParams = {
      };
      var formParams = {
      };

      var authNames = ['petstore_auth'];
      var contentTypes = [];
      var accepts = ['application/json', 'application/xml'];
      var returnType = [Pet];

      return this.apiClient.callApi(
        '/pet/findByStatus', 'GET',
        pathParams, queryParams, headerParams, formParams, postBody,
        authNames, contentTypes, accepts, returnType
      );
    }


    /**
     * Finds Pets by tags
     * Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
     * @param {Object} opts Optional parameters
     * @param {Array.<String>} opts.tags Tags to filter by
     * data is of type: {Array.<module:model/Pet>}
     */
    this.findPetsByTags = function(opts) {
      opts = opts || {};
      var postBody = null;


      var pathParams = {
      };
      var queryParams = {
        'tags': this.apiClient.buildCollectionParam(opts['tags'], 'multi')
      };
      var headerParams = {
      };
      var formParams = {
      };

      var authNames = ['petstore_auth'];
      var contentTypes = [];
      var accepts = ['application/json', 'application/xml'];
      var returnType = [Pet];

      return this.apiClient.callApi(
        '/pet/findByTags', 'GET',
        pathParams, queryParams, headerParams, formParams, postBody,
        authNames, contentTypes, accepts, returnType
      );
    }


    /**
     * Find pet by ID
     * Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
     * @param {Integer} petId ID of pet that needs to be fetched
     * data is of type: {module:model/Pet}
     */
    this.getPetById = function(petId) {
      var postBody = null;

      // verify the required parameter 'petId' is set
      if (petId == undefined || petId == null) {
        throw "Missing the required parameter 'petId' when calling getPetById";
      }


      var pathParams = {
        'petId': petId
      };
      var queryParams = {
      };
      var headerParams = {
      };
      var formParams = {
      };

      var authNames = ['api_key', 'petstore_auth'];
      var contentTypes = [];
      var accepts = ['application/json', 'application/xml'];
      var returnType = Pet;

      return this.apiClient.callApi(
        '/pet/{petId}', 'GET',
        pathParams, queryParams, headerParams, formParams, postBody,
        authNames, contentTypes, accepts, returnType
      );
    }


    /**
     * Fake endpoint to test inline arbitrary object return by &#39;Find pet by ID&#39;
     * Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
     * @param {Integer} petId ID of pet that needs to be fetched
     * data is of type: {module:model/InlineResponse200}
     */
    this.getPetByIdInObject = function(petId) {
      var postBody = null;

      // verify the required parameter 'petId' is set
      if (petId == undefined || petId == null) {
        throw "Missing the required parameter 'petId' when calling getPetByIdInObject";
      }


      var pathParams = {
        'petId': petId
      };
      var queryParams = {
      };
      var headerParams = {
      };
      var formParams = {
      };

      var authNames = ['api_key', 'petstore_auth'];
      var contentTypes = [];
      var accepts = ['application/json', 'application/xml'];
      var returnType = InlineResponse200;

      return this.apiClient.callApi(
        '/pet/{petId}?response=inline_arbitrary_object', 'GET',
        pathParams, queryParams, headerParams, formParams, postBody,
        authNames, contentTypes, accepts, returnType
      );
    }


    /**
     * Fake endpoint to test byte array return by &#39;Find pet by ID&#39;
     * Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
     * @param {Integer} petId ID of pet that needs to be fetched
     * data is of type: {'String'}
     */
    this.petPetIdtestingByteArraytrueGet = function(petId) {
      var postBody = null;

      // verify the required parameter 'petId' is set
      if (petId == undefined || petId == null) {
        throw "Missing the required parameter 'petId' when calling petPetIdtestingByteArraytrueGet";
      }


      var pathParams = {
        'petId': petId
      };
      var queryParams = {
      };
      var headerParams = {
      };
      var formParams = {
      };

      var authNames = ['api_key', 'petstore_auth'];
      var contentTypes = [];
      var accepts = ['application/json', 'application/xml'];
      var returnType = 'String';

      return this.apiClient.callApi(
        '/pet/{petId}?testing_byte_array=true', 'GET',
        pathParams, queryParams, headerParams, formParams, postBody,
        authNames, contentTypes, accepts, returnType
      );
    }


    /**
     * Update an existing pet
     * 
     * @param {Object} opts Optional parameters
     * @param {module:model/Pet} opts.body Pet object that needs to be added to the store
     */
    this.updatePet = function(opts) {
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

      var authNames = ['petstore_auth'];
      var contentTypes = ['application/json', 'application/xml'];
      var accepts = ['application/json', 'application/xml'];
      var returnType = null;

      return this.apiClient.callApi(
        '/pet', 'PUT',
        pathParams, queryParams, headerParams, formParams, postBody,
        authNames, contentTypes, accepts, returnType
      );
    }


    /**
     * Updates a pet in the store with form data
     * 
     * @param {String} petId ID of pet that needs to be updated
     * @param {Object} opts Optional parameters
     * @param {String} opts.name Updated name of the pet
     * @param {String} opts.status Updated status of the pet
     */
    this.updatePetWithForm = function(petId, opts) {
      opts = opts || {};
      var postBody = null;

      // verify the required parameter 'petId' is set
      if (petId == undefined || petId == null) {
        throw "Missing the required parameter 'petId' when calling updatePetWithForm";
      }


      var pathParams = {
        'petId': petId
      };
      var queryParams = {
      };
      var headerParams = {
      };
      var formParams = {
        'name': opts['name'],
        'status': opts['status']
      };

      var authNames = ['petstore_auth'];
      var contentTypes = ['application/x-www-form-urlencoded'];
      var accepts = ['application/json', 'application/xml'];
      var returnType = null;

      return this.apiClient.callApi(
        '/pet/{petId}', 'POST',
        pathParams, queryParams, headerParams, formParams, postBody,
        authNames, contentTypes, accepts, returnType
      );
    }


    /**
     * uploads an image
     * 
     * @param {Integer} petId ID of pet to update
     * @param {Object} opts Optional parameters
     * @param {String} opts.additionalMetadata Additional data to pass to server
     * @param {File} opts.file file to upload
     */
    this.uploadFile = function(petId, opts) {
      opts = opts || {};
      var postBody = null;

      // verify the required parameter 'petId' is set
      if (petId == undefined || petId == null) {
        throw "Missing the required parameter 'petId' when calling uploadFile";
      }


      var pathParams = {
        'petId': petId
      };
      var queryParams = {
      };
      var headerParams = {
      };
      var formParams = {
        'additionalMetadata': opts['additionalMetadata'],
        'file': opts['file']
      };

      var authNames = ['petstore_auth'];
      var contentTypes = ['multipart/form-data'];
      var accepts = ['application/json', 'application/xml'];
      var returnType = null;

      return this.apiClient.callApi(
        '/pet/{petId}/uploadImage', 'POST',
        pathParams, queryParams, headerParams, formParams, postBody,
        authNames, contentTypes, accepts, returnType
      );
    }
  };

  return exports;
}));
