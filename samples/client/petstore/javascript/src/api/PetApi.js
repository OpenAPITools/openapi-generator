(function(root, factory) {
  if (typeof define === 'function' && define.amd) {
    // AMD. Register as an anonymous module.
    define(['../ApiClient', '../model/Pet'], factory);
  } else if (typeof module === 'object' && module.exports) {
    // CommonJS-like environments that support module.exports, like Node.
    module.exports = factory(require('../ApiClient'), require('../model/Pet'));
  } else {
    // Browser globals (root is window)
    if (!root.SwaggerPetstore) {
      root.SwaggerPetstore = {};
    }
    root.SwaggerPetstore.PetApi = factory(root.SwaggerPetstore.ApiClient, root.SwaggerPetstore.Pet);
  }
}(this, function(ApiClient, Pet) {
  'use strict';

  var PetApi = function PetApi(apiClient) {
    this.apiClient = apiClient || ApiClient.default;

    var self = this;
    
    
    /**
     * Update an existing pet
     * 
     * @param {Pet}  body Pet object that needs to be added to the store
     * @param {function} callback the callback function, accepting three arguments: error, data, response
     */
    self.updatePet = function(body, callback) {
      var postBody = body;
      

      
      var pathParams = {
      };
      var queryParams = {
      };
      var headerParams = {
      };
      var formParams = {
      };

      var contentTypes = ['application/json', 'application/xml'];
      var accepts = ['application/json', 'application/xml'];

      var handleResponse = null;
      if (callback) {
        handleResponse = function(error, data, response) {
          callback(error, data, response);
        };
      }

      return this.apiClient.callApi(
        '/pet', 'PUT',
        pathParams, queryParams, headerParams, formParams, postBody,
        contentTypes, accepts, handleResponse
      );
      
    }
    
    /**
     * Add a new pet to the store
     * 
     * @param {Pet}  body Pet object that needs to be added to the store
     * @param {function} callback the callback function, accepting three arguments: error, data, response
     */
    self.addPet = function(body, callback) {
      var postBody = body;
      

      
      var pathParams = {
      };
      var queryParams = {
      };
      var headerParams = {
      };
      var formParams = {
      };

      var contentTypes = ['application/json', 'application/xml'];
      var accepts = ['application/json', 'application/xml'];

      var handleResponse = null;
      if (callback) {
        handleResponse = function(error, data, response) {
          callback(error, data, response);
        };
      }

      return this.apiClient.callApi(
        '/pet', 'POST',
        pathParams, queryParams, headerParams, formParams, postBody,
        contentTypes, accepts, handleResponse
      );
      
    }
    
    /**
     * Finds Pets by status
     * Multiple status values can be provided with comma seperated strings
     * @param {Array}  status Status values that need to be considered for filter
     * @param {function} callback the callback function, accepting three arguments: error, data, response
     *   data is of type: Array
     */
    self.findPetsByStatus = function(status, callback) {
      var postBody = null;
      

      
      var pathParams = {
      };
      var queryParams = {
        'status': status
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
          // TODO: support deserializing array of models
          callback(error, data, response);
        };
      }

      return this.apiClient.callApi(
        '/pet/findByStatus', 'GET',
        pathParams, queryParams, headerParams, formParams, postBody,
        contentTypes, accepts, handleResponse
      );
      
    }
    
    /**
     * Finds Pets by tags
     * Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
     * @param {Array}  tags Tags to filter by
     * @param {function} callback the callback function, accepting three arguments: error, data, response
     *   data is of type: Array
     */
    self.findPetsByTags = function(tags, callback) {
      var postBody = null;
      

      
      var pathParams = {
      };
      var queryParams = {
        'tags': tags
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
          // TODO: support deserializing array of models
          callback(error, data, response);
        };
      }

      return this.apiClient.callApi(
        '/pet/findByTags', 'GET',
        pathParams, queryParams, headerParams, formParams, postBody,
        contentTypes, accepts, handleResponse
      );
      
    }
    
    /**
     * Find pet by ID
     * Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
     * @param {Integer}  petId ID of pet that needs to be fetched
     * @param {function} callback the callback function, accepting three arguments: error, data, response
     *   data is of type: Pet
     */
    self.getPetById = function(petId, callback) {
      var postBody = null;
      
      // verify the required parameter 'petId' is set
      if (petId == null) {
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

      var contentTypes = [];
      var accepts = ['application/json', 'application/xml'];

      var handleResponse = null;
      if (callback) {
        handleResponse = function(error, data, response) {
          if (!error && data) {
            var result = new Pet();
            result.constructFromObject(data);
            callback(error, result, response);
          } else {
            callback(error, data, response);
          }
        };
      }

      return this.apiClient.callApi(
        '/pet/{petId}', 'GET',
        pathParams, queryParams, headerParams, formParams, postBody,
        contentTypes, accepts, handleResponse
      );
      
    }
    
    /**
     * Updates a pet in the store with form data
     * 
     * @param {String}  petId ID of pet that needs to be updated
     * @param {String}  name Updated name of the pet
     * @param {String}  status Updated status of the pet
     * @param {function} callback the callback function, accepting three arguments: error, data, response
     */
    self.updatePetWithForm = function(petId, name, status, callback) {
      var postBody = null;
      
      // verify the required parameter 'petId' is set
      if (petId == null) {
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
        'name': name,
        'status': status
      };

      var contentTypes = ['application/x-www-form-urlencoded'];
      var accepts = ['application/json', 'application/xml'];

      var handleResponse = null;
      if (callback) {
        handleResponse = function(error, data, response) {
          callback(error, data, response);
        };
      }

      return this.apiClient.callApi(
        '/pet/{petId}', 'POST',
        pathParams, queryParams, headerParams, formParams, postBody,
        contentTypes, accepts, handleResponse
      );
      
    }
    
    /**
     * Deletes a pet
     * 
     * @param {Integer}  petId Pet id to delete
     * @param {String}  apiKey 
     * @param {function} callback the callback function, accepting three arguments: error, data, response
     */
    self.deletePet = function(petId, apiKey, callback) {
      var postBody = null;
      
      // verify the required parameter 'petId' is set
      if (petId == null) {
        throw "Missing the required parameter 'petId' when calling deletePet";
      }
      

      
      var pathParams = {
        'petId': petId
      };
      var queryParams = {
      };
      var headerParams = {
        'api_key': apiKey
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
        '/pet/{petId}', 'DELETE',
        pathParams, queryParams, headerParams, formParams, postBody,
        contentTypes, accepts, handleResponse
      );
      
    }
    
    /**
     * uploads an image
     * 
     * @param {Integer}  petId ID of pet to update
     * @param {String}  additionalMetadata Additional data to pass to server
     * @param {File}  file file to upload
     * @param {function} callback the callback function, accepting three arguments: error, data, response
     */
    self.uploadFile = function(petId, additionalMetadata, file, callback) {
      var postBody = null;
      
      // verify the required parameter 'petId' is set
      if (petId == null) {
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
        'additionalMetadata': additionalMetadata,
        'file': file
      };

      var contentTypes = ['multipart/form-data'];
      var accepts = ['application/json', 'application/xml'];

      var handleResponse = null;
      if (callback) {
        handleResponse = function(error, data, response) {
          callback(error, data, response);
        };
      }

      return this.apiClient.callApi(
        '/pet/{petId}/uploadImage', 'POST',
        pathParams, queryParams, headerParams, formParams, postBody,
        contentTypes, accepts, handleResponse
      );
      
    }
    
    /**
     * Fake endpoint to test byte array return by &#39;Find pet by ID&#39;
     * Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
     * @param {Integer}  petId ID of pet that needs to be fetched
     * @param {function} callback the callback function, accepting three arguments: error, data, response
     *   data is of type: String
     */
    self.getPetByIdWithByteArray = function(petId, callback) {
      var postBody = null;
      
      // verify the required parameter 'petId' is set
      if (petId == null) {
        throw "Missing the required parameter 'petId' when calling getPetByIdWithByteArray";
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

      var contentTypes = [];
      var accepts = ['application/json', 'application/xml'];

      var handleResponse = null;
      if (callback) {
        handleResponse = function(error, data, response) {
          callback(error, data, response);
        };
      }

      return this.apiClient.callApi(
        '/pet/{petId}?testing_byte_array=true', 'GET',
        pathParams, queryParams, headerParams, formParams, postBody,
        contentTypes, accepts, handleResponse
      );
      
    }
    
    /**
     * Fake endpoint to test byte array in body parameter for adding a new pet to the store
     * 
     * @param {String}  body Pet object in the form of byte array
     * @param {function} callback the callback function, accepting three arguments: error, data, response
     */
    self.addPetUsingByteArray = function(body, callback) {
      var postBody = body;
      

      
      var pathParams = {
      };
      var queryParams = {
      };
      var headerParams = {
      };
      var formParams = {
      };

      var contentTypes = ['application/json', 'application/xml'];
      var accepts = ['application/json', 'application/xml'];

      var handleResponse = null;
      if (callback) {
        handleResponse = function(error, data, response) {
          callback(error, data, response);
        };
      }

      return this.apiClient.callApi(
        '/pet?testing_byte_array=true', 'POST',
        pathParams, queryParams, headerParams, formParams, postBody,
        contentTypes, accepts, handleResponse
      );
      
    }
    
    
  };

  return PetApi;
}));
