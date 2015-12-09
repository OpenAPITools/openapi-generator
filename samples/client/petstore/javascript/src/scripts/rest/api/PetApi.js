/*
 * @javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavascriptClientCodegen", date = "2015-12-09T16:07:21.000+07:00")
 */

//export module
if ( typeof define === "function" && define.amd ) {     
	define(['jquery'], function($) {
        return PetApi;
	 });
}

var PetApi = function PetApi() {
	var self = this;
  
  /**
   * Update an existing pet
   * 
   * @param {Pet}  body Pet object that needs to be added to the store
   * @param {function} callback the callback function
   * @return void
   */
  self.updatePet = function(body, callback) {
    
    var postBody = JSON.stringify(body);
    var postBinaryBody = null;
    
    // create path and map variables
    var basePath = 'http://petstore.swagger.io/v2';
    // if basePath ends with a /, remove it as path starts with a leading /
    if (basePath.substring(basePath.length-1, basePath.length)=='/') {
    	basePath = basePath.substring(0, basePath.length-1);
    }
    
	var path = basePath + replaceAll(replaceAll("/pet", "\\{format\\}","json"));

    var queryParams = {};
    var headerParams =  {};
    var formParams =  {};

    
    
    

	path += createQueryString(queryParams);

	//if (console) {
		//console.log('path: ' + path);
		//console.log('queryParams: ' + queryParams);
	//}

    

    
    
    apiClient.invokeAPI(path, "PUT", queryParams, postBody, postBinaryBody, headerParams, formParams, accept, contentType, authNames, null);
    
    


  }
  
  /**
   * Add a new pet to the store
   * 
   * @param {Pet}  body Pet object that needs to be added to the store
   * @param {function} callback the callback function
   * @return void
   */
  self.addPet = function(body, callback) {
    
    var postBody = JSON.stringify(body);
    var postBinaryBody = null;
    
    // create path and map variables
    var basePath = 'http://petstore.swagger.io/v2';
    // if basePath ends with a /, remove it as path starts with a leading /
    if (basePath.substring(basePath.length-1, basePath.length)=='/') {
    	basePath = basePath.substring(0, basePath.length-1);
    }
    
	var path = basePath + replaceAll(replaceAll("/pet", "\\{format\\}","json"));

    var queryParams = {};
    var headerParams =  {};
    var formParams =  {};

    
    
    

	path += createQueryString(queryParams);

	//if (console) {
		//console.log('path: ' + path);
		//console.log('queryParams: ' + queryParams);
	//}

    

    
    
    apiClient.invokeAPI(path, "POST", queryParams, postBody, postBinaryBody, headerParams, formParams, accept, contentType, authNames, null);
    
    


  }
  
  /**
   * Finds Pets by status
   * Multiple status values can be provided with comma seperated strings
   * @param {Array}  status Status values that need to be considered for filter
   * @param {function} callback the callback function
   * @return Array
   */
  self.findPetsByStatus = function(status, callback) {
    
    var postBody = null;
    var postBinaryBody = null;
    
    // create path and map variables
    var basePath = 'http://petstore.swagger.io/v2';
    // if basePath ends with a /, remove it as path starts with a leading /
    if (basePath.substring(basePath.length-1, basePath.length)=='/') {
    	basePath = basePath.substring(0, basePath.length-1);
    }
    
	var path = basePath + replaceAll(replaceAll("/pet/findByStatus", "\\{format\\}","json"));

    var queryParams = {};
    var headerParams =  {};
    var formParams =  {};

    
    queryParams.status = status;
    
    
    

	path += createQueryString(queryParams);

	//if (console) {
		//console.log('path: ' + path);
		//console.log('queryParams: ' + queryParams);
	//}

    

    
    
    //TypeRef returnType = new TypeRef<Array>() {};
    //return apiClient.invokeAPI(path, "GET", queryParams, postBody, postBinaryBody, headerParams, formParams, accept, contentType, authNames, returnType);
    
	var options = {type: "GET", async: true, contentType: "application/json", dataType: "json", data: postBody};
    var request = $.ajax(path, options);
    //request.fail(function(jqXHR, textStatus, errorThrown){
    //    errorHandler(jqXHR, textStatus, errorThrown);
    //});
    
		
	request.done(function(response, textStatus, jqXHR){
		/**
		  * @returns Array
		  */
		
		 var myResponse = new  Array();
		myResponse.constructFromObject(response);
		
    	callback(myResponse, textStatus, jqXHR);
	});
    
    
    


  }
  
  /**
   * Finds Pets by tags
   * Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
   * @param {Array}  tags Tags to filter by
   * @param {function} callback the callback function
   * @return Array
   */
  self.findPetsByTags = function(tags, callback) {
    
    var postBody = null;
    var postBinaryBody = null;
    
    // create path and map variables
    var basePath = 'http://petstore.swagger.io/v2';
    // if basePath ends with a /, remove it as path starts with a leading /
    if (basePath.substring(basePath.length-1, basePath.length)=='/') {
    	basePath = basePath.substring(0, basePath.length-1);
    }
    
	var path = basePath + replaceAll(replaceAll("/pet/findByTags", "\\{format\\}","json"));

    var queryParams = {};
    var headerParams =  {};
    var formParams =  {};

    
    queryParams.tags = tags;
    
    
    

	path += createQueryString(queryParams);

	//if (console) {
		//console.log('path: ' + path);
		//console.log('queryParams: ' + queryParams);
	//}

    

    
    
    //TypeRef returnType = new TypeRef<Array>() {};
    //return apiClient.invokeAPI(path, "GET", queryParams, postBody, postBinaryBody, headerParams, formParams, accept, contentType, authNames, returnType);
    
	var options = {type: "GET", async: true, contentType: "application/json", dataType: "json", data: postBody};
    var request = $.ajax(path, options);
    //request.fail(function(jqXHR, textStatus, errorThrown){
    //    errorHandler(jqXHR, textStatus, errorThrown);
    //});
    
		
	request.done(function(response, textStatus, jqXHR){
		/**
		  * @returns Array
		  */
		
		 var myResponse = new  Array();
		myResponse.constructFromObject(response);
		
    	callback(myResponse, textStatus, jqXHR);
	});
    
    
    


  }
  
  /**
   * Find pet by ID
   * Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
   * @param {Long}  petId ID of pet that needs to be fetched
   * @param {function} callback the callback function
   * @return Pet
   */
  self.getPetById = function(petId, callback) {
    
    var postBody = null;
    var postBinaryBody = null;
    
     // verify the required parameter 'petId' is set
     if (petId == null) {
        //throw new ApiException(400, "Missing the required parameter 'petId' when calling getPetById");
        var errorRequiredMsg = "Missing the required parameter 'petId' when calling getPetById";
        throw errorRequiredMsg;
     }
     
    // create path and map variables
    var basePath = 'http://petstore.swagger.io/v2';
    // if basePath ends with a /, remove it as path starts with a leading /
    if (basePath.substring(basePath.length-1, basePath.length)=='/') {
    	basePath = basePath.substring(0, basePath.length-1);
    }
    
	var path = basePath + replaceAll(replaceAll("/pet/{petId}", "\\{format\\}","json")
, "\\{" + "petId" + "\\}", apiClient.escapeString(petId.toString()));

    var queryParams = {};
    var headerParams =  {};
    var formParams =  {};

    
    
    

	path += createQueryString(queryParams);

	//if (console) {
		//console.log('path: ' + path);
		//console.log('queryParams: ' + queryParams);
	//}

    

    
    
    //TypeRef returnType = new TypeRef<Pet>() {};
    //return apiClient.invokeAPI(path, "GET", queryParams, postBody, postBinaryBody, headerParams, formParams, accept, contentType, authNames, returnType);
    
	var options = {type: "GET", async: true, contentType: "application/json", dataType: "json", data: postBody};
    var request = $.ajax(path, options);
    //request.fail(function(jqXHR, textStatus, errorThrown){
    //    errorHandler(jqXHR, textStatus, errorThrown);
    //});
    
		
	request.done(function(response, textStatus, jqXHR){
		/**
		  * @returns Pet
		  */
		
		 var myResponse = new  Pet();
		myResponse.constructFromObject(response);
		
    	callback(myResponse, textStatus, jqXHR);
	});
    
    
    


  }
  
  /**
   * Updates a pet in the store with form data
   * 
   * @param {String}  petId ID of pet that needs to be updated
   * @param {String}  name Updated name of the pet
   * @param {String}  status Updated status of the pet
   * @param {function} callback the callback function
   * @return void
   */
  self.updatePetWithForm = function(petId, name, status, callback) {
    
    var postBody = null;
    var postBinaryBody = null;
    
     // verify the required parameter 'petId' is set
     if (petId == null) {
        //throw new ApiException(400, "Missing the required parameter 'petId' when calling updatePetWithForm");
        var errorRequiredMsg = "Missing the required parameter 'petId' when calling updatePetWithForm";
        throw errorRequiredMsg;
     }
     
    // create path and map variables
    var basePath = 'http://petstore.swagger.io/v2';
    // if basePath ends with a /, remove it as path starts with a leading /
    if (basePath.substring(basePath.length-1, basePath.length)=='/') {
    	basePath = basePath.substring(0, basePath.length-1);
    }
    
	var path = basePath + replaceAll(replaceAll("/pet/{petId}", "\\{format\\}","json")
, "\\{" + "petId" + "\\}", apiClient.escapeString(petId.toString()));

    var queryParams = {};
    var headerParams =  {};
    var formParams =  {};

    
    
    if (name != null)
      formParams.put("name", name);
    if (status != null)
      formParams.put("status", status);
    

	path += createQueryString(queryParams);

	//if (console) {
		//console.log('path: ' + path);
		//console.log('queryParams: ' + queryParams);
	//}

    

    
    
    apiClient.invokeAPI(path, "POST", queryParams, postBody, postBinaryBody, headerParams, formParams, accept, contentType, authNames, null);
    
    


  }
  
  /**
   * Deletes a pet
   * 
   * @param {Long}  petId Pet id to delete
   * @param {String}  apiKey 
   * @param {function} callback the callback function
   * @return void
   */
  self.deletePet = function(petId, apiKey, callback) {
    
    var postBody = null;
    var postBinaryBody = null;
    
     // verify the required parameter 'petId' is set
     if (petId == null) {
        //throw new ApiException(400, "Missing the required parameter 'petId' when calling deletePet");
        var errorRequiredMsg = "Missing the required parameter 'petId' when calling deletePet";
        throw errorRequiredMsg;
     }
     
    // create path and map variables
    var basePath = 'http://petstore.swagger.io/v2';
    // if basePath ends with a /, remove it as path starts with a leading /
    if (basePath.substring(basePath.length-1, basePath.length)=='/') {
    	basePath = basePath.substring(0, basePath.length-1);
    }
    
	var path = basePath + replaceAll(replaceAll("/pet/{petId}", "\\{format\\}","json")
, "\\{" + "petId" + "\\}", apiClient.escapeString(petId.toString()));

    var queryParams = {};
    var headerParams =  {};
    var formParams =  {};

    
    if (apiKey != null)
    headerParams.put("api_key", apiClient.parameterToString(apiKey));
    
    

	path += createQueryString(queryParams);

	//if (console) {
		//console.log('path: ' + path);
		//console.log('queryParams: ' + queryParams);
	//}

    

    
    
    apiClient.invokeAPI(path, "DELETE", queryParams, postBody, postBinaryBody, headerParams, formParams, accept, contentType, authNames, null);
    
    


  }
  
  /**
   * uploads an image
   * 
   * @param {Long}  petId ID of pet to update
   * @param {String}  additionalMetadata Additional data to pass to server
   * @param {File}  file file to upload
   * @param {function} callback the callback function
   * @return void
   */
  self.uploadFile = function(petId, additionalMetadata, file, callback) {
    
    var postBody = null;
    var postBinaryBody = null;
    
     // verify the required parameter 'petId' is set
     if (petId == null) {
        //throw new ApiException(400, "Missing the required parameter 'petId' when calling uploadFile");
        var errorRequiredMsg = "Missing the required parameter 'petId' when calling uploadFile";
        throw errorRequiredMsg;
     }
     
    // create path and map variables
    var basePath = 'http://petstore.swagger.io/v2';
    // if basePath ends with a /, remove it as path starts with a leading /
    if (basePath.substring(basePath.length-1, basePath.length)=='/') {
    	basePath = basePath.substring(0, basePath.length-1);
    }
    
	var path = basePath + replaceAll(replaceAll("/pet/{petId}/uploadImage", "\\{format\\}","json")
, "\\{" + "petId" + "\\}", apiClient.escapeString(petId.toString()));

    var queryParams = {};
    var headerParams =  {};
    var formParams =  {};

    
    
    if (additionalMetadata != null)
      formParams.put("additionalMetadata", additionalMetadata);
    if (file != null)
      formParams.put("file", file);
    

	path += createQueryString(queryParams);

	//if (console) {
		//console.log('path: ' + path);
		//console.log('queryParams: ' + queryParams);
	//}

    

    
    
    apiClient.invokeAPI(path, "POST", queryParams, postBody, postBinaryBody, headerParams, formParams, accept, contentType, authNames, null);
    
    


  }
  


 	function replaceAll (haystack, needle, replace) {
		var result= haystack;
		if (needle !=null && replace!=null) {
			result= haystack.replace(new RegExp(needle, 'g'), replace);
		}
		return result;
	}

 	function createQueryString (queryParams) {
		var queryString ='';
		var i = 0;
		for (var queryParamName in queryParams) {
			if (i==0) {
				queryString += '?' ;
			} else {
				queryString += '&' ;
			}
			
			queryString +=  queryParamName + '=' + encodeURIComponent(queryParams[queryParamName]);
			i++;
		}
		
		return queryString;
	}
}
