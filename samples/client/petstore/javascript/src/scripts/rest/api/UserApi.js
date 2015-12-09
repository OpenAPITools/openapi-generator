/*
 * @javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavascriptClientCodegen", date = "2015-12-09T16:07:21.000+07:00")
 */

//export module
if ( typeof define === "function" && define.amd ) {     
	define(['jquery'], function($) {
        return UserApi;
	 });
}

var UserApi = function UserApi() {
	var self = this;
  
  /**
   * Create user
   * This can only be done by the logged in user.
   * @param {User}  body Created user object
   * @param {function} callback the callback function
   * @return void
   */
  self.createUser = function(body, callback) {
    
    var postBody = JSON.stringify(body);
    var postBinaryBody = null;
    
    // create path and map variables
    var basePath = 'http://petstore.swagger.io/v2';
    // if basePath ends with a /, remove it as path starts with a leading /
    if (basePath.substring(basePath.length-1, basePath.length)=='/') {
    	basePath = basePath.substring(0, basePath.length-1);
    }
    
	var path = basePath + replaceAll(replaceAll("/user", "\\{format\\}","json"));

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
   * Creates list of users with given input array
   * 
   * @param {Array}  body List of user object
   * @param {function} callback the callback function
   * @return void
   */
  self.createUsersWithArrayInput = function(body, callback) {
    
    var postBody = JSON.stringify(body);
    var postBinaryBody = null;
    
    // create path and map variables
    var basePath = 'http://petstore.swagger.io/v2';
    // if basePath ends with a /, remove it as path starts with a leading /
    if (basePath.substring(basePath.length-1, basePath.length)=='/') {
    	basePath = basePath.substring(0, basePath.length-1);
    }
    
	var path = basePath + replaceAll(replaceAll("/user/createWithArray", "\\{format\\}","json"));

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
   * Creates list of users with given input array
   * 
   * @param {Array}  body List of user object
   * @param {function} callback the callback function
   * @return void
   */
  self.createUsersWithListInput = function(body, callback) {
    
    var postBody = JSON.stringify(body);
    var postBinaryBody = null;
    
    // create path and map variables
    var basePath = 'http://petstore.swagger.io/v2';
    // if basePath ends with a /, remove it as path starts with a leading /
    if (basePath.substring(basePath.length-1, basePath.length)=='/') {
    	basePath = basePath.substring(0, basePath.length-1);
    }
    
	var path = basePath + replaceAll(replaceAll("/user/createWithList", "\\{format\\}","json"));

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
   * Logs user into the system
   * 
   * @param {String}  username The user name for login
   * @param {String}  password The password for login in clear text
   * @param {function} callback the callback function
   * @return String
   */
  self.loginUser = function(username, password, callback) {
    
    var postBody = null;
    var postBinaryBody = null;
    
    // create path and map variables
    var basePath = 'http://petstore.swagger.io/v2';
    // if basePath ends with a /, remove it as path starts with a leading /
    if (basePath.substring(basePath.length-1, basePath.length)=='/') {
    	basePath = basePath.substring(0, basePath.length-1);
    }
    
	var path = basePath + replaceAll(replaceAll("/user/login", "\\{format\\}","json"));

    var queryParams = {};
    var headerParams =  {};
    var formParams =  {};

    
    queryParams.username = username;
    
    queryParams.password = password;
    
    
    

	path += createQueryString(queryParams);

	//if (console) {
		//console.log('path: ' + path);
		//console.log('queryParams: ' + queryParams);
	//}

    

    
    
    //TypeRef returnType = new TypeRef<String>() {};
    //return apiClient.invokeAPI(path, "GET", queryParams, postBody, postBinaryBody, headerParams, formParams, accept, contentType, authNames, returnType);
    
	var options = {type: "GET", async: true, contentType: "application/json", dataType: "json", data: postBody};
    var request = $.ajax(path, options);
    //request.fail(function(jqXHR, textStatus, errorThrown){
    //    errorHandler(jqXHR, textStatus, errorThrown);
    //});
    
		
	request.done(function(response, textStatus, jqXHR){
		/**
		  * @returns String
		  */
		 var myResponse = response;
		
		
    	callback(myResponse, textStatus, jqXHR);
	});
    
    
    


  }
  
  /**
   * Logs out current logged in user session
   * 
   * @param {function} callback the callback function
   * @return void
   */
  self.logoutUser = function(callback) {
    
    var postBody = null;
    var postBinaryBody = null;
    
    // create path and map variables
    var basePath = 'http://petstore.swagger.io/v2';
    // if basePath ends with a /, remove it as path starts with a leading /
    if (basePath.substring(basePath.length-1, basePath.length)=='/') {
    	basePath = basePath.substring(0, basePath.length-1);
    }
    
	var path = basePath + replaceAll(replaceAll("/user/logout", "\\{format\\}","json"));

    var queryParams = {};
    var headerParams =  {};
    var formParams =  {};

    
    
    

	path += createQueryString(queryParams);

	//if (console) {
		//console.log('path: ' + path);
		//console.log('queryParams: ' + queryParams);
	//}

    

    
    
    apiClient.invokeAPI(path, "GET", queryParams, postBody, postBinaryBody, headerParams, formParams, accept, contentType, authNames, null);
    
    


  }
  
  /**
   * Get user by user name
   * 
   * @param {String}  username The name that needs to be fetched. Use user1 for testing.
   * @param {function} callback the callback function
   * @return User
   */
  self.getUserByName = function(username, callback) {
    
    var postBody = null;
    var postBinaryBody = null;
    
     // verify the required parameter 'username' is set
     if (username == null) {
        //throw new ApiException(400, "Missing the required parameter 'username' when calling getUserByName");
        var errorRequiredMsg = "Missing the required parameter 'username' when calling getUserByName";
        throw errorRequiredMsg;
     }
     
    // create path and map variables
    var basePath = 'http://petstore.swagger.io/v2';
    // if basePath ends with a /, remove it as path starts with a leading /
    if (basePath.substring(basePath.length-1, basePath.length)=='/') {
    	basePath = basePath.substring(0, basePath.length-1);
    }
    
	var path = basePath + replaceAll(replaceAll("/user/{username}", "\\{format\\}","json")
, "\\{" + "username" + "\\}", apiClient.escapeString(username.toString()));

    var queryParams = {};
    var headerParams =  {};
    var formParams =  {};

    
    
    

	path += createQueryString(queryParams);

	//if (console) {
		//console.log('path: ' + path);
		//console.log('queryParams: ' + queryParams);
	//}

    

    
    
    //TypeRef returnType = new TypeRef<User>() {};
    //return apiClient.invokeAPI(path, "GET", queryParams, postBody, postBinaryBody, headerParams, formParams, accept, contentType, authNames, returnType);
    
	var options = {type: "GET", async: true, contentType: "application/json", dataType: "json", data: postBody};
    var request = $.ajax(path, options);
    //request.fail(function(jqXHR, textStatus, errorThrown){
    //    errorHandler(jqXHR, textStatus, errorThrown);
    //});
    
		
	request.done(function(response, textStatus, jqXHR){
		/**
		  * @returns User
		  */
		
		 var myResponse = new  User();
		myResponse.constructFromObject(response);
		
    	callback(myResponse, textStatus, jqXHR);
	});
    
    
    


  }
  
  /**
   * Updated user
   * This can only be done by the logged in user.
   * @param {String}  username name that need to be deleted
   * @param {User}  body Updated user object
   * @param {function} callback the callback function
   * @return void
   */
  self.updateUser = function(username, body, callback) {
    
    var postBody = JSON.stringify(body);
    var postBinaryBody = null;
    
     // verify the required parameter 'username' is set
     if (username == null) {
        //throw new ApiException(400, "Missing the required parameter 'username' when calling updateUser");
        var errorRequiredMsg = "Missing the required parameter 'username' when calling updateUser";
        throw errorRequiredMsg;
     }
     
    // create path and map variables
    var basePath = 'http://petstore.swagger.io/v2';
    // if basePath ends with a /, remove it as path starts with a leading /
    if (basePath.substring(basePath.length-1, basePath.length)=='/') {
    	basePath = basePath.substring(0, basePath.length-1);
    }
    
	var path = basePath + replaceAll(replaceAll("/user/{username}", "\\{format\\}","json")
, "\\{" + "username" + "\\}", apiClient.escapeString(username.toString()));

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
   * Delete user
   * This can only be done by the logged in user.
   * @param {String}  username The name that needs to be deleted
   * @param {function} callback the callback function
   * @return void
   */
  self.deleteUser = function(username, callback) {
    
    var postBody = null;
    var postBinaryBody = null;
    
     // verify the required parameter 'username' is set
     if (username == null) {
        //throw new ApiException(400, "Missing the required parameter 'username' when calling deleteUser");
        var errorRequiredMsg = "Missing the required parameter 'username' when calling deleteUser";
        throw errorRequiredMsg;
     }
     
    // create path and map variables
    var basePath = 'http://petstore.swagger.io/v2';
    // if basePath ends with a /, remove it as path starts with a leading /
    if (basePath.substring(basePath.length-1, basePath.length)=='/') {
    	basePath = basePath.substring(0, basePath.length-1);
    }
    
	var path = basePath + replaceAll(replaceAll("/user/{username}", "\\{format\\}","json")
, "\\{" + "username" + "\\}", apiClient.escapeString(username.toString()));

    var queryParams = {};
    var headerParams =  {};
    var formParams =  {};

    
    
    

	path += createQueryString(queryParams);

	//if (console) {
		//console.log('path: ' + path);
		//console.log('queryParams: ' + queryParams);
	//}

    

    
    
    apiClient.invokeAPI(path, "DELETE", queryParams, postBody, postBinaryBody, headerParams, formParams, accept, contentType, authNames, null);
    
    


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
