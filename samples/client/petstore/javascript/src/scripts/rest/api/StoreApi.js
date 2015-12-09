/*
 * @javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavascriptClientCodegen", date = "2015-12-09T16:07:21.000+07:00")
 */

//export module
if ( typeof define === "function" && define.amd ) {     
	define(['jquery'], function($) {
        return StoreApi;
	 });
}

var StoreApi = function StoreApi() {
	var self = this;
  
  /**
   * Returns pet inventories by status
   * Returns a map of status codes to quantities
   * @param {function} callback the callback function
   * @return Map<String, Integer>
   */
  self.getInventory = function(callback) {
    
    var postBody = null;
    var postBinaryBody = null;
    
    // create path and map variables
    var basePath = 'http://petstore.swagger.io/v2';
    // if basePath ends with a /, remove it as path starts with a leading /
    if (basePath.substring(basePath.length-1, basePath.length)=='/') {
    	basePath = basePath.substring(0, basePath.length-1);
    }
    
	var path = basePath + replaceAll(replaceAll("/store/inventory", "\\{format\\}","json"));

    var queryParams = {};
    var headerParams =  {};
    var formParams =  {};

    
    
    

	path += createQueryString(queryParams);

	//if (console) {
		//console.log('path: ' + path);
		//console.log('queryParams: ' + queryParams);
	//}

    

    
    
    //TypeRef returnType = new TypeRef<Map<String, Integer>>() {};
    //return apiClient.invokeAPI(path, "GET", queryParams, postBody, postBinaryBody, headerParams, formParams, accept, contentType, authNames, returnType);
    
	var options = {type: "GET", async: true, contentType: "application/json", dataType: "json", data: postBody};
    var request = $.ajax(path, options);
    //request.fail(function(jqXHR, textStatus, errorThrown){
    //    errorHandler(jqXHR, textStatus, errorThrown);
    //});
    
		
	request.done(function(response, textStatus, jqXHR){
		/**
		  * @returns Map<String, Integer>
		  */
		
		 var myResponse = new  Map<String, Integer>();
		myResponse.constructFromObject(response);
		
    	callback(myResponse, textStatus, jqXHR);
	});
    
    
    


  }
  
  /**
   * Place an order for a pet
   * 
   * @param {Order}  body order placed for purchasing the pet
   * @param {function} callback the callback function
   * @return Order
   */
  self.placeOrder = function(body, callback) {
    
    var postBody = JSON.stringify(body);
    var postBinaryBody = null;
    
    // create path and map variables
    var basePath = 'http://petstore.swagger.io/v2';
    // if basePath ends with a /, remove it as path starts with a leading /
    if (basePath.substring(basePath.length-1, basePath.length)=='/') {
    	basePath = basePath.substring(0, basePath.length-1);
    }
    
	var path = basePath + replaceAll(replaceAll("/store/order", "\\{format\\}","json"));

    var queryParams = {};
    var headerParams =  {};
    var formParams =  {};

    
    
    

	path += createQueryString(queryParams);

	//if (console) {
		//console.log('path: ' + path);
		//console.log('queryParams: ' + queryParams);
	//}

    

    
    
    //TypeRef returnType = new TypeRef<Order>() {};
    //return apiClient.invokeAPI(path, "POST", queryParams, postBody, postBinaryBody, headerParams, formParams, accept, contentType, authNames, returnType);
    
	var options = {type: "POST", async: true, contentType: "application/json", dataType: "json", data: postBody};
    var request = $.ajax(path, options);
    //request.fail(function(jqXHR, textStatus, errorThrown){
    //    errorHandler(jqXHR, textStatus, errorThrown);
    //});
    
		
	request.done(function(response, textStatus, jqXHR){
		/**
		  * @returns Order
		  */
		
		 var myResponse = new  Order();
		myResponse.constructFromObject(response);
		
    	callback(myResponse, textStatus, jqXHR);
	});
    
    
    


  }
  
  /**
   * Find purchase order by ID
   * For valid response try integer IDs with value &lt;= 5 or &gt; 10. Other values will generated exceptions
   * @param {String}  orderId ID of pet that needs to be fetched
   * @param {function} callback the callback function
   * @return Order
   */
  self.getOrderById = function(orderId, callback) {
    
    var postBody = null;
    var postBinaryBody = null;
    
     // verify the required parameter 'orderId' is set
     if (orderId == null) {
        //throw new ApiException(400, "Missing the required parameter 'orderId' when calling getOrderById");
        var errorRequiredMsg = "Missing the required parameter 'orderId' when calling getOrderById";
        throw errorRequiredMsg;
     }
     
    // create path and map variables
    var basePath = 'http://petstore.swagger.io/v2';
    // if basePath ends with a /, remove it as path starts with a leading /
    if (basePath.substring(basePath.length-1, basePath.length)=='/') {
    	basePath = basePath.substring(0, basePath.length-1);
    }
    
	var path = basePath + replaceAll(replaceAll("/store/order/{orderId}", "\\{format\\}","json")
, "\\{" + "orderId" + "\\}", apiClient.escapeString(orderId.toString()));

    var queryParams = {};
    var headerParams =  {};
    var formParams =  {};

    
    
    

	path += createQueryString(queryParams);

	//if (console) {
		//console.log('path: ' + path);
		//console.log('queryParams: ' + queryParams);
	//}

    

    
    
    //TypeRef returnType = new TypeRef<Order>() {};
    //return apiClient.invokeAPI(path, "GET", queryParams, postBody, postBinaryBody, headerParams, formParams, accept, contentType, authNames, returnType);
    
	var options = {type: "GET", async: true, contentType: "application/json", dataType: "json", data: postBody};
    var request = $.ajax(path, options);
    //request.fail(function(jqXHR, textStatus, errorThrown){
    //    errorHandler(jqXHR, textStatus, errorThrown);
    //});
    
		
	request.done(function(response, textStatus, jqXHR){
		/**
		  * @returns Order
		  */
		
		 var myResponse = new  Order();
		myResponse.constructFromObject(response);
		
    	callback(myResponse, textStatus, jqXHR);
	});
    
    
    


  }
  
  /**
   * Delete purchase order by ID
   * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
   * @param {String}  orderId ID of the order that needs to be deleted
   * @param {function} callback the callback function
   * @return void
   */
  self.deleteOrder = function(orderId, callback) {
    
    var postBody = null;
    var postBinaryBody = null;
    
     // verify the required parameter 'orderId' is set
     if (orderId == null) {
        //throw new ApiException(400, "Missing the required parameter 'orderId' when calling deleteOrder");
        var errorRequiredMsg = "Missing the required parameter 'orderId' when calling deleteOrder";
        throw errorRequiredMsg;
     }
     
    // create path and map variables
    var basePath = 'http://petstore.swagger.io/v2';
    // if basePath ends with a /, remove it as path starts with a leading /
    if (basePath.substring(basePath.length-1, basePath.length)=='/') {
    	basePath = basePath.substring(0, basePath.length-1);
    }
    
	var path = basePath + replaceAll(replaceAll("/store/order/{orderId}", "\\{format\\}","json")
, "\\{" + "orderId" + "\\}", apiClient.escapeString(orderId.toString()));

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
