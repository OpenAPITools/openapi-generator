  using System;
  using System.Collections.Generic;
  using Com.Wordnik.Petstore;
  using Com.Wordnik.Petstore.Model;
  namespace Com.Wordnik.Petstore.Api {
    public class StoreApi {
      string basePath = "http://petstore.swagger.wordnik.com/api";
      private readonly ApiInvoker apiInvoker = ApiInvoker.GetInstance();

      public ApiInvoker getInvoker() {
        return apiInvoker;
      }
      
      // Sets the endpoint base url for the services being accessed
      public void setBasePath(string basePath) {
        this.basePath = basePath;
      }
      
      // Gets the endpoint base url for the services being accessed
      public String getBasePath() {
        return basePath;
      }

      /// <summary>
      /// Delete purchase order by ID For valid response try integer IDs with value &lt; 1000.  Anything above 1000 or nonintegers will generate API errors
      /// </summary>
      /// <param name="orderId">ID of the order that needs to be deleted</param>
      /// <returns></returns>
      public void deleteOrder (string orderId) {
        // create path and map variables
        var path = "/store/order/{orderId}".Replace("{format}","json").Replace("{" + "orderId" + "}", apiInvoker.escapeString(orderId.ToString()));

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();

        // verify required params are set
        if (orderId == null ) {
           throw new ApiException(400, "missing required params");
        }
        string paramStr = null;
        try {
          var response = apiInvoker.invokeAPI(basePath, path, "DELETE", queryParams, null, headerParams);
          if(response != null){
             return ;
          }
          else {
            return ;
          }
        } catch (ApiException ex) {
          if(ex.ErrorCode == 404) {
          	return ;
          }
          else {
            throw ex;
          }
        }
      }
      /// <summary>
      /// Find purchase order by ID For valid response try integer IDs with value &lt;= 5. Anything above 5 or nonintegers will generate API errors
      /// </summary>
      /// <param name="orderId">ID of pet that needs to be fetched</param>
      /// <returns></returns>
      public Order getOrderById (string orderId) {
        // create path and map variables
        var path = "/store/order/{orderId}".Replace("{format}","json").Replace("{" + "orderId" + "}", apiInvoker.escapeString(orderId.ToString()));

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();

        // verify required params are set
        if (orderId == null ) {
           throw new ApiException(400, "missing required params");
        }
        string paramStr = null;
        try {
          var response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, null, headerParams);
          if(response != null){
             return (Order) ApiInvoker.deserialize(response, typeof(Order));
          }
          else {
            return null;
          }
        } catch (ApiException ex) {
          if(ex.ErrorCode == 404) {
          	return null;
          }
          else {
            throw ex;
          }
        }
      }
      /// <summary>
      /// Place an order for a pet 
      /// </summary>
      /// <param name="body">order placed for purchasing the pet</param>
      /// <returns></returns>
      public void placeOrder (Order body) {
        // create path and map variables
        var path = "/store/order".Replace("{format}","json");

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();

        // verify required params are set
        if (body == null ) {
           throw new ApiException(400, "missing required params");
        }
        string paramStr = null;
        try {
          var response = apiInvoker.invokeAPI(basePath, path, "POST", queryParams, body, headerParams);
          if(response != null){
             return ;
          }
          else {
            return ;
          }
        } catch (ApiException ex) {
          if(ex.ErrorCode == 404) {
          	return ;
          }
          else {
            throw ex;
          }
        }
      }
      }
    }
