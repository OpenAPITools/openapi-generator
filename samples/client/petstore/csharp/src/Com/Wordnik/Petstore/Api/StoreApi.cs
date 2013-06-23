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
      
      public void setBasePath(string basePath) {
        this.basePath = basePath;
      }
      
      public String getBasePath() {
        return basePath;
      }

      public Order getOrderById (string Orderid) {
        // create path and map variables
        var path = "/store.{format}/order/{orderId}".Replace("{format}","json").Replace("{" + "Orderid" + "}", apiInvoker.escapeString(Orderid.ToString()));

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();

        // verify required params are set
        if (Orderid == null ) {
           throw new ApiException(400, "missing required params");
        }
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
      public void deleteOrder (string Orderid) {
        // create path and map variables
        var path = "/store.{format}/order/{orderId}".Replace("{format}","json").Replace("{" + "Orderid" + "}", apiInvoker.escapeString(Orderid.ToString()));

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();

        // verify required params are set
        if (Orderid == null ) {
           throw new ApiException(400, "missing required params");
        }
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
      public void placeOrder (Order body) {
        // create path and map variables
        var path = "/store.{format}/order".Replace("{format}","json");

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();

        // verify required params are set
        if (body == null ) {
           throw new ApiException(400, "missing required params");
        }
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
