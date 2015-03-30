  using System;
  using System.Collections.Generic;
  using io.swagger.client;
  using io.swagger.Model;
  
  
  
  

  namespace io.swagger.Api {
    
    public class StoreApi {
      string basePath;
      private readonly ApiInvoker apiInvoker = ApiInvoker.GetInstance();

      public StoreApi(String basePath = "http://petstore.swagger.io/v2")
      {
        this.basePath = basePath;
      }

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
      /// Returns pet inventories by status Returns a map of status codes to quantities
      /// </summary>
      
      /// <returns></returns>
      public Dictionary<String, int?>  getInventory () {
        // create path and map variables
        var path = "/store/inventory".Replace("{format}","json");

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();
        var formParams = new Dictionary<String, object>();

        

        

        

        

        try {
          if (typeof(Dictionary<String, int?>) == typeof(byte[])) {
            var response = apiInvoker.invokeBinaryAPI(basePath, path, "GET", queryParams, null, headerParams, formParams);
            return ((object)response) as Dictionary<String, int?>;
          } else {
            var response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, null, headerParams, formParams);
            if(response != null){
               return (Dictionary<String, int?>) ApiInvoker.deserialize(response, typeof(Dictionary<String, int?>));
            }
            else {
              return null;
            }
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
      /// <param name="Body">order placed for purchasing the pet</param>
      
      /// <returns></returns>
      public Order  placeOrder (Order Body) {
        // create path and map variables
        var path = "/store/order".Replace("{format}","json");

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();
        var formParams = new Dictionary<String, object>();

        

        

        

        

        try {
          if (typeof(Order) == typeof(byte[])) {
            var response = apiInvoker.invokeBinaryAPI(basePath, path, "GET", queryParams, null, headerParams, formParams);
            return ((object)response) as Order;
          } else {
            var response = apiInvoker.invokeAPI(basePath, path, "POST", queryParams, Body, headerParams, formParams);
            if(response != null){
               return (Order) ApiInvoker.deserialize(response, typeof(Order));
            }
            else {
              return null;
            }
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
      /// Find purchase order by ID For valid response try integer IDs with value &lt;= 5 or &gt; 10. Other values will generated exceptions
      /// </summary>
      /// <param name="OrderId">ID of pet that needs to be fetched</param>
      
      /// <returns></returns>
      public Order  getOrderById (string OrderId) {
        // create path and map variables
        var path = "/store/order/{orderId}".Replace("{format}","json").Replace("{" + "orderId" + "}", apiInvoker.escapeString(OrderId.ToString()));

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();
        var formParams = new Dictionary<String, object>();

        

        

        

        

        try {
          if (typeof(Order) == typeof(byte[])) {
            var response = apiInvoker.invokeBinaryAPI(basePath, path, "GET", queryParams, null, headerParams, formParams);
            return ((object)response) as Order;
          } else {
            var response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, null, headerParams, formParams);
            if(response != null){
               return (Order) ApiInvoker.deserialize(response, typeof(Order));
            }
            else {
              return null;
            }
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
      /// Delete purchase order by ID For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
      /// </summary>
      /// <param name="OrderId">ID of the order that needs to be deleted</param>
      
      /// <returns></returns>
      public void  deleteOrder (string OrderId) {
        // create path and map variables
        var path = "/store/order/{orderId}".Replace("{format}","json").Replace("{" + "orderId" + "}", apiInvoker.escapeString(OrderId.ToString()));

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();
        var formParams = new Dictionary<String, object>();

        

        

        

        

        try {
          if (typeof(void) == typeof(byte[])) {
            var response = apiInvoker.invokeBinaryAPI(basePath, path, "GET", queryParams, null, headerParams, formParams);
            return ;
          } else {
            var response = apiInvoker.invokeAPI(basePath, path, "DELETE", queryParams, null, headerParams, formParams);
            if(response != null){
               return ;
            }
            else {
              return ;
            }
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
