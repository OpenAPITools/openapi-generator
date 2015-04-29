using System;
using System.Collections.Generic;
using RestSharp;
using io.swagger.client;
using io.swagger.Model;

namespace io.swagger.Api {
  
  public class StoreApi {
    string basePath;
    protected RestClient restClient;

    public StoreApi(String basePath = "http://petstore.swagger.io/v2")
    {
      this.basePath = basePath;
      this.restClient = new RestClient(basePath);
    }

    /// <summary>
    /// Sets the endpoint base url for the services being accessed
    /// </summary>
    /// <param name="basePath"> Base URL
    /// <returns></returns>
    public void SetBasePath(string basePath) {
      this.basePath = basePath;
    }

    /// <summary>
    /// Gets the endpoint base url for the services being accessed
    /// <returns>Base URL</returns>
    /// </summary>
    public String GetBasePath() {
      return this.basePath;
    }

    
    
    /// <summary>
    /// Returns pet inventories by status Returns a map of status codes to quantities
    /// </summary>
    /// <returns></returns>
    public Dictionary<String, int?>  GetInventory () {
      // create path and map variables
      var path = "/store/inventory".Replace("{format}","json");

      var _request = new RestRequest("/store/inventory", Method.GET);

      

      _request.AddUrlSegment("format", "json"); // set format to json by default
      
      
      
      
      

      try {
        IRestResponse response = restClient.Execute(_request);
        return (Dictionary<String, int?>) ApiInvoker.Deserialize(response.Content, typeof(Dictionary<String, int?>));
        //return ((object)response) as Dictionary<String, int?>;
        
      } catch (Exception ex) {
        if(ex != null) {
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
    public Order  PlaceOrder (Order Body) {
      // create path and map variables
      var path = "/store/order".Replace("{format}","json");

      var _request = new RestRequest("/store/order", Method.POST);

      

      _request.AddUrlSegment("format", "json"); // set format to json by default
      
      
      
      
      
      _request.AddParameter("application/json", ApiInvoker.Serialize(Body), ParameterType.RequestBody); // HTTP request body (model)
      

      try {
        IRestResponse response = restClient.Execute(_request);
        return (Order) ApiInvoker.Deserialize(response.Content, typeof(Order));
        //return ((object)response) as Order;
        
      } catch (Exception ex) {
        if(ex != null) {
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
    public Order  GetOrderById (string OrderId) {
      // create path and map variables
      var path = "/store/order/{orderId}".Replace("{format}","json").Replace("{" + "orderId" + "}", apiInvoker.ParameterToString(OrderId));

      var _request = new RestRequest("/store/order/{orderId}", Method.GET);

      

      _request.AddUrlSegment("format", "json"); // set format to json by default
      _request.AddUrlSegment("orderId", ApiInvoker.ParameterToString(OrderId)); // path (url segment) parameter
      
      
      
      
      

      try {
        IRestResponse response = restClient.Execute(_request);
        return (Order) ApiInvoker.Deserialize(response.Content, typeof(Order));
        //return ((object)response) as Order;
        
      } catch (Exception ex) {
        if(ex != null) {
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
    public void  DeleteOrder (string OrderId) {
      // create path and map variables
      var path = "/store/order/{orderId}".Replace("{format}","json").Replace("{" + "orderId" + "}", apiInvoker.ParameterToString(OrderId));

      var _request = new RestRequest("/store/order/{orderId}", Method.DELETE);

      

      _request.AddUrlSegment("format", "json"); // set format to json by default
      _request.AddUrlSegment("orderId", ApiInvoker.ParameterToString(OrderId)); // path (url segment) parameter
      
      
      
      
      

      try {
        
        restClient.Execute(_request);
        return;
      } catch (Exception ex) {
        if(ex != null) {
          return ;
        }
        else {
          throw ex;
        }
      }
    }
    
  }
  
}
