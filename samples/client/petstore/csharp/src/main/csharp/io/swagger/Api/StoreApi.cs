using System;
using System.Collections.Generic;
using RestSharp;
using io.swagger.client;
using io.swagger.Model;

namespace io.swagger.Api {
  
  public class StoreApi {
    string basePath;
    private readonly ApiInvoker apiInvoker = ApiInvoker.GetInstance();
    protected RestClient _client;

    public StoreApi(String basePath = "http://petstore.swagger.io/v2")
    {
      this.basePath = basePath;
      _client = new RestClient(basePath);
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
    public Dictionary<String, int?>  GetInventory () {
      // create path and map variables
      var path = "/store/inventory".Replace("{format}","json");

      var _request = new RestRequest("/store/inventory", Method.GET);

      

      // path (url segment) parameters
      _request.AddUrlSegment("format", "json"); // set format to json by default
      
      // query parameters, if any
      
      // header parameters, if any
      
      // form parameters, if any
      

      try {
        IRestResponse response = _client.Execute(_request);
        return (Dictionary<String, int?>) ApiInvoker.deserialize(response.Content, typeof(Dictionary<String, int?>));
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

      

      // path (url segment) parameters
      _request.AddUrlSegment("format", "json"); // set format to json by default
      
      // query parameters, if any
      
      // header parameters, if any
      
      // form parameters, if any
      

      try {
        IRestResponse response = _client.Execute(_request);
        return (Order) ApiInvoker.deserialize(response.Content, typeof(Order));
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

      

      // path (url segment) parameters
      _request.AddUrlSegment("format", "json"); // set format to json by default
      _request.AddUrlSegment("orderId", apiInvoker.ParameterToString(OrderId));
      // query parameters, if any
      
      // header parameters, if any
      
      // form parameters, if any
      

      try {
        IRestResponse response = _client.Execute(_request);
        return (Order) ApiInvoker.deserialize(response.Content, typeof(Order));
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

      

      // path (url segment) parameters
      _request.AddUrlSegment("format", "json"); // set format to json by default
      _request.AddUrlSegment("orderId", apiInvoker.ParameterToString(OrderId));
      // query parameters, if any
      
      // header parameters, if any
      
      // form parameters, if any
      

      try {
        
        _client.Execute(_request);
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
