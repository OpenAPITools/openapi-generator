using System;
using System.Collections.Generic;
using RestSharp;
using IO.Swagger.Client;
using IO.Swagger.Model;

namespace IO.Swagger.Api {
  
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
    /// <returns>Dictionary<String, int?></returns>
    public Dictionary<String, int?> GetInventory () {

      var _request = new RestRequest("/store/inventory", Method.GET);

      

      // add default header, if any
      foreach(KeyValuePair<string, string> defaultHeader in ApiInvoker.GetDefaultHeader())
      {
        _request.AddHeader(defaultHeader.Key, defaultHeader.Value);
      }

      _request.AddUrlSegment("format", "json"); // set format to json by default
      
      
      
      
      

      try {
        // make the HTTP request
        IRestResponse response = restClient.Execute(_request);
        return (Dictionary<String, int?>) ApiInvoker.Deserialize(response.Content, typeof(Dictionary<String, int?>));
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
    /// <returns>Order</returns>
    public Order PlaceOrder (Order Body) {

      var _request = new RestRequest("/store/order", Method.POST);

      

      // add default header, if any
      foreach(KeyValuePair<string, string> defaultHeader in ApiInvoker.GetDefaultHeader())
      {
        _request.AddHeader(defaultHeader.Key, defaultHeader.Value);
      }

      _request.AddUrlSegment("format", "json"); // set format to json by default
      
      
      
      
      _request.AddParameter("application/json", ApiInvoker.Serialize(Body), ParameterType.RequestBody); // http body (model) parameter
      

      try {
        // make the HTTP request
        IRestResponse response = restClient.Execute(_request);
        return (Order) ApiInvoker.Deserialize(response.Content, typeof(Order));
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
    /// <returns>Order</returns>
    public Order GetOrderById (string OrderId) {

      var _request = new RestRequest("/store/order/{orderId}", Method.GET);

      
      // verify the required parameter 'OrderId' is set
      if (OrderId == null) throw new ApiException(400, "Missing required parameter 'OrderId' when calling GetOrderById");
      

      // add default header, if any
      foreach(KeyValuePair<string, string> defaultHeader in ApiInvoker.GetDefaultHeader())
      {
        _request.AddHeader(defaultHeader.Key, defaultHeader.Value);
      }

      _request.AddUrlSegment("format", "json"); // set format to json by default
      _request.AddUrlSegment("orderId", ApiInvoker.ParameterToString(OrderId)); // path (url segment) parameter
      
      
      
      
      

      try {
        // make the HTTP request
        IRestResponse response = restClient.Execute(_request);
        return (Order) ApiInvoker.Deserialize(response.Content, typeof(Order));
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
    public void DeleteOrder (string OrderId) {

      var _request = new RestRequest("/store/order/{orderId}", Method.DELETE);

      
      // verify the required parameter 'OrderId' is set
      if (OrderId == null) throw new ApiException(400, "Missing required parameter 'OrderId' when calling DeleteOrder");
      

      // add default header, if any
      foreach(KeyValuePair<string, string> defaultHeader in ApiInvoker.GetDefaultHeader())
      {
        _request.AddHeader(defaultHeader.Key, defaultHeader.Value);
      }

      _request.AddUrlSegment("format", "json"); // set format to json by default
      _request.AddUrlSegment("orderId", ApiInvoker.ParameterToString(OrderId)); // path (url segment) parameter
      
      
      
      
      

      try {
        // make the HTTP request
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
