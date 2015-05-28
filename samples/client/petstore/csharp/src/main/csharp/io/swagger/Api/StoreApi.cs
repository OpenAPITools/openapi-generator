using System;
using System.Collections.Generic;
using RestSharp;
using IO.Swagger.Client;
using IO.Swagger.Model;

namespace IO.Swagger.Api {
  
  public class StoreApi {
    string basePath;
    public ApiClient apiClient {get; set;}

    public StoreApi(String basePath = "http://petstore.swagger.io/v2")
    {
      this.basePath = basePath;
      this.apiClient = new ApiClient(basePath);
    }

    /// <summary>
    /// Create a new object 
    /// </summary>
    /// <param name="apiClient"> an instance of ApiClient
    /// <returns></returns>
    public StoreApi(ApiClient apiClient = null) {
      if (apiClient == null) { // use the default one in Configuration
        this.apiClient = Configuration.apiClient; 
      } else {
        this.apiClient = apiClient;
      }
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

      

      var path = "/store/inventory";
      path = path.Replace("{format}", "json");
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

      
      
      
      

      // make the HTTP request
      IRestResponse response = (IRestResponse) apiClient.CallApi(path, Method.GET, queryParams, postBody, headerParams, formParams, fileParams);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling GetInventory: " + response.Content);
      }
      return (Dictionary<String, int?>) apiClient.Deserialize(response.Content, typeof(Dictionary<String, int?>));
    }
    
    /// <summary>
    /// Place an order for a pet 
    /// </summary>
    /// <param name="Body">order placed for purchasing the pet</param>
    /// <returns>Order</returns>
    public Order PlaceOrder (Order Body) {

      

      var path = "/store/order";
      path = path.Replace("{format}", "json");
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

      
      
      
      postBody = apiClient.Serialize(Body); // http body (model) parameter
      

      // make the HTTP request
      IRestResponse response = (IRestResponse) apiClient.CallApi(path, Method.POST, queryParams, postBody, headerParams, formParams, fileParams);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling PlaceOrder: " + response.Content);
      }
      return (Order) apiClient.Deserialize(response.Content, typeof(Order));
    }
    
    /// <summary>
    /// Find purchase order by ID For valid response try integer IDs with value &lt;= 5 or &gt; 10. Other values will generated exceptions
    /// </summary>
    /// <param name="OrderId">ID of pet that needs to be fetched</param>
    /// <returns>Order</returns>
    public Order GetOrderById (string OrderId) {

      
      // verify the required parameter 'OrderId' is set
      if (OrderId == null) throw new ApiException(400, "Missing required parameter 'OrderId' when calling GetOrderById");
      

      var path = "/store/order/{orderId}";
      path = path.Replace("{format}", "json");
      path = path.Replace("{" + "orderId" + "}", apiClient.ParameterToString(OrderId));
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

      
      
      
      

      // make the HTTP request
      IRestResponse response = (IRestResponse) apiClient.CallApi(path, Method.GET, queryParams, postBody, headerParams, formParams, fileParams);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling GetOrderById: " + response.Content);
      }
      return (Order) apiClient.Deserialize(response.Content, typeof(Order));
    }
    
    /// <summary>
    /// Delete purchase order by ID For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
    /// </summary>
    /// <param name="OrderId">ID of the order that needs to be deleted</param>
    /// <returns></returns>
    public void DeleteOrder (string OrderId) {

      
      // verify the required parameter 'OrderId' is set
      if (OrderId == null) throw new ApiException(400, "Missing required parameter 'OrderId' when calling DeleteOrder");
      

      var path = "/store/order/{orderId}";
      path = path.Replace("{format}", "json");
      path = path.Replace("{" + "orderId" + "}", apiClient.ParameterToString(OrderId));
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

      
      
      
      

      // make the HTTP request
      IRestResponse response = (IRestResponse) apiClient.CallApi(path, Method.DELETE, queryParams, postBody, headerParams, formParams, fileParams);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling DeleteOrder: " + response.Content);
      }
      
      return;
    }
    
  }
  
}
