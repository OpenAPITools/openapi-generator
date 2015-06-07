using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using RestSharp;
using IO.Swagger.Client;
using IO.Swagger.Model;

namespace IO.Swagger.Api {
  

  public interface IStoreApi {
    
    /// <summary>
    /// Returns pet inventories by status Returns a map of status codes to quantities
    /// </summary>
    
    /// <returns>Dictionary<String, int?></returns>
    Dictionary<String, int?> GetInventory ();

    /// <summary>
    /// Returns pet inventories by status Returns a map of status codes to quantities
    /// </summary>
    
    /// <returns>Dictionary<String, int?></returns>
    Task<Dictionary<String, int?>> GetInventoryAsync ();
    
    /// <summary>
    /// Place an order for a pet 
    /// </summary>
    /// <param name="Body">order placed for purchasing the pet</param>
    /// <returns>Order</returns>
    Order PlaceOrder (Order Body);

    /// <summary>
    /// Place an order for a pet 
    /// </summary>
    /// <param name="Body">order placed for purchasing the pet</param>
    /// <returns>Order</returns>
    Task<Order> PlaceOrderAsync (Order Body);
    
    /// <summary>
    /// Find purchase order by ID For valid response try integer IDs with value &lt;= 5 or &gt; 10. Other values will generated exceptions
    /// </summary>
    /// <param name="OrderId">ID of pet that needs to be fetched</param>
    /// <returns>Order</returns>
    Order GetOrderById (string OrderId);

    /// <summary>
    /// Find purchase order by ID For valid response try integer IDs with value &lt;= 5 or &gt; 10. Other values will generated exceptions
    /// </summary>
    /// <param name="OrderId">ID of pet that needs to be fetched</param>
    /// <returns>Order</returns>
    Task<Order> GetOrderByIdAsync (string OrderId);
    
    /// <summary>
    /// Delete purchase order by ID For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
    /// </summary>
    /// <param name="OrderId">ID of the order that needs to be deleted</param>
    /// <returns></returns>
    void DeleteOrder (string OrderId);

    /// <summary>
    /// Delete purchase order by ID For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
    /// </summary>
    /// <param name="OrderId">ID of the order that needs to be deleted</param>
    /// <returns></returns>
    Task DeleteOrderAsync (string OrderId);
    
  }

  /// <summary>
  /// Represents a collection of functions to interact with the API endpoints
  /// </summary>
  public class StoreApi : IStoreApi {

    /// <summary>
    /// Initializes a new instance of the <see cref="StoreApi"/> class.
    /// </summary>
    /// <param name="apiClient"> an instance of ApiClient (optional)
    /// <returns></returns>
    public StoreApi(ApiClient apiClient = null) {
      if (apiClient == null) { // use the default one in Configuration
        this.apiClient = Configuration.apiClient; 
      } else {
        this.apiClient = apiClient;
      }
    }

    /// <summary>
    /// Initializes a new instance of the <see cref="StoreApi"/> class.
    /// </summary>
    /// <returns></returns>
    public StoreApi(String basePath)
    {
      this.apiClient = new ApiClient(basePath);
    }

    /// <summary>
    /// Sets the base path of the API client.
    /// </summary>
    /// <value>The base path</value>
    public void SetBasePath(String basePath) {
      this.apiClient.basePath = basePath;
    }

    /// <summary>
    /// Gets the base path of the API client.
    /// </summary>
    /// <value>The base path</value>
    public String GetBasePath(String basePath) {
      return this.apiClient.basePath;
    }

    /// <summary>
    /// Gets or sets the API client.
    /// </summary>
    /// <value>The API client</value>
    public ApiClient apiClient {get; set;}


    
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

      
      
      
      

      // authentication setting, if any
      String[] authSettings = new String[] { "api_key" };

      // make the HTTP request
      IRestResponse response = (IRestResponse) apiClient.CallApi(path, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, authSettings);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling GetInventory: " + response.Content, response.Content);
      }
      return (Dictionary<String, int?>) apiClient.Deserialize(response.Content, typeof(Dictionary<String, int?>));
    }
	
	 /// <summary>
    /// Returns pet inventories by status Returns a map of status codes to quantities
    /// </summary>
    
    /// <returns>Dictionary<String, int?></returns>
    public async Task<Dictionary<String, int?>> GetInventoryAsync () {

      

      var path = "/store/inventory";
      path = path.Replace("{format}", "json");
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

      
      
      
      

      // authentication setting, if any
      String[] authSettings = new String[] { "api_key" };

      // make the HTTP request
      IRestResponse response = (IRestResponse) await apiClient.CallApiAsync(path, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling GetInventory: " + response.Content, response.Content);
      }
      return (Dictionary<String, int?>) ApiInvoker.Deserialize(response.Content, typeof(Dictionary<String, int?>));
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
      

      // authentication setting, if any
      String[] authSettings = new String[] {  };

      // make the HTTP request
      IRestResponse response = (IRestResponse) apiClient.CallApi(path, Method.POST, queryParams, postBody, headerParams, formParams, fileParams, authSettings);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling PlaceOrder: " + response.Content, response.Content);
      }
      return (Order) apiClient.Deserialize(response.Content, typeof(Order));
    }
	
	 /// <summary>
    /// Place an order for a pet 
    /// </summary>
    /// <param name="Body">order placed for purchasing the pet</param>
    /// <returns>Order</returns>
    public async Task<Order> PlaceOrderAsync (Order Body) {

      

      var path = "/store/order";
      path = path.Replace("{format}", "json");
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

      
      
      
      postBody = apiClient.Serialize(Body); // http body (model) parameter
      

      // authentication setting, if any
      String[] authSettings = new String[] {  };

      // make the HTTP request
      IRestResponse response = (IRestResponse) await apiClient.CallApiAsync(path, Method.POST, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling PlaceOrder: " + response.Content, response.Content);
      }
      return (Order) ApiInvoker.Deserialize(response.Content, typeof(Order));
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

      
      
      
      

      // authentication setting, if any
      String[] authSettings = new String[] {  };

      // make the HTTP request
      IRestResponse response = (IRestResponse) apiClient.CallApi(path, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, authSettings);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling GetOrderById: " + response.Content, response.Content);
      }
      return (Order) apiClient.Deserialize(response.Content, typeof(Order));
    }
	
	 /// <summary>
    /// Find purchase order by ID For valid response try integer IDs with value &lt;= 5 or &gt; 10. Other values will generated exceptions
    /// </summary>
    /// <param name="OrderId">ID of pet that needs to be fetched</param>
    /// <returns>Order</returns>
    public async Task<Order> GetOrderByIdAsync (string OrderId) {

      
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

      
      
      
      

      // authentication setting, if any
      String[] authSettings = new String[] {  };

      // make the HTTP request
      IRestResponse response = (IRestResponse) await apiClient.CallApiAsync(path, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling GetOrderById: " + response.Content, response.Content);
      }
      return (Order) ApiInvoker.Deserialize(response.Content, typeof(Order));
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

      
      
      
      

      // authentication setting, if any
      String[] authSettings = new String[] {  };

      // make the HTTP request
      IRestResponse response = (IRestResponse) apiClient.CallApi(path, Method.DELETE, queryParams, postBody, headerParams, formParams, fileParams, authSettings);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling DeleteOrder: " + response.Content, response.Content);
      }
      
      return;
    }
	
	 /// <summary>
    /// Delete purchase order by ID For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
    /// </summary>
    /// <param name="OrderId">ID of the order that needs to be deleted</param>
    /// <returns></returns>
    public async Task DeleteOrderAsync (string OrderId) {

      
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

      
      
      
      

      // authentication setting, if any
      String[] authSettings = new String[] {  };

      // make the HTTP request
      IRestResponse response = (IRestResponse) await apiClient.CallApiAsync(path, Method.DELETE, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling DeleteOrder: " + response.Content, response.Content);
      }
      
      return;
    }
    
  }  
  
}
