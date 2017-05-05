using System;
using System.Collections.Generic;
using RestSharp;
using IO.Swagger.Client;
using IO.Swagger.Model;

namespace IO.Swagger.Api
{
    /// <summary>
    /// Represents a collection of functions to interact with the API endpoints
    /// </summary>
    public interface IStoreApi
    {
        /// <summary>
        /// Delete purchase order by ID For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
        /// </summary>
        /// <param name="orderId">ID of the order that needs to be deleted</param>
        /// <returns></returns>
        void DeleteOrder (string orderId);
        /// <summary>
        /// Returns pet inventories by status Returns a map of status codes to quantities
        /// </summary>
        /// <returns>Dictionary&lt;string, int?&gt;</returns>
        Dictionary<string, int?> GetInventory ();
        /// <summary>
        /// Find purchase order by ID For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
        /// </summary>
        /// <param name="orderId">ID of pet that needs to be fetched</param>
        /// <returns>Order</returns>
        Order GetOrderById (long? orderId);
        /// <summary>
        /// Place an order for a pet 
        /// </summary>
        /// <param name="body">order placed for purchasing the pet</param>
        /// <returns>Order</returns>
        Order PlaceOrder (Order body);
    }
  
    /// <summary>
    /// Represents a collection of functions to interact with the API endpoints
    /// </summary>
    public class StoreApi : IStoreApi
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="StoreApi"/> class.
        /// </summary>
        /// <param name="apiClient"> an instance of ApiClient (optional)</param>
        /// <returns></returns>
        public StoreApi(ApiClient apiClient = null)
        {
            if (apiClient == null) // use the default one in Configuration
                this.ApiClient = Configuration.DefaultApiClient; 
            else
                this.ApiClient = apiClient;
        }
    
        /// <summary>
        /// Initializes a new instance of the <see cref="StoreApi"/> class.
        /// </summary>
        /// <returns></returns>
        public StoreApi(String basePath)
        {
            this.ApiClient = new ApiClient(basePath);
        }
    
        /// <summary>
        /// Sets the base path of the API client.
        /// </summary>
        /// <param name="basePath">The base path</param>
        /// <value>The base path</value>
        public void SetBasePath(String basePath)
        {
            this.ApiClient.BasePath = basePath;
        }
    
        /// <summary>
        /// Gets the base path of the API client.
        /// </summary>
        /// <param name="basePath">The base path</param>
        /// <value>The base path</value>
        public String GetBasePath(String basePath)
        {
            return this.ApiClient.BasePath;
        }
    
        /// <summary>
        /// Gets or sets the API client.
        /// </summary>
        /// <value>An instance of the ApiClient</value>
        public ApiClient ApiClient {get; set;}
    
        /// <summary>
        /// Delete purchase order by ID For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
        /// </summary>
        /// <param name="orderId">ID of the order that needs to be deleted</param> 
        /// <returns></returns>            
        public void DeleteOrder (string orderId)
        {
            
            // verify the required parameter 'orderId' is set
            if (orderId == null) throw new ApiException(400, "Missing required parameter 'orderId' when calling DeleteOrder");
            
    
            var path = "/store/order/{orderId}";
            path = path.Replace("{format}", "json");
            path = path.Replace("{" + "orderId" + "}", ApiClient.ParameterToString(orderId));
    
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;
    
                                                    
            // authentication setting, if any
            String[] authSettings = new String[] {  };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) ApiClient.CallApi(path, Method.DELETE, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
    
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling DeleteOrder: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException ((int)response.StatusCode, "Error calling DeleteOrder: " + response.ErrorMessage, response.ErrorMessage);
    
            return;
        }
    
        /// <summary>
        /// Returns pet inventories by status Returns a map of status codes to quantities
        /// </summary>
        /// <returns>Dictionary&lt;string, int?&gt;</returns>            
        public Dictionary<string, int?> GetInventory ()
        {
            
    
            var path = "/store/inventory";
            path = path.Replace("{format}", "json");
                
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;
    
                                                    
            // authentication setting, if any
            String[] authSettings = new String[] { "api_key" };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) ApiClient.CallApi(path, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
    
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling GetInventory: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException ((int)response.StatusCode, "Error calling GetInventory: " + response.ErrorMessage, response.ErrorMessage);
    
            return (Dictionary<string, int?>) ApiClient.Deserialize(response.Content, typeof(Dictionary<string, int?>), response.Headers);
        }
    
        /// <summary>
        /// Find purchase order by ID For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
        /// </summary>
        /// <param name="orderId">ID of pet that needs to be fetched</param> 
        /// <returns>Order</returns>            
        public Order GetOrderById (long? orderId)
        {
            
            // verify the required parameter 'orderId' is set
            if (orderId == null) throw new ApiException(400, "Missing required parameter 'orderId' when calling GetOrderById");
            
    
            var path = "/store/order/{orderId}";
            path = path.Replace("{format}", "json");
            path = path.Replace("{" + "orderId" + "}", ApiClient.ParameterToString(orderId));
    
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;
    
                                                    
            // authentication setting, if any
            String[] authSettings = new String[] {  };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) ApiClient.CallApi(path, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
    
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling GetOrderById: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException ((int)response.StatusCode, "Error calling GetOrderById: " + response.ErrorMessage, response.ErrorMessage);
    
            return (Order) ApiClient.Deserialize(response.Content, typeof(Order), response.Headers);
        }
    
        /// <summary>
        /// Place an order for a pet 
        /// </summary>
        /// <param name="body">order placed for purchasing the pet</param> 
        /// <returns>Order</returns>            
        public Order PlaceOrder (Order body)
        {
            
            // verify the required parameter 'body' is set
            if (body == null) throw new ApiException(400, "Missing required parameter 'body' when calling PlaceOrder");
            
    
            var path = "/store/order";
            path = path.Replace("{format}", "json");
                
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;
    
                                                postBody = ApiClient.Serialize(body); // http body (model) parameter
    
            // authentication setting, if any
            String[] authSettings = new String[] {  };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) ApiClient.CallApi(path, Method.POST, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
    
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling PlaceOrder: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException ((int)response.StatusCode, "Error calling PlaceOrder: " + response.ErrorMessage, response.ErrorMessage);
    
            return (Order) ApiClient.Deserialize(response.Content, typeof(Order), response.Headers);
        }
    
    }
}
