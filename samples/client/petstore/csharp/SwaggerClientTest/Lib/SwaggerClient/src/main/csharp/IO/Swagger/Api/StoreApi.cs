using System;
using System.IO;
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
        /// Returns pet inventories by status
        /// </summary>
        /// <remarks>
        /// Returns a map of status codes to quantities
        /// </remarks>
        /// <returns></returns>
        Dictionary<string, int?> GetInventory ();
  
        /// <summary>
        /// Returns pet inventories by status
        /// </summary>
        /// <remarks>
        /// Returns a map of status codes to quantities
        /// </remarks>
        /// <returns></returns>
        System.Threading.Tasks.Task<Dictionary<string, int?>> GetInventoryAsync ();
        
        /// <summary>
        /// Place an order for a pet
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <param name="body">order placed for purchasing the pet</param>
        /// <returns>Order</returns>
        Order PlaceOrder (Order body);
  
        /// <summary>
        /// Place an order for a pet
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <param name="body">order placed for purchasing the pet</param>
        /// <returns>Order</returns>
        System.Threading.Tasks.Task<Order> PlaceOrderAsync (Order body);
        
        /// <summary>
        /// Find purchase order by ID
        /// </summary>
        /// <remarks>
        /// For valid response try integer IDs with value &lt;= 5 or &gt; 10. Other values will generated exceptions
        /// </remarks>
        /// <param name="orderId">ID of pet that needs to be fetched</param>
        /// <returns>Order</returns>
        Order GetOrderById (string orderId);
  
        /// <summary>
        /// Find purchase order by ID
        /// </summary>
        /// <remarks>
        /// For valid response try integer IDs with value &lt;= 5 or &gt; 10. Other values will generated exceptions
        /// </remarks>
        /// <param name="orderId">ID of pet that needs to be fetched</param>
        /// <returns>Order</returns>
        System.Threading.Tasks.Task<Order> GetOrderByIdAsync (string orderId);
        
        /// <summary>
        /// Delete purchase order by ID
        /// </summary>
        /// <remarks>
        /// For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
        /// </remarks>
        /// <param name="orderId">ID of the order that needs to be deleted</param>
        /// <returns></returns>
        void DeleteOrder (string orderId);
  
        /// <summary>
        /// Delete purchase order by ID
        /// </summary>
        /// <remarks>
        /// For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
        /// </remarks>
        /// <param name="orderId">ID of the order that needs to be deleted</param>
        /// <returns></returns>
        System.Threading.Tasks.Task DeleteOrderAsync (string orderId);
        
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
        /// <value>The base path</value>
        public String GetBasePath()
        {
            return this.ApiClient.BasePath;
        }
    
        /// <summary>
        /// Gets or sets the API client.
        /// </summary>
        /// <value>An instance of the ApiClient</value>
        public ApiClient ApiClient {get; set;}
    
        
        /// <summary>
        /// Returns pet inventories by status Returns a map of status codes to quantities
        /// </summary>
        /// <returns></returns>            
        public Dictionary<string, int?> GetInventory ()
        {
            
    
            var path_ = "/store/inventory";
    
            var pathParams = new Dictionary<String, String>();
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;

            // to determine the Accept header
            String[] http_header_accepts = new String[] {
                "application/json", "application/xml"
            };
            String http_header_accept = ApiClient.SelectHeaderAccept(http_header_accepts);
            if (http_header_accept != null)
                headerParams.Add("Accept", ApiClient.SelectHeaderAccept(http_header_accepts));

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            pathParams.Add("format", "json");
            
            
            
            
            
    
            // authentication setting, if any
            String[] authSettings = new String[] { "api_key" };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) ApiClient.CallApi(path_, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, pathParams, authSettings);
    
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling GetInventory: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException ((int)response.StatusCode, "Error calling GetInventory: " + response.ErrorMessage, response.ErrorMessage);
    
            return (Dictionary<string, int?>) ApiClient.Deserialize(response, typeof(Dictionary<string, int?>));
        }
    
        /// <summary>
        /// Returns pet inventories by status Returns a map of status codes to quantities
        /// </summary>
        /// <returns></returns>
        public async System.Threading.Tasks.Task<Dictionary<string, int?>> GetInventoryAsync ()
        {
            
    
            var path_ = "/store/inventory";
    
            var pathParams = new Dictionary<String, String>();
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;

            // to determine the Accept header
            String[] http_header_accepts = new String[] {
                "application/json", "application/xml"
            };
            String http_header_accept = ApiClient.SelectHeaderAccept(http_header_accepts);
            if (http_header_accept != null)
                headerParams.Add("Accept", ApiClient.SelectHeaderAccept(http_header_accepts));

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            pathParams.Add("format", "json");
            
            
            
            
            
    
            // authentication setting, if any
            String[] authSettings = new String[] { "api_key" };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) await ApiClient.CallApiAsync(path_, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, pathParams, authSettings);
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling GetInventory: " + response.Content, response.Content);

            return (Dictionary<string, int?>) ApiClient.Deserialize(response, typeof(Dictionary<string, int?>));
        }
        
        /// <summary>
        /// Place an order for a pet 
        /// </summary>
        /// <param name="body">order placed for purchasing the pet</param> 
        /// <returns>Order</returns>            
        public Order PlaceOrder (Order body)
        {
            
    
            var path_ = "/store/order";
    
            var pathParams = new Dictionary<String, String>();
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;

            // to determine the Accept header
            String[] http_header_accepts = new String[] {
                "application/json", "application/xml"
            };
            String http_header_accept = ApiClient.SelectHeaderAccept(http_header_accepts);
            if (http_header_accept != null)
                headerParams.Add("Accept", ApiClient.SelectHeaderAccept(http_header_accepts));

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            pathParams.Add("format", "json");
            
            
            
            
            postBody = ApiClient.Serialize(body); // http body (model) parameter
            
    
            // authentication setting, if any
            String[] authSettings = new String[] {  };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) ApiClient.CallApi(path_, Method.POST, queryParams, postBody, headerParams, formParams, fileParams, pathParams, authSettings);
    
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling PlaceOrder: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException ((int)response.StatusCode, "Error calling PlaceOrder: " + response.ErrorMessage, response.ErrorMessage);
    
            return (Order) ApiClient.Deserialize(response, typeof(Order));
        }
    
        /// <summary>
        /// Place an order for a pet 
        /// </summary>
        /// <param name="body">order placed for purchasing the pet</param>
        /// <returns>Order</returns>
        public async System.Threading.Tasks.Task<Order> PlaceOrderAsync (Order body)
        {
            
    
            var path_ = "/store/order";
    
            var pathParams = new Dictionary<String, String>();
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;

            // to determine the Accept header
            String[] http_header_accepts = new String[] {
                "application/json", "application/xml"
            };
            String http_header_accept = ApiClient.SelectHeaderAccept(http_header_accepts);
            if (http_header_accept != null)
                headerParams.Add("Accept", ApiClient.SelectHeaderAccept(http_header_accepts));

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            pathParams.Add("format", "json");
            
            
            
            
            postBody = ApiClient.Serialize(body); // http body (model) parameter
            
    
            // authentication setting, if any
            String[] authSettings = new String[] {  };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) await ApiClient.CallApiAsync(path_, Method.POST, queryParams, postBody, headerParams, formParams, fileParams, pathParams, authSettings);
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling PlaceOrder: " + response.Content, response.Content);

            return (Order) ApiClient.Deserialize(response, typeof(Order));
        }
        
        /// <summary>
        /// Find purchase order by ID For valid response try integer IDs with value &lt;= 5 or &gt; 10. Other values will generated exceptions
        /// </summary>
        /// <param name="orderId">ID of pet that needs to be fetched</param> 
        /// <returns>Order</returns>            
        public Order GetOrderById (string orderId)
        {
            
            // verify the required parameter 'orderId' is set
            if (orderId == null) throw new ApiException(400, "Missing required parameter 'orderId' when calling GetOrderById");
            
    
            var path_ = "/store/order/{orderId}";
    
            var pathParams = new Dictionary<String, String>();
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;

            // to determine the Accept header
            String[] http_header_accepts = new String[] {
                "application/json", "application/xml"
            };
            String http_header_accept = ApiClient.SelectHeaderAccept(http_header_accepts);
            if (http_header_accept != null)
                headerParams.Add("Accept", ApiClient.SelectHeaderAccept(http_header_accepts));

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            pathParams.Add("format", "json");
            if (orderId != null) pathParams.Add("orderId", ApiClient.ParameterToString(orderId)); // path parameter
            
            
            
            
            
    
            // authentication setting, if any
            String[] authSettings = new String[] {  };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) ApiClient.CallApi(path_, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, pathParams, authSettings);
    
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling GetOrderById: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException ((int)response.StatusCode, "Error calling GetOrderById: " + response.ErrorMessage, response.ErrorMessage);
    
            return (Order) ApiClient.Deserialize(response, typeof(Order));
        }
    
        /// <summary>
        /// Find purchase order by ID For valid response try integer IDs with value &lt;= 5 or &gt; 10. Other values will generated exceptions
        /// </summary>
        /// <param name="orderId">ID of pet that needs to be fetched</param>
        /// <returns>Order</returns>
        public async System.Threading.Tasks.Task<Order> GetOrderByIdAsync (string orderId)
        {
            // verify the required parameter 'orderId' is set
            if (orderId == null) throw new ApiException(400, "Missing required parameter 'orderId' when calling GetOrderById");
            
    
            var path_ = "/store/order/{orderId}";
    
            var pathParams = new Dictionary<String, String>();
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;

            // to determine the Accept header
            String[] http_header_accepts = new String[] {
                "application/json", "application/xml"
            };
            String http_header_accept = ApiClient.SelectHeaderAccept(http_header_accepts);
            if (http_header_accept != null)
                headerParams.Add("Accept", ApiClient.SelectHeaderAccept(http_header_accepts));

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            pathParams.Add("format", "json");
            if (orderId != null) pathParams.Add("orderId", ApiClient.ParameterToString(orderId)); // path parameter
            
            
            
            
            
    
            // authentication setting, if any
            String[] authSettings = new String[] {  };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) await ApiClient.CallApiAsync(path_, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, pathParams, authSettings);
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling GetOrderById: " + response.Content, response.Content);

            return (Order) ApiClient.Deserialize(response, typeof(Order));
        }
        
        /// <summary>
        /// Delete purchase order by ID For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
        /// </summary>
        /// <param name="orderId">ID of the order that needs to be deleted</param> 
        /// <returns></returns>            
        public void DeleteOrder (string orderId)
        {
            
            // verify the required parameter 'orderId' is set
            if (orderId == null) throw new ApiException(400, "Missing required parameter 'orderId' when calling DeleteOrder");
            
    
            var path_ = "/store/order/{orderId}";
    
            var pathParams = new Dictionary<String, String>();
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;

            // to determine the Accept header
            String[] http_header_accepts = new String[] {
                "application/json", "application/xml"
            };
            String http_header_accept = ApiClient.SelectHeaderAccept(http_header_accepts);
            if (http_header_accept != null)
                headerParams.Add("Accept", ApiClient.SelectHeaderAccept(http_header_accepts));

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            pathParams.Add("format", "json");
            if (orderId != null) pathParams.Add("orderId", ApiClient.ParameterToString(orderId)); // path parameter
            
            
            
            
            
    
            // authentication setting, if any
            String[] authSettings = new String[] {  };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) ApiClient.CallApi(path_, Method.DELETE, queryParams, postBody, headerParams, formParams, fileParams, pathParams, authSettings);
    
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling DeleteOrder: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException ((int)response.StatusCode, "Error calling DeleteOrder: " + response.ErrorMessage, response.ErrorMessage);
    
            return;
        }
    
        /// <summary>
        /// Delete purchase order by ID For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
        /// </summary>
        /// <param name="orderId">ID of the order that needs to be deleted</param>
        /// <returns></returns>
        public async System.Threading.Tasks.Task DeleteOrderAsync (string orderId)
        {
            // verify the required parameter 'orderId' is set
            if (orderId == null) throw new ApiException(400, "Missing required parameter 'orderId' when calling DeleteOrder");
            
    
            var path_ = "/store/order/{orderId}";
    
            var pathParams = new Dictionary<String, String>();
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;

            // to determine the Accept header
            String[] http_header_accepts = new String[] {
                "application/json", "application/xml"
            };
            String http_header_accept = ApiClient.SelectHeaderAccept(http_header_accepts);
            if (http_header_accept != null)
                headerParams.Add("Accept", ApiClient.SelectHeaderAccept(http_header_accepts));

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            pathParams.Add("format", "json");
            if (orderId != null) pathParams.Add("orderId", ApiClient.ParameterToString(orderId)); // path parameter
            
            
            
            
            
    
            // authentication setting, if any
            String[] authSettings = new String[] {  };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) await ApiClient.CallApiAsync(path_, Method.DELETE, queryParams, postBody, headerParams, formParams, fileParams, pathParams, authSettings);
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling DeleteOrder: " + response.Content, response.Content);

            
            return;
        }
        
    }
    
}
