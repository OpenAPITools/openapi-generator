using System;
using System.IO;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
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
        #region Synchronous Operations
        
        /// <summary>
        /// Finds orders by status
        /// </summary>
        /// <remarks>
        /// A single status value can be provided as a string
        /// </remarks>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="status">Status value that needs to be considered for query</param>
        /// <returns>List&lt;Order&gt;</returns>
        List<Order> FindOrdersByStatus (string status = null);
  
        /// <summary>
        /// Finds orders by status
        /// </summary>
        /// <remarks>
        /// A single status value can be provided as a string
        /// </remarks>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="status">Status value that needs to be considered for query</param>
        /// <returns>ApiResponse of List&lt;Order&gt;</returns>
        ApiResponse<List<Order>> FindOrdersByStatusWithHttpInfo (string status = null);
        
        /// <summary>
        /// Returns pet inventories by status
        /// </summary>
        /// <remarks>
        /// Returns a map of status codes to quantities
        /// </remarks>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <returns>Dictionary&lt;string, int?&gt;</returns>
        Dictionary<string, int?> GetInventory ();
  
        /// <summary>
        /// Returns pet inventories by status
        /// </summary>
        /// <remarks>
        /// Returns a map of status codes to quantities
        /// </remarks>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <returns>ApiResponse of Dictionary&lt;string, int?&gt;</returns>
        ApiResponse<Dictionary<string, int?>> GetInventoryWithHttpInfo ();
        
        /// <summary>
        /// Place an order for a pet
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="body">order placed for purchasing the pet</param>
        /// <returns>Order</returns>
        Order PlaceOrder (Order body = null);
  
        /// <summary>
        /// Place an order for a pet
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="body">order placed for purchasing the pet</param>
        /// <returns>ApiResponse of Order</returns>
        ApiResponse<Order> PlaceOrderWithHttpInfo (Order body = null);
        
        /// <summary>
        /// Find purchase order by ID
        /// </summary>
        /// <remarks>
        /// For valid response try integer IDs with value &lt;= 5 or &gt; 10. Other values will generated exceptions
        /// </remarks>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="orderId">ID of pet that needs to be fetched</param>
        /// <returns>Order</returns>
        Order GetOrderById (string orderId);
  
        /// <summary>
        /// Find purchase order by ID
        /// </summary>
        /// <remarks>
        /// For valid response try integer IDs with value &lt;= 5 or &gt; 10. Other values will generated exceptions
        /// </remarks>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="orderId">ID of pet that needs to be fetched</param>
        /// <returns>ApiResponse of Order</returns>
        ApiResponse<Order> GetOrderByIdWithHttpInfo (string orderId);
        
        /// <summary>
        /// Delete purchase order by ID
        /// </summary>
        /// <remarks>
        /// For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
        /// </remarks>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="orderId">ID of the order that needs to be deleted</param>
        /// <returns></returns>
        void DeleteOrder (string orderId);
  
        /// <summary>
        /// Delete purchase order by ID
        /// </summary>
        /// <remarks>
        /// For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
        /// </remarks>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="orderId">ID of the order that needs to be deleted</param>
        /// <returns>ApiResponse of Object(void)</returns>
        ApiResponse<Object> DeleteOrderWithHttpInfo (string orderId);
        
        #endregion Synchronous Operations
        
        #region Asynchronous Operations
        
        /// <summary>
        /// Finds orders by status
        /// </summary>
        /// <remarks>
        /// A single status value can be provided as a string
        /// </remarks>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="status">Status value that needs to be considered for query</param>
        /// <returns>Task of List&lt;Order&gt;</returns>
        System.Threading.Tasks.Task<List<Order>> FindOrdersByStatusAsync (string status = null);

        /// <summary>
        /// Finds orders by status
        /// </summary>
        /// <remarks>
        /// A single status value can be provided as a string
        /// </remarks>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="status">Status value that needs to be considered for query</param>
        /// <returns>Task of ApiResponse (List&lt;Order&gt;)</returns>
        System.Threading.Tasks.Task<ApiResponse<List<Order>>> FindOrdersByStatusAsyncWithHttpInfo (string status = null);
        
        /// <summary>
        /// Returns pet inventories by status
        /// </summary>
        /// <remarks>
        /// Returns a map of status codes to quantities
        /// </remarks>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <returns>Task of Dictionary&lt;string, int?&gt;</returns>
        System.Threading.Tasks.Task<Dictionary<string, int?>> GetInventoryAsync ();

        /// <summary>
        /// Returns pet inventories by status
        /// </summary>
        /// <remarks>
        /// Returns a map of status codes to quantities
        /// </remarks>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <returns>Task of ApiResponse (Dictionary&lt;string, int?&gt;)</returns>
        System.Threading.Tasks.Task<ApiResponse<Dictionary<string, int?>>> GetInventoryAsyncWithHttpInfo ();
        
        /// <summary>
        /// Place an order for a pet
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="body">order placed for purchasing the pet</param>
        /// <returns>Task of Order</returns>
        System.Threading.Tasks.Task<Order> PlaceOrderAsync (Order body = null);

        /// <summary>
        /// Place an order for a pet
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="body">order placed for purchasing the pet</param>
        /// <returns>Task of ApiResponse (Order)</returns>
        System.Threading.Tasks.Task<ApiResponse<Order>> PlaceOrderAsyncWithHttpInfo (Order body = null);
        
        /// <summary>
        /// Find purchase order by ID
        /// </summary>
        /// <remarks>
        /// For valid response try integer IDs with value &lt;= 5 or &gt; 10. Other values will generated exceptions
        /// </remarks>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="orderId">ID of pet that needs to be fetched</param>
        /// <returns>Task of Order</returns>
        System.Threading.Tasks.Task<Order> GetOrderByIdAsync (string orderId);

        /// <summary>
        /// Find purchase order by ID
        /// </summary>
        /// <remarks>
        /// For valid response try integer IDs with value &lt;= 5 or &gt; 10. Other values will generated exceptions
        /// </remarks>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="orderId">ID of pet that needs to be fetched</param>
        /// <returns>Task of ApiResponse (Order)</returns>
        System.Threading.Tasks.Task<ApiResponse<Order>> GetOrderByIdAsyncWithHttpInfo (string orderId);
        
        /// <summary>
        /// Delete purchase order by ID
        /// </summary>
        /// <remarks>
        /// For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
        /// </remarks>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="orderId">ID of the order that needs to be deleted</param>
        /// <returns>Task of void</returns>
        System.Threading.Tasks.Task DeleteOrderAsync (string orderId);

        /// <summary>
        /// Delete purchase order by ID
        /// </summary>
        /// <remarks>
        /// For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
        /// </remarks>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="orderId">ID of the order that needs to be deleted</param>
        /// <returns>Task of ApiResponse</returns>
        System.Threading.Tasks.Task<ApiResponse<Object>> DeleteOrderAsyncWithHttpInfo (string orderId);
        
        #endregion Asynchronous Operations
        
    }
  
    /// <summary>
    /// Represents a collection of functions to interact with the API endpoints
    /// </summary>
    public class StoreApi : IStoreApi
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="StoreApi"/> class.
        /// </summary>
        /// <returns></returns>
        public StoreApi(String basePath)
        {
            this.Configuration = new Configuration(new ApiClient(basePath));
        }
    
        /// <summary>
        /// Initializes a new instance of the <see cref="StoreApi"/> class
        /// using Configuration object
        /// </summary>
        /// <param name="configuration">An instance of Configuration</param>
        /// <returns></returns>
        public StoreApi(Configuration configuration = null)
        {
            if (configuration == null) // use the default one in Configuration
                this.Configuration = Configuration.Default; 
            else
                this.Configuration = configuration;
        }

        /// <summary>
        /// Gets the base path of the API client.
        /// </summary>
        /// <value>The base path</value>
        public String GetBasePath()
        {
            return this.Configuration.ApiClient.RestClient.BaseUrl.ToString();
        }

        /// <summary>
        /// Sets the base path of the API client.
        /// </summary>
        /// <value>The base path</value>
        [Obsolete("SetBasePath is deprecated, please do 'Configuraiton.ApiClient = new ApiClient(\"http://new-path\")' instead.")]
        public void SetBasePath(String basePath)
        {
            // do nothing
        }
    
        /// <summary>
        /// Gets or sets the configuration object
        /// </summary>
        /// <value>An instance of the Configuration</value>
        public Configuration Configuration {get; set;}

        /// <summary>
        /// Gets the default header.
        /// </summary>
        /// <returns>Dictionary of HTTP header</returns>
        [Obsolete("DefaultHeader is deprecated, please use Configuration.DefaultHeader instead.")]
        public Dictionary<String, String> DefaultHeader()
        {
            return this.Configuration.DefaultHeader;
        }

        /// <summary>
        /// Add default header.
        /// </summary>
        /// <param name="key">Header field name.</param>
        /// <param name="value">Header field value.</param>
        /// <returns></returns>
        [Obsolete("AddDefaultHeader is deprecated, please use Configuration.AddDefaultHeader instead.")]
        public void AddDefaultHeader(string key, string value)
        {
            this.Configuration.AddDefaultHeader(key, value);
        }
   
        
        /// <summary>
        /// Finds orders by status A single status value can be provided as a string
        /// </summary>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="status">Status value that needs to be considered for query</param> 
        /// <returns>List&lt;Order&gt;</returns>
        public List<Order> FindOrdersByStatus (string status = null)
        {
             ApiResponse<List<Order>> localVarResponse = FindOrdersByStatusWithHttpInfo(status);
             return localVarResponse.Data;
        }

        /// <summary>
        /// Finds orders by status A single status value can be provided as a string
        /// </summary>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="status">Status value that needs to be considered for query</param> 
        /// <returns>ApiResponse of List&lt;Order&gt;</returns>
        public ApiResponse< List<Order> > FindOrdersByStatusWithHttpInfo (string status = null)
        {
            
    
            var localVarPath = "/store/findByStatus";
    
            var localVarPathParams = new Dictionary<String, String>();
            var localVarQueryParams = new Dictionary<String, String>();
            var localVarHeaderParams = new Dictionary<String, String>(Configuration.DefaultHeader);
            var localVarFormParams = new Dictionary<String, String>();
            var localVarFileParams = new Dictionary<String, FileParameter>();
            Object localVarPostBody = null;

            // to determine the Content-Type header
            String[] localVarHttpContentTypes = new String[] {
                
            };
            String localVarHttpContentType = Configuration.ApiClient.SelectHeaderContentType(localVarHttpContentTypes);

            // to determine the Accept header
            String[] localVarHttpHeaderAccepts = new String[] {
                "application/json", "application/xml"
            };
            String localVarHttpHeaderAccept = Configuration.ApiClient.SelectHeaderAccept(localVarHttpHeaderAccepts);
            if (localVarHttpHeaderAccept != null)
                localVarHeaderParams.Add("Accept", localVarHttpHeaderAccept);

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            localVarPathParams.Add("format", "json");
            
            if (status != null) localVarQueryParams.Add("status", Configuration.ApiClient.ParameterToString(status)); // query parameter
            
            
            
            

            // authentication (test_api_client_id) required
            
            if (!String.IsNullOrEmpty(Configuration.GetApiKeyWithPrefix("x-test_api_client_id")))
            {
                localVarHeaderParams["x-test_api_client_id"] = Configuration.GetApiKeyWithPrefix("x-test_api_client_id");
            }
            // authentication (test_api_client_secret) required
            
            if (!String.IsNullOrEmpty(Configuration.GetApiKeyWithPrefix("x-test_api_client_secret")))
            {
                localVarHeaderParams["x-test_api_client_secret"] = Configuration.GetApiKeyWithPrefix("x-test_api_client_secret");
            }
            
    
            // make the HTTP request
            IRestResponse localVarResponse = (IRestResponse) Configuration.ApiClient.CallApi(localVarPath, 
                Method.GET, localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarFileParams,
                localVarPathParams, localVarHttpContentType);

            int localVarStatusCode = (int) localVarResponse.StatusCode;
    
            if (localVarStatusCode >= 400)
                throw new ApiException (localVarStatusCode, "Error calling FindOrdersByStatus: " + localVarResponse.Content, localVarResponse.Content);
            else if (localVarStatusCode == 0)
                throw new ApiException (localVarStatusCode, "Error calling FindOrdersByStatus: " + localVarResponse.ErrorMessage, localVarResponse.ErrorMessage);
    
            return new ApiResponse<List<Order>>(localVarStatusCode,
                localVarResponse.Headers.ToDictionary(x => x.Name, x => x.Value.ToString()),
                (List<Order>) Configuration.ApiClient.Deserialize(localVarResponse, typeof(List<Order>)));
            
        }

        
        /// <summary>
        /// Finds orders by status A single status value can be provided as a string
        /// </summary>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="status">Status value that needs to be considered for query</param>
        /// <returns>Task of List&lt;Order&gt;</returns>
        public async System.Threading.Tasks.Task<List<Order>> FindOrdersByStatusAsync (string status = null)
        {
             ApiResponse<List<Order>> localVarResponse = await FindOrdersByStatusAsyncWithHttpInfo(status);
             return localVarResponse.Data;

        }

        /// <summary>
        /// Finds orders by status A single status value can be provided as a string
        /// </summary>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="status">Status value that needs to be considered for query</param>
        /// <returns>Task of ApiResponse (List&lt;Order&gt;)</returns>
        public async System.Threading.Tasks.Task<ApiResponse<List<Order>>> FindOrdersByStatusAsyncWithHttpInfo (string status = null)
        {
            
    
            var localVarPath = "/store/findByStatus";
    
            var localVarPathParams = new Dictionary<String, String>();
            var localVarQueryParams = new Dictionary<String, String>();
            var localVarHeaderParams = new Dictionary<String, String>(Configuration.DefaultHeader);
            var localVarFormParams = new Dictionary<String, String>();
            var localVarFileParams = new Dictionary<String, FileParameter>();
            Object localVarPostBody = null;

            // to determine the Content-Type header
            String[] localVarHttpContentTypes = new String[] {
                
            };
            String localVarHttpContentType = Configuration.ApiClient.SelectHeaderContentType(localVarHttpContentTypes);

            // to determine the Accept header
            String[] localVarHttpHeaderAccepts = new String[] {
                "application/json", "application/xml"
            };
            String localVarHttpHeaderAccept = Configuration.ApiClient.SelectHeaderAccept(localVarHttpHeaderAccepts);
            if (localVarHttpHeaderAccept != null)
                localVarHeaderParams.Add("Accept", localVarHttpHeaderAccept);

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            localVarPathParams.Add("format", "json");
            
            if (status != null) localVarQueryParams.Add("status", Configuration.ApiClient.ParameterToString(status)); // query parameter
            
            
            
            

            
            // authentication (test_api_client_id) required
            
            if (!String.IsNullOrEmpty(Configuration.GetApiKeyWithPrefix("x-test_api_client_id")))
            {
                localVarHeaderParams["x-test_api_client_id"] = Configuration.GetApiKeyWithPrefix("x-test_api_client_id");
            }
            
            // authentication (test_api_client_secret) required
            
            if (!String.IsNullOrEmpty(Configuration.GetApiKeyWithPrefix("x-test_api_client_secret")))
            {
                localVarHeaderParams["x-test_api_client_secret"] = Configuration.GetApiKeyWithPrefix("x-test_api_client_secret");
            }
            

            // make the HTTP request
            IRestResponse localVarResponse = (IRestResponse) await Configuration.ApiClient.CallApiAsync(localVarPath, 
                Method.GET, localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarFileParams, 
                localVarPathParams, localVarHttpContentType);

            int localVarStatusCode = (int) localVarResponse.StatusCode;
 
            if (localVarStatusCode >= 400)
                throw new ApiException (localVarStatusCode, "Error calling FindOrdersByStatus: " + localVarResponse.Content, localVarResponse.Content);
            else if (localVarStatusCode == 0)
                throw new ApiException (localVarStatusCode, "Error calling FindOrdersByStatus: " + localVarResponse.ErrorMessage, localVarResponse.ErrorMessage);

            return new ApiResponse<List<Order>>(localVarStatusCode,
                localVarResponse.Headers.ToDictionary(x => x.Name, x => x.Value.ToString()),
                (List<Order>) Configuration.ApiClient.Deserialize(localVarResponse, typeof(List<Order>)));
            
        }
        
        /// <summary>
        /// Returns pet inventories by status Returns a map of status codes to quantities
        /// </summary>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <returns>Dictionary&lt;string, int?&gt;</returns>
        public Dictionary<string, int?> GetInventory ()
        {
             ApiResponse<Dictionary<string, int?>> localVarResponse = GetInventoryWithHttpInfo();
             return localVarResponse.Data;
        }

        /// <summary>
        /// Returns pet inventories by status Returns a map of status codes to quantities
        /// </summary>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <returns>ApiResponse of Dictionary&lt;string, int?&gt;</returns>
        public ApiResponse< Dictionary<string, int?> > GetInventoryWithHttpInfo ()
        {
            
    
            var localVarPath = "/store/inventory";
    
            var localVarPathParams = new Dictionary<String, String>();
            var localVarQueryParams = new Dictionary<String, String>();
            var localVarHeaderParams = new Dictionary<String, String>(Configuration.DefaultHeader);
            var localVarFormParams = new Dictionary<String, String>();
            var localVarFileParams = new Dictionary<String, FileParameter>();
            Object localVarPostBody = null;

            // to determine the Content-Type header
            String[] localVarHttpContentTypes = new String[] {
                
            };
            String localVarHttpContentType = Configuration.ApiClient.SelectHeaderContentType(localVarHttpContentTypes);

            // to determine the Accept header
            String[] localVarHttpHeaderAccepts = new String[] {
                "application/json", "application/xml"
            };
            String localVarHttpHeaderAccept = Configuration.ApiClient.SelectHeaderAccept(localVarHttpHeaderAccepts);
            if (localVarHttpHeaderAccept != null)
                localVarHeaderParams.Add("Accept", localVarHttpHeaderAccept);

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            localVarPathParams.Add("format", "json");
            
            
            
            
            

            // authentication (api_key) required
            
            if (!String.IsNullOrEmpty(Configuration.GetApiKeyWithPrefix("api_key")))
            {
                localVarHeaderParams["api_key"] = Configuration.GetApiKeyWithPrefix("api_key");
            }
            
    
            // make the HTTP request
            IRestResponse localVarResponse = (IRestResponse) Configuration.ApiClient.CallApi(localVarPath, 
                Method.GET, localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarFileParams,
                localVarPathParams, localVarHttpContentType);

            int localVarStatusCode = (int) localVarResponse.StatusCode;
    
            if (localVarStatusCode >= 400)
                throw new ApiException (localVarStatusCode, "Error calling GetInventory: " + localVarResponse.Content, localVarResponse.Content);
            else if (localVarStatusCode == 0)
                throw new ApiException (localVarStatusCode, "Error calling GetInventory: " + localVarResponse.ErrorMessage, localVarResponse.ErrorMessage);
    
            return new ApiResponse<Dictionary<string, int?>>(localVarStatusCode,
                localVarResponse.Headers.ToDictionary(x => x.Name, x => x.Value.ToString()),
                (Dictionary<string, int?>) Configuration.ApiClient.Deserialize(localVarResponse, typeof(Dictionary<string, int?>)));
            
        }

        
        /// <summary>
        /// Returns pet inventories by status Returns a map of status codes to quantities
        /// </summary>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <returns>Task of Dictionary&lt;string, int?&gt;</returns>
        public async System.Threading.Tasks.Task<Dictionary<string, int?>> GetInventoryAsync ()
        {
             ApiResponse<Dictionary<string, int?>> localVarResponse = await GetInventoryAsyncWithHttpInfo();
             return localVarResponse.Data;

        }

        /// <summary>
        /// Returns pet inventories by status Returns a map of status codes to quantities
        /// </summary>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <returns>Task of ApiResponse (Dictionary&lt;string, int?&gt;)</returns>
        public async System.Threading.Tasks.Task<ApiResponse<Dictionary<string, int?>>> GetInventoryAsyncWithHttpInfo ()
        {
            
    
            var localVarPath = "/store/inventory";
    
            var localVarPathParams = new Dictionary<String, String>();
            var localVarQueryParams = new Dictionary<String, String>();
            var localVarHeaderParams = new Dictionary<String, String>(Configuration.DefaultHeader);
            var localVarFormParams = new Dictionary<String, String>();
            var localVarFileParams = new Dictionary<String, FileParameter>();
            Object localVarPostBody = null;

            // to determine the Content-Type header
            String[] localVarHttpContentTypes = new String[] {
                
            };
            String localVarHttpContentType = Configuration.ApiClient.SelectHeaderContentType(localVarHttpContentTypes);

            // to determine the Accept header
            String[] localVarHttpHeaderAccepts = new String[] {
                "application/json", "application/xml"
            };
            String localVarHttpHeaderAccept = Configuration.ApiClient.SelectHeaderAccept(localVarHttpHeaderAccepts);
            if (localVarHttpHeaderAccept != null)
                localVarHeaderParams.Add("Accept", localVarHttpHeaderAccept);

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            localVarPathParams.Add("format", "json");
            
            
            
            
            

            
            // authentication (api_key) required
            
            if (!String.IsNullOrEmpty(Configuration.GetApiKeyWithPrefix("api_key")))
            {
                localVarHeaderParams["api_key"] = Configuration.GetApiKeyWithPrefix("api_key");
            }
            

            // make the HTTP request
            IRestResponse localVarResponse = (IRestResponse) await Configuration.ApiClient.CallApiAsync(localVarPath, 
                Method.GET, localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarFileParams, 
                localVarPathParams, localVarHttpContentType);

            int localVarStatusCode = (int) localVarResponse.StatusCode;
 
            if (localVarStatusCode >= 400)
                throw new ApiException (localVarStatusCode, "Error calling GetInventory: " + localVarResponse.Content, localVarResponse.Content);
            else if (localVarStatusCode == 0)
                throw new ApiException (localVarStatusCode, "Error calling GetInventory: " + localVarResponse.ErrorMessage, localVarResponse.ErrorMessage);

            return new ApiResponse<Dictionary<string, int?>>(localVarStatusCode,
                localVarResponse.Headers.ToDictionary(x => x.Name, x => x.Value.ToString()),
                (Dictionary<string, int?>) Configuration.ApiClient.Deserialize(localVarResponse, typeof(Dictionary<string, int?>)));
            
        }
        
        /// <summary>
        /// Place an order for a pet 
        /// </summary>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="body">order placed for purchasing the pet</param> 
        /// <returns>Order</returns>
        public Order PlaceOrder (Order body = null)
        {
             ApiResponse<Order> localVarResponse = PlaceOrderWithHttpInfo(body);
             return localVarResponse.Data;
        }

        /// <summary>
        /// Place an order for a pet 
        /// </summary>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="body">order placed for purchasing the pet</param> 
        /// <returns>ApiResponse of Order</returns>
        public ApiResponse< Order > PlaceOrderWithHttpInfo (Order body = null)
        {
            
    
            var localVarPath = "/store/order";
    
            var localVarPathParams = new Dictionary<String, String>();
            var localVarQueryParams = new Dictionary<String, String>();
            var localVarHeaderParams = new Dictionary<String, String>(Configuration.DefaultHeader);
            var localVarFormParams = new Dictionary<String, String>();
            var localVarFileParams = new Dictionary<String, FileParameter>();
            Object localVarPostBody = null;

            // to determine the Content-Type header
            String[] localVarHttpContentTypes = new String[] {
                
            };
            String localVarHttpContentType = Configuration.ApiClient.SelectHeaderContentType(localVarHttpContentTypes);

            // to determine the Accept header
            String[] localVarHttpHeaderAccepts = new String[] {
                "application/json", "application/xml"
            };
            String localVarHttpHeaderAccept = Configuration.ApiClient.SelectHeaderAccept(localVarHttpHeaderAccepts);
            if (localVarHttpHeaderAccept != null)
                localVarHeaderParams.Add("Accept", localVarHttpHeaderAccept);

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            localVarPathParams.Add("format", "json");
            
            
            
            
            if (body.GetType() != typeof(byte[]))
            {
                localVarPostBody = Configuration.ApiClient.Serialize(body); // http body (model) parameter
            }
            else
            {
                localVarPostBody = body; // byte array
            }

            // authentication (test_api_client_id) required
            
            if (!String.IsNullOrEmpty(Configuration.GetApiKeyWithPrefix("x-test_api_client_id")))
            {
                localVarHeaderParams["x-test_api_client_id"] = Configuration.GetApiKeyWithPrefix("x-test_api_client_id");
            }
            // authentication (test_api_client_secret) required
            
            if (!String.IsNullOrEmpty(Configuration.GetApiKeyWithPrefix("x-test_api_client_secret")))
            {
                localVarHeaderParams["x-test_api_client_secret"] = Configuration.GetApiKeyWithPrefix("x-test_api_client_secret");
            }
            
    
            // make the HTTP request
            IRestResponse localVarResponse = (IRestResponse) Configuration.ApiClient.CallApi(localVarPath, 
                Method.POST, localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarFileParams,
                localVarPathParams, localVarHttpContentType);

            int localVarStatusCode = (int) localVarResponse.StatusCode;
    
            if (localVarStatusCode >= 400)
                throw new ApiException (localVarStatusCode, "Error calling PlaceOrder: " + localVarResponse.Content, localVarResponse.Content);
            else if (localVarStatusCode == 0)
                throw new ApiException (localVarStatusCode, "Error calling PlaceOrder: " + localVarResponse.ErrorMessage, localVarResponse.ErrorMessage);
    
            return new ApiResponse<Order>(localVarStatusCode,
                localVarResponse.Headers.ToDictionary(x => x.Name, x => x.Value.ToString()),
                (Order) Configuration.ApiClient.Deserialize(localVarResponse, typeof(Order)));
            
        }

        
        /// <summary>
        /// Place an order for a pet 
        /// </summary>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="body">order placed for purchasing the pet</param>
        /// <returns>Task of Order</returns>
        public async System.Threading.Tasks.Task<Order> PlaceOrderAsync (Order body = null)
        {
             ApiResponse<Order> localVarResponse = await PlaceOrderAsyncWithHttpInfo(body);
             return localVarResponse.Data;

        }

        /// <summary>
        /// Place an order for a pet 
        /// </summary>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="body">order placed for purchasing the pet</param>
        /// <returns>Task of ApiResponse (Order)</returns>
        public async System.Threading.Tasks.Task<ApiResponse<Order>> PlaceOrderAsyncWithHttpInfo (Order body = null)
        {
            
    
            var localVarPath = "/store/order";
    
            var localVarPathParams = new Dictionary<String, String>();
            var localVarQueryParams = new Dictionary<String, String>();
            var localVarHeaderParams = new Dictionary<String, String>(Configuration.DefaultHeader);
            var localVarFormParams = new Dictionary<String, String>();
            var localVarFileParams = new Dictionary<String, FileParameter>();
            Object localVarPostBody = null;

            // to determine the Content-Type header
            String[] localVarHttpContentTypes = new String[] {
                
            };
            String localVarHttpContentType = Configuration.ApiClient.SelectHeaderContentType(localVarHttpContentTypes);

            // to determine the Accept header
            String[] localVarHttpHeaderAccepts = new String[] {
                "application/json", "application/xml"
            };
            String localVarHttpHeaderAccept = Configuration.ApiClient.SelectHeaderAccept(localVarHttpHeaderAccepts);
            if (localVarHttpHeaderAccept != null)
                localVarHeaderParams.Add("Accept", localVarHttpHeaderAccept);

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            localVarPathParams.Add("format", "json");
            
            
            
            
            if (body.GetType() != typeof(byte[]))
            {
                localVarPostBody = Configuration.ApiClient.Serialize(body); // http body (model) parameter
            }
            else
            {
                localVarPostBody = body; // byte array
            }

            
            // authentication (test_api_client_id) required
            
            if (!String.IsNullOrEmpty(Configuration.GetApiKeyWithPrefix("x-test_api_client_id")))
            {
                localVarHeaderParams["x-test_api_client_id"] = Configuration.GetApiKeyWithPrefix("x-test_api_client_id");
            }
            
            // authentication (test_api_client_secret) required
            
            if (!String.IsNullOrEmpty(Configuration.GetApiKeyWithPrefix("x-test_api_client_secret")))
            {
                localVarHeaderParams["x-test_api_client_secret"] = Configuration.GetApiKeyWithPrefix("x-test_api_client_secret");
            }
            

            // make the HTTP request
            IRestResponse localVarResponse = (IRestResponse) await Configuration.ApiClient.CallApiAsync(localVarPath, 
                Method.POST, localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarFileParams, 
                localVarPathParams, localVarHttpContentType);

            int localVarStatusCode = (int) localVarResponse.StatusCode;
 
            if (localVarStatusCode >= 400)
                throw new ApiException (localVarStatusCode, "Error calling PlaceOrder: " + localVarResponse.Content, localVarResponse.Content);
            else if (localVarStatusCode == 0)
                throw new ApiException (localVarStatusCode, "Error calling PlaceOrder: " + localVarResponse.ErrorMessage, localVarResponse.ErrorMessage);

            return new ApiResponse<Order>(localVarStatusCode,
                localVarResponse.Headers.ToDictionary(x => x.Name, x => x.Value.ToString()),
                (Order) Configuration.ApiClient.Deserialize(localVarResponse, typeof(Order)));
            
        }
        
        /// <summary>
        /// Find purchase order by ID For valid response try integer IDs with value &lt;= 5 or &gt; 10. Other values will generated exceptions
        /// </summary>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="orderId">ID of pet that needs to be fetched</param> 
        /// <returns>Order</returns>
        public Order GetOrderById (string orderId)
        {
             ApiResponse<Order> localVarResponse = GetOrderByIdWithHttpInfo(orderId);
             return localVarResponse.Data;
        }

        /// <summary>
        /// Find purchase order by ID For valid response try integer IDs with value &lt;= 5 or &gt; 10. Other values will generated exceptions
        /// </summary>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="orderId">ID of pet that needs to be fetched</param> 
        /// <returns>ApiResponse of Order</returns>
        public ApiResponse< Order > GetOrderByIdWithHttpInfo (string orderId)
        {
            
            // verify the required parameter 'orderId' is set
            if (orderId == null)
                throw new ApiException(400, "Missing required parameter 'orderId' when calling StoreApi->GetOrderById");
            
    
            var localVarPath = "/store/order/{orderId}";
    
            var localVarPathParams = new Dictionary<String, String>();
            var localVarQueryParams = new Dictionary<String, String>();
            var localVarHeaderParams = new Dictionary<String, String>(Configuration.DefaultHeader);
            var localVarFormParams = new Dictionary<String, String>();
            var localVarFileParams = new Dictionary<String, FileParameter>();
            Object localVarPostBody = null;

            // to determine the Content-Type header
            String[] localVarHttpContentTypes = new String[] {
                
            };
            String localVarHttpContentType = Configuration.ApiClient.SelectHeaderContentType(localVarHttpContentTypes);

            // to determine the Accept header
            String[] localVarHttpHeaderAccepts = new String[] {
                "application/json", "application/xml"
            };
            String localVarHttpHeaderAccept = Configuration.ApiClient.SelectHeaderAccept(localVarHttpHeaderAccepts);
            if (localVarHttpHeaderAccept != null)
                localVarHeaderParams.Add("Accept", localVarHttpHeaderAccept);

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            localVarPathParams.Add("format", "json");
            if (orderId != null) localVarPathParams.Add("orderId", Configuration.ApiClient.ParameterToString(orderId)); // path parameter
            
            
            
            
            

            // authentication (test_api_key_query) required
            
            if (!String.IsNullOrEmpty(Configuration.GetApiKeyWithPrefix("test_api_key_query")))
            {
                localVarQueryParams["test_api_key_query"] = Configuration.GetApiKeyWithPrefix("test_api_key_query");
            }
            // authentication (test_api_key_header) required
            
            if (!String.IsNullOrEmpty(Configuration.GetApiKeyWithPrefix("test_api_key_header")))
            {
                localVarHeaderParams["test_api_key_header"] = Configuration.GetApiKeyWithPrefix("test_api_key_header");
            }
            
    
            // make the HTTP request
            IRestResponse localVarResponse = (IRestResponse) Configuration.ApiClient.CallApi(localVarPath, 
                Method.GET, localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarFileParams,
                localVarPathParams, localVarHttpContentType);

            int localVarStatusCode = (int) localVarResponse.StatusCode;
    
            if (localVarStatusCode >= 400)
                throw new ApiException (localVarStatusCode, "Error calling GetOrderById: " + localVarResponse.Content, localVarResponse.Content);
            else if (localVarStatusCode == 0)
                throw new ApiException (localVarStatusCode, "Error calling GetOrderById: " + localVarResponse.ErrorMessage, localVarResponse.ErrorMessage);
    
            return new ApiResponse<Order>(localVarStatusCode,
                localVarResponse.Headers.ToDictionary(x => x.Name, x => x.Value.ToString()),
                (Order) Configuration.ApiClient.Deserialize(localVarResponse, typeof(Order)));
            
        }

        
        /// <summary>
        /// Find purchase order by ID For valid response try integer IDs with value &lt;= 5 or &gt; 10. Other values will generated exceptions
        /// </summary>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="orderId">ID of pet that needs to be fetched</param>
        /// <returns>Task of Order</returns>
        public async System.Threading.Tasks.Task<Order> GetOrderByIdAsync (string orderId)
        {
             ApiResponse<Order> localVarResponse = await GetOrderByIdAsyncWithHttpInfo(orderId);
             return localVarResponse.Data;

        }

        /// <summary>
        /// Find purchase order by ID For valid response try integer IDs with value &lt;= 5 or &gt; 10. Other values will generated exceptions
        /// </summary>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="orderId">ID of pet that needs to be fetched</param>
        /// <returns>Task of ApiResponse (Order)</returns>
        public async System.Threading.Tasks.Task<ApiResponse<Order>> GetOrderByIdAsyncWithHttpInfo (string orderId)
        {
            // verify the required parameter 'orderId' is set
            if (orderId == null) throw new ApiException(400, "Missing required parameter 'orderId' when calling GetOrderById");
            
    
            var localVarPath = "/store/order/{orderId}";
    
            var localVarPathParams = new Dictionary<String, String>();
            var localVarQueryParams = new Dictionary<String, String>();
            var localVarHeaderParams = new Dictionary<String, String>(Configuration.DefaultHeader);
            var localVarFormParams = new Dictionary<String, String>();
            var localVarFileParams = new Dictionary<String, FileParameter>();
            Object localVarPostBody = null;

            // to determine the Content-Type header
            String[] localVarHttpContentTypes = new String[] {
                
            };
            String localVarHttpContentType = Configuration.ApiClient.SelectHeaderContentType(localVarHttpContentTypes);

            // to determine the Accept header
            String[] localVarHttpHeaderAccepts = new String[] {
                "application/json", "application/xml"
            };
            String localVarHttpHeaderAccept = Configuration.ApiClient.SelectHeaderAccept(localVarHttpHeaderAccepts);
            if (localVarHttpHeaderAccept != null)
                localVarHeaderParams.Add("Accept", localVarHttpHeaderAccept);

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            localVarPathParams.Add("format", "json");
            if (orderId != null) localVarPathParams.Add("orderId", Configuration.ApiClient.ParameterToString(orderId)); // path parameter
            
            
            
            
            

            
            // authentication (test_api_key_query) required
            
            if (!String.IsNullOrEmpty(Configuration.GetApiKeyWithPrefix("test_api_key_query")))
            {
                localVarQueryParams["test_api_key_query"] = Configuration.GetApiKeyWithPrefix("test_api_key_query");
            }
            
            // authentication (test_api_key_header) required
            
            if (!String.IsNullOrEmpty(Configuration.GetApiKeyWithPrefix("test_api_key_header")))
            {
                localVarHeaderParams["test_api_key_header"] = Configuration.GetApiKeyWithPrefix("test_api_key_header");
            }
            

            // make the HTTP request
            IRestResponse localVarResponse = (IRestResponse) await Configuration.ApiClient.CallApiAsync(localVarPath, 
                Method.GET, localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarFileParams, 
                localVarPathParams, localVarHttpContentType);

            int localVarStatusCode = (int) localVarResponse.StatusCode;
 
            if (localVarStatusCode >= 400)
                throw new ApiException (localVarStatusCode, "Error calling GetOrderById: " + localVarResponse.Content, localVarResponse.Content);
            else if (localVarStatusCode == 0)
                throw new ApiException (localVarStatusCode, "Error calling GetOrderById: " + localVarResponse.ErrorMessage, localVarResponse.ErrorMessage);

            return new ApiResponse<Order>(localVarStatusCode,
                localVarResponse.Headers.ToDictionary(x => x.Name, x => x.Value.ToString()),
                (Order) Configuration.ApiClient.Deserialize(localVarResponse, typeof(Order)));
            
        }
        
        /// <summary>
        /// Delete purchase order by ID For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
        /// </summary>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="orderId">ID of the order that needs to be deleted</param> 
        /// <returns></returns>
        public void DeleteOrder (string orderId)
        {
             DeleteOrderWithHttpInfo(orderId);
        }

        /// <summary>
        /// Delete purchase order by ID For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
        /// </summary>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="orderId">ID of the order that needs to be deleted</param> 
        /// <returns>ApiResponse of Object(void)</returns>
        public ApiResponse<Object> DeleteOrderWithHttpInfo (string orderId)
        {
            
            // verify the required parameter 'orderId' is set
            if (orderId == null)
                throw new ApiException(400, "Missing required parameter 'orderId' when calling StoreApi->DeleteOrder");
            
    
            var localVarPath = "/store/order/{orderId}";
    
            var localVarPathParams = new Dictionary<String, String>();
            var localVarQueryParams = new Dictionary<String, String>();
            var localVarHeaderParams = new Dictionary<String, String>(Configuration.DefaultHeader);
            var localVarFormParams = new Dictionary<String, String>();
            var localVarFileParams = new Dictionary<String, FileParameter>();
            Object localVarPostBody = null;

            // to determine the Content-Type header
            String[] localVarHttpContentTypes = new String[] {
                
            };
            String localVarHttpContentType = Configuration.ApiClient.SelectHeaderContentType(localVarHttpContentTypes);

            // to determine the Accept header
            String[] localVarHttpHeaderAccepts = new String[] {
                "application/json", "application/xml"
            };
            String localVarHttpHeaderAccept = Configuration.ApiClient.SelectHeaderAccept(localVarHttpHeaderAccepts);
            if (localVarHttpHeaderAccept != null)
                localVarHeaderParams.Add("Accept", localVarHttpHeaderAccept);

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            localVarPathParams.Add("format", "json");
            if (orderId != null) localVarPathParams.Add("orderId", Configuration.ApiClient.ParameterToString(orderId)); // path parameter
            
            
            
            
            

            
    
            // make the HTTP request
            IRestResponse localVarResponse = (IRestResponse) Configuration.ApiClient.CallApi(localVarPath, 
                Method.DELETE, localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarFileParams,
                localVarPathParams, localVarHttpContentType);

            int localVarStatusCode = (int) localVarResponse.StatusCode;
    
            if (localVarStatusCode >= 400)
                throw new ApiException (localVarStatusCode, "Error calling DeleteOrder: " + localVarResponse.Content, localVarResponse.Content);
            else if (localVarStatusCode == 0)
                throw new ApiException (localVarStatusCode, "Error calling DeleteOrder: " + localVarResponse.ErrorMessage, localVarResponse.ErrorMessage);
    
            
            return new ApiResponse<Object>(localVarStatusCode,
                localVarResponse.Headers.ToDictionary(x => x.Name, x => x.Value.ToString()),
                null);
        }

        
        /// <summary>
        /// Delete purchase order by ID For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
        /// </summary>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="orderId">ID of the order that needs to be deleted</param>
        /// <returns>Task of void</returns>
        public async System.Threading.Tasks.Task DeleteOrderAsync (string orderId)
        {
             await DeleteOrderAsyncWithHttpInfo(orderId);

        }

        /// <summary>
        /// Delete purchase order by ID For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
        /// </summary>
        /// <exception cref="IO.Swagger.Client.ApiException">Thrown when fails to make API call</exception>
        /// <param name="orderId">ID of the order that needs to be deleted</param>
        /// <returns>Task of ApiResponse</returns>
        public async System.Threading.Tasks.Task<ApiResponse<Object>> DeleteOrderAsyncWithHttpInfo (string orderId)
        {
            // verify the required parameter 'orderId' is set
            if (orderId == null) throw new ApiException(400, "Missing required parameter 'orderId' when calling DeleteOrder");
            
    
            var localVarPath = "/store/order/{orderId}";
    
            var localVarPathParams = new Dictionary<String, String>();
            var localVarQueryParams = new Dictionary<String, String>();
            var localVarHeaderParams = new Dictionary<String, String>(Configuration.DefaultHeader);
            var localVarFormParams = new Dictionary<String, String>();
            var localVarFileParams = new Dictionary<String, FileParameter>();
            Object localVarPostBody = null;

            // to determine the Content-Type header
            String[] localVarHttpContentTypes = new String[] {
                
            };
            String localVarHttpContentType = Configuration.ApiClient.SelectHeaderContentType(localVarHttpContentTypes);

            // to determine the Accept header
            String[] localVarHttpHeaderAccepts = new String[] {
                "application/json", "application/xml"
            };
            String localVarHttpHeaderAccept = Configuration.ApiClient.SelectHeaderAccept(localVarHttpHeaderAccepts);
            if (localVarHttpHeaderAccept != null)
                localVarHeaderParams.Add("Accept", localVarHttpHeaderAccept);

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            localVarPathParams.Add("format", "json");
            if (orderId != null) localVarPathParams.Add("orderId", Configuration.ApiClient.ParameterToString(orderId)); // path parameter
            
            
            
            
            

            

            // make the HTTP request
            IRestResponse localVarResponse = (IRestResponse) await Configuration.ApiClient.CallApiAsync(localVarPath, 
                Method.DELETE, localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarFileParams, 
                localVarPathParams, localVarHttpContentType);

            int localVarStatusCode = (int) localVarResponse.StatusCode;
 
            if (localVarStatusCode >= 400)
                throw new ApiException (localVarStatusCode, "Error calling DeleteOrder: " + localVarResponse.Content, localVarResponse.Content);
            else if (localVarStatusCode == 0)
                throw new ApiException (localVarStatusCode, "Error calling DeleteOrder: " + localVarResponse.ErrorMessage, localVarResponse.ErrorMessage);

            
            return new ApiResponse<Object>(localVarStatusCode,
                localVarResponse.Headers.ToDictionary(x => x.Name, x => x.Value.ToString()),
                null);
        }
        
    }
    
}
