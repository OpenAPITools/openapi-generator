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
    public interface IPetApi
    {
        
        /// <summary>
        /// Update an existing pet
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <param name="body">Pet object that needs to be added to the store</param>
        /// <returns></returns>
        void UpdatePet (Pet body = null);
  
        /// <summary>
        /// Update an existing pet
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <param name="body">Pet object that needs to be added to the store</param>
        /// <returns>ApiResponse of Object(void)</returns>
        ApiResponse<Object> UpdatePetWithHttpInfo (Pet body = null);

        /// <summary>
        /// Update an existing pet
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <param name="body">Pet object that needs to be added to the store</param>
        /// <returns>Task of void</returns>
        System.Threading.Tasks.Task UpdatePetAsync (Pet body = null);

        /// <summary>
        /// Update an existing pet
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <param name="body">Pet object that needs to be added to the store</param>
        /// <returns>Task of ApiResponse</returns>
        System.Threading.Tasks.Task<ApiResponse<Object>> UpdatePetAsyncWithHttpInfo (Pet body = null);
        
        /// <summary>
        /// Add a new pet to the store
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <param name="body">Pet object that needs to be added to the store</param>
        /// <returns></returns>
        void AddPet (Pet body = null);
  
        /// <summary>
        /// Add a new pet to the store
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <param name="body">Pet object that needs to be added to the store</param>
        /// <returns>ApiResponse of Object(void)</returns>
        ApiResponse<Object> AddPetWithHttpInfo (Pet body = null);

        /// <summary>
        /// Add a new pet to the store
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <param name="body">Pet object that needs to be added to the store</param>
        /// <returns>Task of void</returns>
        System.Threading.Tasks.Task AddPetAsync (Pet body = null);

        /// <summary>
        /// Add a new pet to the store
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <param name="body">Pet object that needs to be added to the store</param>
        /// <returns>Task of ApiResponse</returns>
        System.Threading.Tasks.Task<ApiResponse<Object>> AddPetAsyncWithHttpInfo (Pet body = null);
        
        /// <summary>
        /// Finds Pets by status
        /// </summary>
        /// <remarks>
        /// Multiple status values can be provided with comma seperated strings
        /// </remarks>
        /// <param name="status">Status values that need to be considered for filter</param>
        /// <returns>List&lt;Pet&gt;</returns>
        List<Pet> FindPetsByStatus (List<string> status = null);
  
        /// <summary>
        /// Finds Pets by status
        /// </summary>
        /// <remarks>
        /// Multiple status values can be provided with comma seperated strings
        /// </remarks>
        /// <param name="status">Status values that need to be considered for filter</param>
        /// <returns>ApiResponse of List&lt;Pet&gt;</returns>
        ApiResponse<List<Pet>> FindPetsByStatusWithHttpInfo (List<string> status = null);

        /// <summary>
        /// Finds Pets by status
        /// </summary>
        /// <remarks>
        /// Multiple status values can be provided with comma seperated strings
        /// </remarks>
        /// <param name="status">Status values that need to be considered for filter</param>
        /// <returns>Task of List&lt;Pet&gt;</returns>
        System.Threading.Tasks.Task<List<Pet>> FindPetsByStatusAsync (List<string> status = null);

        /// <summary>
        /// Finds Pets by status
        /// </summary>
        /// <remarks>
        /// Multiple status values can be provided with comma seperated strings
        /// </remarks>
        /// <param name="status">Status values that need to be considered for filter</param>
        /// <returns>Task of ApiResponse (List&lt;Pet&gt;)</returns>
        System.Threading.Tasks.Task<ApiResponse<List<Pet>>> FindPetsByStatusAsyncWithHttpInfo (List<string> status = null);
        
        /// <summary>
        /// Finds Pets by tags
        /// </summary>
        /// <remarks>
        /// Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
        /// </remarks>
        /// <param name="tags">Tags to filter by</param>
        /// <returns>List&lt;Pet&gt;</returns>
        List<Pet> FindPetsByTags (List<string> tags = null);
  
        /// <summary>
        /// Finds Pets by tags
        /// </summary>
        /// <remarks>
        /// Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
        /// </remarks>
        /// <param name="tags">Tags to filter by</param>
        /// <returns>ApiResponse of List&lt;Pet&gt;</returns>
        ApiResponse<List<Pet>> FindPetsByTagsWithHttpInfo (List<string> tags = null);

        /// <summary>
        /// Finds Pets by tags
        /// </summary>
        /// <remarks>
        /// Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
        /// </remarks>
        /// <param name="tags">Tags to filter by</param>
        /// <returns>Task of List&lt;Pet&gt;</returns>
        System.Threading.Tasks.Task<List<Pet>> FindPetsByTagsAsync (List<string> tags = null);

        /// <summary>
        /// Finds Pets by tags
        /// </summary>
        /// <remarks>
        /// Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
        /// </remarks>
        /// <param name="tags">Tags to filter by</param>
        /// <returns>Task of ApiResponse (List&lt;Pet&gt;)</returns>
        System.Threading.Tasks.Task<ApiResponse<List<Pet>>> FindPetsByTagsAsyncWithHttpInfo (List<string> tags = null);
        
        /// <summary>
        /// Find pet by ID
        /// </summary>
        /// <remarks>
        /// Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
        /// </remarks>
        /// <param name="petId">ID of pet that needs to be fetched</param>
        /// <returns>Pet</returns>
        Pet GetPetById (long? petId);
  
        /// <summary>
        /// Find pet by ID
        /// </summary>
        /// <remarks>
        /// Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
        /// </remarks>
        /// <param name="petId">ID of pet that needs to be fetched</param>
        /// <returns>ApiResponse of Pet</returns>
        ApiResponse<Pet> GetPetByIdWithHttpInfo (long? petId);

        /// <summary>
        /// Find pet by ID
        /// </summary>
        /// <remarks>
        /// Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
        /// </remarks>
        /// <param name="petId">ID of pet that needs to be fetched</param>
        /// <returns>Task of Pet</returns>
        System.Threading.Tasks.Task<Pet> GetPetByIdAsync (long? petId);

        /// <summary>
        /// Find pet by ID
        /// </summary>
        /// <remarks>
        /// Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
        /// </remarks>
        /// <param name="petId">ID of pet that needs to be fetched</param>
        /// <returns>Task of ApiResponse (Pet)</returns>
        System.Threading.Tasks.Task<ApiResponse<Pet>> GetPetByIdAsyncWithHttpInfo (long? petId);
        
        /// <summary>
        /// Updates a pet in the store with form data
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <param name="petId">ID of pet that needs to be updated</param>
        /// <param name="name">Updated name of the pet</param>
        /// <param name="status">Updated status of the pet</param>
        /// <returns></returns>
        void UpdatePetWithForm (string petId, string name = null, string status = null);
  
        /// <summary>
        /// Updates a pet in the store with form data
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <param name="petId">ID of pet that needs to be updated</param>
        /// <param name="name">Updated name of the pet</param>
        /// <param name="status">Updated status of the pet</param>
        /// <returns>ApiResponse of Object(void)</returns>
        ApiResponse<Object> UpdatePetWithFormWithHttpInfo (string petId, string name = null, string status = null);

        /// <summary>
        /// Updates a pet in the store with form data
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <param name="petId">ID of pet that needs to be updated</param>
        /// <param name="name">Updated name of the pet</param>
        /// <param name="status">Updated status of the pet</param>
        /// <returns>Task of void</returns>
        System.Threading.Tasks.Task UpdatePetWithFormAsync (string petId, string name = null, string status = null);

        /// <summary>
        /// Updates a pet in the store with form data
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <param name="petId">ID of pet that needs to be updated</param>
        /// <param name="name">Updated name of the pet</param>
        /// <param name="status">Updated status of the pet</param>
        /// <returns>Task of ApiResponse</returns>
        System.Threading.Tasks.Task<ApiResponse<Object>> UpdatePetWithFormAsyncWithHttpInfo (string petId, string name = null, string status = null);
        
        /// <summary>
        /// Deletes a pet
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <param name="petId">Pet id to delete</param>
        /// <param name="apiKey"></param>
        /// <returns></returns>
        void DeletePet (long? petId, string apiKey = null);
  
        /// <summary>
        /// Deletes a pet
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <param name="petId">Pet id to delete</param>
        /// <param name="apiKey"></param>
        /// <returns>ApiResponse of Object(void)</returns>
        ApiResponse<Object> DeletePetWithHttpInfo (long? petId, string apiKey = null);

        /// <summary>
        /// Deletes a pet
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <param name="petId">Pet id to delete</param>
        /// <param name="apiKey"></param>
        /// <returns>Task of void</returns>
        System.Threading.Tasks.Task DeletePetAsync (long? petId, string apiKey = null);

        /// <summary>
        /// Deletes a pet
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <param name="petId">Pet id to delete</param>
        /// <param name="apiKey"></param>
        /// <returns>Task of ApiResponse</returns>
        System.Threading.Tasks.Task<ApiResponse<Object>> DeletePetAsyncWithHttpInfo (long? petId, string apiKey = null);
        
        /// <summary>
        /// uploads an image
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <param name="petId">ID of pet to update</param>
        /// <param name="additionalMetadata">Additional data to pass to server</param>
        /// <param name="file">file to upload</param>
        /// <returns></returns>
        void UploadFile (long? petId, string additionalMetadata = null, Stream file = null);
  
        /// <summary>
        /// uploads an image
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <param name="petId">ID of pet to update</param>
        /// <param name="additionalMetadata">Additional data to pass to server</param>
        /// <param name="file">file to upload</param>
        /// <returns>ApiResponse of Object(void)</returns>
        ApiResponse<Object> UploadFileWithHttpInfo (long? petId, string additionalMetadata = null, Stream file = null);

        /// <summary>
        /// uploads an image
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <param name="petId">ID of pet to update</param>
        /// <param name="additionalMetadata">Additional data to pass to server</param>
        /// <param name="file">file to upload</param>
        /// <returns>Task of void</returns>
        System.Threading.Tasks.Task UploadFileAsync (long? petId, string additionalMetadata = null, Stream file = null);

        /// <summary>
        /// uploads an image
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <param name="petId">ID of pet to update</param>
        /// <param name="additionalMetadata">Additional data to pass to server</param>
        /// <param name="file">file to upload</param>
        /// <returns>Task of ApiResponse</returns>
        System.Threading.Tasks.Task<ApiResponse<Object>> UploadFileAsyncWithHttpInfo (long? petId, string additionalMetadata = null, Stream file = null);
        
        /// <summary>
        /// Fake endpoint to test byte array return by &#39;Find pet by ID&#39;
        /// </summary>
        /// <remarks>
        /// Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
        /// </remarks>
        /// <param name="petId">ID of pet that needs to be fetched</param>
        /// <returns>byte[]</returns>
        byte[] GetPetByIdWithByteArray (long? petId);
  
        /// <summary>
        /// Fake endpoint to test byte array return by &#39;Find pet by ID&#39;
        /// </summary>
        /// <remarks>
        /// Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
        /// </remarks>
        /// <param name="petId">ID of pet that needs to be fetched</param>
        /// <returns>ApiResponse of byte[]</returns>
        ApiResponse<byte[]> GetPetByIdWithByteArrayWithHttpInfo (long? petId);

        /// <summary>
        /// Fake endpoint to test byte array return by &#39;Find pet by ID&#39;
        /// </summary>
        /// <remarks>
        /// Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
        /// </remarks>
        /// <param name="petId">ID of pet that needs to be fetched</param>
        /// <returns>Task of byte[]</returns>
        System.Threading.Tasks.Task<byte[]> GetPetByIdWithByteArrayAsync (long? petId);

        /// <summary>
        /// Fake endpoint to test byte array return by &#39;Find pet by ID&#39;
        /// </summary>
        /// <remarks>
        /// Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
        /// </remarks>
        /// <param name="petId">ID of pet that needs to be fetched</param>
        /// <returns>Task of ApiResponse (byte[])</returns>
        System.Threading.Tasks.Task<ApiResponse<byte[]>> GetPetByIdWithByteArrayAsyncWithHttpInfo (long? petId);
        
        /// <summary>
        /// Fake endpoint to test byte array in body parameter for adding a new pet to the store
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <param name="body">Pet object in the form of byte array</param>
        /// <returns></returns>
        void AddPetUsingByteArray (byte[] body = null);
  
        /// <summary>
        /// Fake endpoint to test byte array in body parameter for adding a new pet to the store
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <param name="body">Pet object in the form of byte array</param>
        /// <returns>ApiResponse of Object(void)</returns>
        ApiResponse<Object> AddPetUsingByteArrayWithHttpInfo (byte[] body = null);

        /// <summary>
        /// Fake endpoint to test byte array in body parameter for adding a new pet to the store
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <param name="body">Pet object in the form of byte array</param>
        /// <returns>Task of void</returns>
        System.Threading.Tasks.Task AddPetUsingByteArrayAsync (byte[] body = null);

        /// <summary>
        /// Fake endpoint to test byte array in body parameter for adding a new pet to the store
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <param name="body">Pet object in the form of byte array</param>
        /// <returns>Task of ApiResponse</returns>
        System.Threading.Tasks.Task<ApiResponse<Object>> AddPetUsingByteArrayAsyncWithHttpInfo (byte[] body = null);
        
    }
  
    /// <summary>
    /// Represents a collection of functions to interact with the API endpoints
    /// </summary>
    public class PetApi : IPetApi
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="PetApi"/> class.
        /// </summary>
        /// <returns></returns>
        public PetApi(String basePath)
        {
            this.Configuration = new Configuration(new ApiClient(basePath));
        }
    
        /// <summary>
        /// Initializes a new instance of the <see cref="PetApi"/> class
        /// using Configuration object
        /// </summary>
        /// <param name="configuration">An instance of Configuration</param>
        /// <returns></returns>
        public PetApi(Configuration configuration = null)
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
        /// Update an existing pet 
        /// </summary>
        /// <param name="body">Pet object that needs to be added to the store</param> 
        /// <returns></returns>
        public void UpdatePet (Pet body = null)
        {
             UpdatePetWithHttpInfo(body);
        }

        /// <summary>
        /// Update an existing pet 
        /// </summary>
        /// <param name="body">Pet object that needs to be added to the store</param> 
        /// <returns>ApiResponse of Object(void)</returns>
        public ApiResponse<Object> UpdatePetWithHttpInfo (Pet body = null)
        {
            
    
            var path_ = "/pet";
    
            var pathParams = new Dictionary<String, String>();
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>(Configuration.DefaultHeader);
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            Object postBody = null;

            // to determine the Content-Type header
            String[] httpContentTypes = new String[] {
                "application/json", "application/xml"
            };
            String httpContentType = Configuration.ApiClient.SelectHeaderContentType(httpContentTypes);

            // to determine the Accept header
            String[] httpHeaderAccepts = new String[] {
                "application/json", "application/xml"
            };
            String httpHeaderAccept = Configuration.ApiClient.SelectHeaderAccept(httpHeaderAccepts);
            if (httpHeaderAccept != null)
                headerParams.Add("Accept", httpHeaderAccept);

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            pathParams.Add("format", "json");
            
            
            
            
            if (body.GetType() != typeof(byte[]))
            {
                postBody = Configuration.ApiClient.Serialize(body); // http body (model) parameter
            }
            else
            {
                postBody = body; // byte array
            }

            // authentication (petstore_auth) required
            
            // oauth required
            if (!String.IsNullOrEmpty(Configuration.AccessToken))
            {
                headerParams["Authorization"] = "Bearer " + Configuration.AccessToken;
            }
            
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) Configuration.ApiClient.CallApi(path_, 
                Method.PUT, queryParams, postBody, headerParams, formParams, fileParams,
                pathParams, httpContentType);

            int statusCode = (int) response.StatusCode;
    
            if (statusCode >= 400)
                throw new ApiException (statusCode, "Error calling UpdatePet: " + response.Content, response.Content);
            else if (statusCode == 0)
                throw new ApiException (statusCode, "Error calling UpdatePet: " + response.ErrorMessage, response.ErrorMessage);
    
            
            return new ApiResponse<Object>(statusCode,
                response.Headers.ToDictionary(x => x.Name, x => x.Value.ToString()),
                null);
        }
    
        /// <summary>
        /// Update an existing pet 
        /// </summary>
        /// <param name="body">Pet object that needs to be added to the store</param>
        /// <returns>Task of void</returns>
        public async System.Threading.Tasks.Task UpdatePetAsync (Pet body = null)
        {
             await UpdatePetAsyncWithHttpInfo(body);

        }

        /// <summary>
        /// Update an existing pet 
        /// </summary>
        /// <param name="body">Pet object that needs to be added to the store</param>
        /// <returns>Task of ApiResponse</returns>
        public async System.Threading.Tasks.Task<ApiResponse<Object>> UpdatePetAsyncWithHttpInfo (Pet body = null)
        {
            
    
            var path_ = "/pet";
    
            var pathParams = new Dictionary<String, String>();
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            Object postBody = null;

            // to determine the Content-Type header
            String[] httpContentTypes = new String[] {
                "application/json", "application/xml"
            };
            String httpContentType = Configuration.ApiClient.SelectHeaderContentType(httpContentTypes);

            // to determine the Accept header
            String[] httpHeaderAccepts = new String[] {
                "application/json", "application/xml"
            };
            String httpHeaderAccept = Configuration.ApiClient.SelectHeaderAccept(httpHeaderAccepts);
            if (httpHeaderAccept != null)
                headerParams.Add("Accept", httpHeaderAccept);

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            pathParams.Add("format", "json");
            
            
            
            
            postBody = Configuration.ApiClient.Serialize(body); // http body (model) parameter
            

            
            // authentication (petstore_auth) required
            
            // oauth required
            if (!String.IsNullOrEmpty(Configuration.AccessToken))
            {
                headerParams["Authorization"] = "Bearer " + Configuration.AccessToken;
            }
            

            // make the HTTP request
            IRestResponse response = (IRestResponse) await Configuration.ApiClient.CallApiAsync(path_, 
                Method.PUT, queryParams, postBody, headerParams, formParams, fileParams, 
                pathParams, httpContentType);

            int statusCode = (int) response.StatusCode;
 
            if (statusCode >= 400)
                throw new ApiException (statusCode, "Error calling UpdatePet: " + response.Content, response.Content);
            else if (statusCode == 0)
                throw new ApiException (statusCode, "Error calling UpdatePet: " + response.ErrorMessage, response.ErrorMessage);

            
            return new ApiResponse<Object>(statusCode,
                response.Headers.ToDictionary(x => x.Name, x => x.Value.ToString()),
                null);
        }
        
        /// <summary>
        /// Add a new pet to the store 
        /// </summary>
        /// <param name="body">Pet object that needs to be added to the store</param> 
        /// <returns></returns>
        public void AddPet (Pet body = null)
        {
             AddPetWithHttpInfo(body);
        }

        /// <summary>
        /// Add a new pet to the store 
        /// </summary>
        /// <param name="body">Pet object that needs to be added to the store</param> 
        /// <returns>ApiResponse of Object(void)</returns>
        public ApiResponse<Object> AddPetWithHttpInfo (Pet body = null)
        {
            
    
            var path_ = "/pet";
    
            var pathParams = new Dictionary<String, String>();
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>(Configuration.DefaultHeader);
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            Object postBody = null;

            // to determine the Content-Type header
            String[] httpContentTypes = new String[] {
                "application/json", "application/xml"
            };
            String httpContentType = Configuration.ApiClient.SelectHeaderContentType(httpContentTypes);

            // to determine the Accept header
            String[] httpHeaderAccepts = new String[] {
                "application/json", "application/xml"
            };
            String httpHeaderAccept = Configuration.ApiClient.SelectHeaderAccept(httpHeaderAccepts);
            if (httpHeaderAccept != null)
                headerParams.Add("Accept", httpHeaderAccept);

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            pathParams.Add("format", "json");
            
            
            
            
            if (body.GetType() != typeof(byte[]))
            {
                postBody = Configuration.ApiClient.Serialize(body); // http body (model) parameter
            }
            else
            {
                postBody = body; // byte array
            }

            // authentication (petstore_auth) required
            
            // oauth required
            if (!String.IsNullOrEmpty(Configuration.AccessToken))
            {
                headerParams["Authorization"] = "Bearer " + Configuration.AccessToken;
            }
            
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) Configuration.ApiClient.CallApi(path_, 
                Method.POST, queryParams, postBody, headerParams, formParams, fileParams,
                pathParams, httpContentType);

            int statusCode = (int) response.StatusCode;
    
            if (statusCode >= 400)
                throw new ApiException (statusCode, "Error calling AddPet: " + response.Content, response.Content);
            else if (statusCode == 0)
                throw new ApiException (statusCode, "Error calling AddPet: " + response.ErrorMessage, response.ErrorMessage);
    
            
            return new ApiResponse<Object>(statusCode,
                response.Headers.ToDictionary(x => x.Name, x => x.Value.ToString()),
                null);
        }
    
        /// <summary>
        /// Add a new pet to the store 
        /// </summary>
        /// <param name="body">Pet object that needs to be added to the store</param>
        /// <returns>Task of void</returns>
        public async System.Threading.Tasks.Task AddPetAsync (Pet body = null)
        {
             await AddPetAsyncWithHttpInfo(body);

        }

        /// <summary>
        /// Add a new pet to the store 
        /// </summary>
        /// <param name="body">Pet object that needs to be added to the store</param>
        /// <returns>Task of ApiResponse</returns>
        public async System.Threading.Tasks.Task<ApiResponse<Object>> AddPetAsyncWithHttpInfo (Pet body = null)
        {
            
    
            var path_ = "/pet";
    
            var pathParams = new Dictionary<String, String>();
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            Object postBody = null;

            // to determine the Content-Type header
            String[] httpContentTypes = new String[] {
                "application/json", "application/xml"
            };
            String httpContentType = Configuration.ApiClient.SelectHeaderContentType(httpContentTypes);

            // to determine the Accept header
            String[] httpHeaderAccepts = new String[] {
                "application/json", "application/xml"
            };
            String httpHeaderAccept = Configuration.ApiClient.SelectHeaderAccept(httpHeaderAccepts);
            if (httpHeaderAccept != null)
                headerParams.Add("Accept", httpHeaderAccept);

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            pathParams.Add("format", "json");
            
            
            
            
            postBody = Configuration.ApiClient.Serialize(body); // http body (model) parameter
            

            
            // authentication (petstore_auth) required
            
            // oauth required
            if (!String.IsNullOrEmpty(Configuration.AccessToken))
            {
                headerParams["Authorization"] = "Bearer " + Configuration.AccessToken;
            }
            

            // make the HTTP request
            IRestResponse response = (IRestResponse) await Configuration.ApiClient.CallApiAsync(path_, 
                Method.POST, queryParams, postBody, headerParams, formParams, fileParams, 
                pathParams, httpContentType);

            int statusCode = (int) response.StatusCode;
 
            if (statusCode >= 400)
                throw new ApiException (statusCode, "Error calling AddPet: " + response.Content, response.Content);
            else if (statusCode == 0)
                throw new ApiException (statusCode, "Error calling AddPet: " + response.ErrorMessage, response.ErrorMessage);

            
            return new ApiResponse<Object>(statusCode,
                response.Headers.ToDictionary(x => x.Name, x => x.Value.ToString()),
                null);
        }
        
        /// <summary>
        /// Finds Pets by status Multiple status values can be provided with comma seperated strings
        /// </summary>
        /// <param name="status">Status values that need to be considered for filter</param> 
        /// <returns>List&lt;Pet&gt;</returns>
        public List<Pet> FindPetsByStatus (List<string> status = null)
        {
             ApiResponse<List<Pet>> response = FindPetsByStatusWithHttpInfo(status);
             return response.Data;
        }

        /// <summary>
        /// Finds Pets by status Multiple status values can be provided with comma seperated strings
        /// </summary>
        /// <param name="status">Status values that need to be considered for filter</param> 
        /// <returns>ApiResponse of List&lt;Pet&gt;</returns>
        public ApiResponse< List<Pet> > FindPetsByStatusWithHttpInfo (List<string> status = null)
        {
            
    
            var path_ = "/pet/findByStatus";
    
            var pathParams = new Dictionary<String, String>();
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>(Configuration.DefaultHeader);
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            Object postBody = null;

            // to determine the Content-Type header
            String[] httpContentTypes = new String[] {
                
            };
            String httpContentType = Configuration.ApiClient.SelectHeaderContentType(httpContentTypes);

            // to determine the Accept header
            String[] httpHeaderAccepts = new String[] {
                "application/json", "application/xml"
            };
            String httpHeaderAccept = Configuration.ApiClient.SelectHeaderAccept(httpHeaderAccepts);
            if (httpHeaderAccept != null)
                headerParams.Add("Accept", httpHeaderAccept);

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            pathParams.Add("format", "json");
            
            if (status != null) queryParams.Add("status", Configuration.ApiClient.ParameterToString(status)); // query parameter
            
            
            
            

            // authentication (petstore_auth) required
            
            // oauth required
            if (!String.IsNullOrEmpty(Configuration.AccessToken))
            {
                headerParams["Authorization"] = "Bearer " + Configuration.AccessToken;
            }
            
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) Configuration.ApiClient.CallApi(path_, 
                Method.GET, queryParams, postBody, headerParams, formParams, fileParams,
                pathParams, httpContentType);

            int statusCode = (int) response.StatusCode;
    
            if (statusCode >= 400)
                throw new ApiException (statusCode, "Error calling FindPetsByStatus: " + response.Content, response.Content);
            else if (statusCode == 0)
                throw new ApiException (statusCode, "Error calling FindPetsByStatus: " + response.ErrorMessage, response.ErrorMessage);
    
            return new ApiResponse<List<Pet>>(statusCode,
                response.Headers.ToDictionary(x => x.Name, x => x.Value.ToString()),
                (List<Pet>) Configuration.ApiClient.Deserialize(response, typeof(List<Pet>)));
            
        }
    
        /// <summary>
        /// Finds Pets by status Multiple status values can be provided with comma seperated strings
        /// </summary>
        /// <param name="status">Status values that need to be considered for filter</param>
        /// <returns>Task of List&lt;Pet&gt;</returns>
        public async System.Threading.Tasks.Task<List<Pet>> FindPetsByStatusAsync (List<string> status = null)
        {
             ApiResponse<List<Pet>> response = await FindPetsByStatusAsyncWithHttpInfo(status);
             return response.Data;

        }

        /// <summary>
        /// Finds Pets by status Multiple status values can be provided with comma seperated strings
        /// </summary>
        /// <param name="status">Status values that need to be considered for filter</param>
        /// <returns>Task of ApiResponse (List&lt;Pet&gt;)</returns>
        public async System.Threading.Tasks.Task<ApiResponse<List<Pet>>> FindPetsByStatusAsyncWithHttpInfo (List<string> status = null)
        {
            
    
            var path_ = "/pet/findByStatus";
    
            var pathParams = new Dictionary<String, String>();
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            Object postBody = null;

            // to determine the Content-Type header
            String[] httpContentTypes = new String[] {
                
            };
            String httpContentType = Configuration.ApiClient.SelectHeaderContentType(httpContentTypes);

            // to determine the Accept header
            String[] httpHeaderAccepts = new String[] {
                "application/json", "application/xml"
            };
            String httpHeaderAccept = Configuration.ApiClient.SelectHeaderAccept(httpHeaderAccepts);
            if (httpHeaderAccept != null)
                headerParams.Add("Accept", httpHeaderAccept);

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            pathParams.Add("format", "json");
            
            if (status != null) queryParams.Add("status", Configuration.ApiClient.ParameterToString(status)); // query parameter
            
            
            
            

            
            // authentication (petstore_auth) required
            
            // oauth required
            if (!String.IsNullOrEmpty(Configuration.AccessToken))
            {
                headerParams["Authorization"] = "Bearer " + Configuration.AccessToken;
            }
            

            // make the HTTP request
            IRestResponse response = (IRestResponse) await Configuration.ApiClient.CallApiAsync(path_, 
                Method.GET, queryParams, postBody, headerParams, formParams, fileParams, 
                pathParams, httpContentType);

            int statusCode = (int) response.StatusCode;
 
            if (statusCode >= 400)
                throw new ApiException (statusCode, "Error calling FindPetsByStatus: " + response.Content, response.Content);
            else if (statusCode == 0)
                throw new ApiException (statusCode, "Error calling FindPetsByStatus: " + response.ErrorMessage, response.ErrorMessage);

            return new ApiResponse<List<Pet>>(statusCode,
                response.Headers.ToDictionary(x => x.Name, x => x.Value.ToString()),
                (List<Pet>) Configuration.ApiClient.Deserialize(response, typeof(List<Pet>)));
            
        }
        
        /// <summary>
        /// Finds Pets by tags Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
        /// </summary>
        /// <param name="tags">Tags to filter by</param> 
        /// <returns>List&lt;Pet&gt;</returns>
        public List<Pet> FindPetsByTags (List<string> tags = null)
        {
             ApiResponse<List<Pet>> response = FindPetsByTagsWithHttpInfo(tags);
             return response.Data;
        }

        /// <summary>
        /// Finds Pets by tags Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
        /// </summary>
        /// <param name="tags">Tags to filter by</param> 
        /// <returns>ApiResponse of List&lt;Pet&gt;</returns>
        public ApiResponse< List<Pet> > FindPetsByTagsWithHttpInfo (List<string> tags = null)
        {
            
    
            var path_ = "/pet/findByTags";
    
            var pathParams = new Dictionary<String, String>();
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>(Configuration.DefaultHeader);
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            Object postBody = null;

            // to determine the Content-Type header
            String[] httpContentTypes = new String[] {
                
            };
            String httpContentType = Configuration.ApiClient.SelectHeaderContentType(httpContentTypes);

            // to determine the Accept header
            String[] httpHeaderAccepts = new String[] {
                "application/json", "application/xml"
            };
            String httpHeaderAccept = Configuration.ApiClient.SelectHeaderAccept(httpHeaderAccepts);
            if (httpHeaderAccept != null)
                headerParams.Add("Accept", httpHeaderAccept);

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            pathParams.Add("format", "json");
            
            if (tags != null) queryParams.Add("tags", Configuration.ApiClient.ParameterToString(tags)); // query parameter
            
            
            
            

            // authentication (petstore_auth) required
            
            // oauth required
            if (!String.IsNullOrEmpty(Configuration.AccessToken))
            {
                headerParams["Authorization"] = "Bearer " + Configuration.AccessToken;
            }
            
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) Configuration.ApiClient.CallApi(path_, 
                Method.GET, queryParams, postBody, headerParams, formParams, fileParams,
                pathParams, httpContentType);

            int statusCode = (int) response.StatusCode;
    
            if (statusCode >= 400)
                throw new ApiException (statusCode, "Error calling FindPetsByTags: " + response.Content, response.Content);
            else if (statusCode == 0)
                throw new ApiException (statusCode, "Error calling FindPetsByTags: " + response.ErrorMessage, response.ErrorMessage);
    
            return new ApiResponse<List<Pet>>(statusCode,
                response.Headers.ToDictionary(x => x.Name, x => x.Value.ToString()),
                (List<Pet>) Configuration.ApiClient.Deserialize(response, typeof(List<Pet>)));
            
        }
    
        /// <summary>
        /// Finds Pets by tags Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
        /// </summary>
        /// <param name="tags">Tags to filter by</param>
        /// <returns>Task of List&lt;Pet&gt;</returns>
        public async System.Threading.Tasks.Task<List<Pet>> FindPetsByTagsAsync (List<string> tags = null)
        {
             ApiResponse<List<Pet>> response = await FindPetsByTagsAsyncWithHttpInfo(tags);
             return response.Data;

        }

        /// <summary>
        /// Finds Pets by tags Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
        /// </summary>
        /// <param name="tags">Tags to filter by</param>
        /// <returns>Task of ApiResponse (List&lt;Pet&gt;)</returns>
        public async System.Threading.Tasks.Task<ApiResponse<List<Pet>>> FindPetsByTagsAsyncWithHttpInfo (List<string> tags = null)
        {
            
    
            var path_ = "/pet/findByTags";
    
            var pathParams = new Dictionary<String, String>();
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            Object postBody = null;

            // to determine the Content-Type header
            String[] httpContentTypes = new String[] {
                
            };
            String httpContentType = Configuration.ApiClient.SelectHeaderContentType(httpContentTypes);

            // to determine the Accept header
            String[] httpHeaderAccepts = new String[] {
                "application/json", "application/xml"
            };
            String httpHeaderAccept = Configuration.ApiClient.SelectHeaderAccept(httpHeaderAccepts);
            if (httpHeaderAccept != null)
                headerParams.Add("Accept", httpHeaderAccept);

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            pathParams.Add("format", "json");
            
            if (tags != null) queryParams.Add("tags", Configuration.ApiClient.ParameterToString(tags)); // query parameter
            
            
            
            

            
            // authentication (petstore_auth) required
            
            // oauth required
            if (!String.IsNullOrEmpty(Configuration.AccessToken))
            {
                headerParams["Authorization"] = "Bearer " + Configuration.AccessToken;
            }
            

            // make the HTTP request
            IRestResponse response = (IRestResponse) await Configuration.ApiClient.CallApiAsync(path_, 
                Method.GET, queryParams, postBody, headerParams, formParams, fileParams, 
                pathParams, httpContentType);

            int statusCode = (int) response.StatusCode;
 
            if (statusCode >= 400)
                throw new ApiException (statusCode, "Error calling FindPetsByTags: " + response.Content, response.Content);
            else if (statusCode == 0)
                throw new ApiException (statusCode, "Error calling FindPetsByTags: " + response.ErrorMessage, response.ErrorMessage);

            return new ApiResponse<List<Pet>>(statusCode,
                response.Headers.ToDictionary(x => x.Name, x => x.Value.ToString()),
                (List<Pet>) Configuration.ApiClient.Deserialize(response, typeof(List<Pet>)));
            
        }
        
        /// <summary>
        /// Find pet by ID Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
        /// </summary>
        /// <param name="petId">ID of pet that needs to be fetched</param> 
        /// <returns>Pet</returns>
        public Pet GetPetById (long? petId)
        {
             ApiResponse<Pet> response = GetPetByIdWithHttpInfo(petId);
             return response.Data;
        }

        /// <summary>
        /// Find pet by ID Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
        /// </summary>
        /// <param name="petId">ID of pet that needs to be fetched</param> 
        /// <returns>ApiResponse of Pet</returns>
        public ApiResponse< Pet > GetPetByIdWithHttpInfo (long? petId)
        {
            
            // verify the required parameter 'petId' is set
            if (petId == null)
                throw new ApiException(400, "Missing required parameter 'petId' when calling PetApi->GetPetById");
            
    
            var path_ = "/pet/{petId}";
    
            var pathParams = new Dictionary<String, String>();
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>(Configuration.DefaultHeader);
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            Object postBody = null;

            // to determine the Content-Type header
            String[] httpContentTypes = new String[] {
                
            };
            String httpContentType = Configuration.ApiClient.SelectHeaderContentType(httpContentTypes);

            // to determine the Accept header
            String[] httpHeaderAccepts = new String[] {
                "application/json", "application/xml"
            };
            String httpHeaderAccept = Configuration.ApiClient.SelectHeaderAccept(httpHeaderAccepts);
            if (httpHeaderAccept != null)
                headerParams.Add("Accept", httpHeaderAccept);

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            pathParams.Add("format", "json");
            if (petId != null) pathParams.Add("petId", Configuration.ApiClient.ParameterToString(petId)); // path parameter
            
            
            
            
            

            // authentication (api_key) required
            
            var apiKeyValue = Configuration.GetApiKeyWithPrefix("api_key");
            if (!String.IsNullOrEmpty(apiKeyValue))
            {
                headerParams["api_key"] = apiKeyValue;
            }
            
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) Configuration.ApiClient.CallApi(path_, 
                Method.GET, queryParams, postBody, headerParams, formParams, fileParams,
                pathParams, httpContentType);

            int statusCode = (int) response.StatusCode;
    
            if (statusCode >= 400)
                throw new ApiException (statusCode, "Error calling GetPetById: " + response.Content, response.Content);
            else if (statusCode == 0)
                throw new ApiException (statusCode, "Error calling GetPetById: " + response.ErrorMessage, response.ErrorMessage);
    
            return new ApiResponse<Pet>(statusCode,
                response.Headers.ToDictionary(x => x.Name, x => x.Value.ToString()),
                (Pet) Configuration.ApiClient.Deserialize(response, typeof(Pet)));
            
        }
    
        /// <summary>
        /// Find pet by ID Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
        /// </summary>
        /// <param name="petId">ID of pet that needs to be fetched</param>
        /// <returns>Task of Pet</returns>
        public async System.Threading.Tasks.Task<Pet> GetPetByIdAsync (long? petId)
        {
             ApiResponse<Pet> response = await GetPetByIdAsyncWithHttpInfo(petId);
             return response.Data;

        }

        /// <summary>
        /// Find pet by ID Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
        /// </summary>
        /// <param name="petId">ID of pet that needs to be fetched</param>
        /// <returns>Task of ApiResponse (Pet)</returns>
        public async System.Threading.Tasks.Task<ApiResponse<Pet>> GetPetByIdAsyncWithHttpInfo (long? petId)
        {
            // verify the required parameter 'petId' is set
            if (petId == null) throw new ApiException(400, "Missing required parameter 'petId' when calling GetPetById");
            
    
            var path_ = "/pet/{petId}";
    
            var pathParams = new Dictionary<String, String>();
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            Object postBody = null;

            // to determine the Content-Type header
            String[] httpContentTypes = new String[] {
                
            };
            String httpContentType = Configuration.ApiClient.SelectHeaderContentType(httpContentTypes);

            // to determine the Accept header
            String[] httpHeaderAccepts = new String[] {
                "application/json", "application/xml"
            };
            String httpHeaderAccept = Configuration.ApiClient.SelectHeaderAccept(httpHeaderAccepts);
            if (httpHeaderAccept != null)
                headerParams.Add("Accept", httpHeaderAccept);

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            pathParams.Add("format", "json");
            if (petId != null) pathParams.Add("petId", Configuration.ApiClient.ParameterToString(petId)); // path parameter
            
            
            
            
            

            
            // authentication (api_key) required
            
            var apiKeyValue = Configuration.GetApiKeyWithPrefix("api_key");
            if (!String.IsNullOrEmpty(apiKeyValue))
            {
                headerParams["api_key"] = apiKeyValue;
            }
            

            // make the HTTP request
            IRestResponse response = (IRestResponse) await Configuration.ApiClient.CallApiAsync(path_, 
                Method.GET, queryParams, postBody, headerParams, formParams, fileParams, 
                pathParams, httpContentType);

            int statusCode = (int) response.StatusCode;
 
            if (statusCode >= 400)
                throw new ApiException (statusCode, "Error calling GetPetById: " + response.Content, response.Content);
            else if (statusCode == 0)
                throw new ApiException (statusCode, "Error calling GetPetById: " + response.ErrorMessage, response.ErrorMessage);

            return new ApiResponse<Pet>(statusCode,
                response.Headers.ToDictionary(x => x.Name, x => x.Value.ToString()),
                (Pet) Configuration.ApiClient.Deserialize(response, typeof(Pet)));
            
        }
        
        /// <summary>
        /// Updates a pet in the store with form data 
        /// </summary>
        /// <param name="petId">ID of pet that needs to be updated</param> 
        /// <param name="name">Updated name of the pet</param> 
        /// <param name="status">Updated status of the pet</param> 
        /// <returns></returns>
        public void UpdatePetWithForm (string petId, string name = null, string status = null)
        {
             UpdatePetWithFormWithHttpInfo(petId, name, status);
        }

        /// <summary>
        /// Updates a pet in the store with form data 
        /// </summary>
        /// <param name="petId">ID of pet that needs to be updated</param> 
        /// <param name="name">Updated name of the pet</param> 
        /// <param name="status">Updated status of the pet</param> 
        /// <returns>ApiResponse of Object(void)</returns>
        public ApiResponse<Object> UpdatePetWithFormWithHttpInfo (string petId, string name = null, string status = null)
        {
            
            // verify the required parameter 'petId' is set
            if (petId == null)
                throw new ApiException(400, "Missing required parameter 'petId' when calling PetApi->UpdatePetWithForm");
            
    
            var path_ = "/pet/{petId}";
    
            var pathParams = new Dictionary<String, String>();
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>(Configuration.DefaultHeader);
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            Object postBody = null;

            // to determine the Content-Type header
            String[] httpContentTypes = new String[] {
                "application/x-www-form-urlencoded"
            };
            String httpContentType = Configuration.ApiClient.SelectHeaderContentType(httpContentTypes);

            // to determine the Accept header
            String[] httpHeaderAccepts = new String[] {
                "application/json", "application/xml"
            };
            String httpHeaderAccept = Configuration.ApiClient.SelectHeaderAccept(httpHeaderAccepts);
            if (httpHeaderAccept != null)
                headerParams.Add("Accept", httpHeaderAccept);

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            pathParams.Add("format", "json");
            if (petId != null) pathParams.Add("petId", Configuration.ApiClient.ParameterToString(petId)); // path parameter
            
            
            
            if (name != null) formParams.Add("name", Configuration.ApiClient.ParameterToString(name)); // form parameter
            if (status != null) formParams.Add("status", Configuration.ApiClient.ParameterToString(status)); // form parameter
            
            

            // authentication (petstore_auth) required
            
            // oauth required
            if (!String.IsNullOrEmpty(Configuration.AccessToken))
            {
                headerParams["Authorization"] = "Bearer " + Configuration.AccessToken;
            }
            
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) Configuration.ApiClient.CallApi(path_, 
                Method.POST, queryParams, postBody, headerParams, formParams, fileParams,
                pathParams, httpContentType);

            int statusCode = (int) response.StatusCode;
    
            if (statusCode >= 400)
                throw new ApiException (statusCode, "Error calling UpdatePetWithForm: " + response.Content, response.Content);
            else if (statusCode == 0)
                throw new ApiException (statusCode, "Error calling UpdatePetWithForm: " + response.ErrorMessage, response.ErrorMessage);
    
            
            return new ApiResponse<Object>(statusCode,
                response.Headers.ToDictionary(x => x.Name, x => x.Value.ToString()),
                null);
        }
    
        /// <summary>
        /// Updates a pet in the store with form data 
        /// </summary>
        /// <param name="petId">ID of pet that needs to be updated</param>
        /// <param name="name">Updated name of the pet</param>
        /// <param name="status">Updated status of the pet</param>
        /// <returns>Task of void</returns>
        public async System.Threading.Tasks.Task UpdatePetWithFormAsync (string petId, string name = null, string status = null)
        {
             await UpdatePetWithFormAsyncWithHttpInfo(petId, name, status);

        }

        /// <summary>
        /// Updates a pet in the store with form data 
        /// </summary>
        /// <param name="petId">ID of pet that needs to be updated</param>
        /// <param name="name">Updated name of the pet</param>
        /// <param name="status">Updated status of the pet</param>
        /// <returns>Task of ApiResponse</returns>
        public async System.Threading.Tasks.Task<ApiResponse<Object>> UpdatePetWithFormAsyncWithHttpInfo (string petId, string name = null, string status = null)
        {
            // verify the required parameter 'petId' is set
            if (petId == null) throw new ApiException(400, "Missing required parameter 'petId' when calling UpdatePetWithForm");
            
    
            var path_ = "/pet/{petId}";
    
            var pathParams = new Dictionary<String, String>();
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            Object postBody = null;

            // to determine the Content-Type header
            String[] httpContentTypes = new String[] {
                "application/x-www-form-urlencoded"
            };
            String httpContentType = Configuration.ApiClient.SelectHeaderContentType(httpContentTypes);

            // to determine the Accept header
            String[] httpHeaderAccepts = new String[] {
                "application/json", "application/xml"
            };
            String httpHeaderAccept = Configuration.ApiClient.SelectHeaderAccept(httpHeaderAccepts);
            if (httpHeaderAccept != null)
                headerParams.Add("Accept", httpHeaderAccept);

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            pathParams.Add("format", "json");
            if (petId != null) pathParams.Add("petId", Configuration.ApiClient.ParameterToString(petId)); // path parameter
            
            
            
            if (name != null) formParams.Add("name", Configuration.ApiClient.ParameterToString(name)); // form parameter
            if (status != null) formParams.Add("status", Configuration.ApiClient.ParameterToString(status)); // form parameter
            
            

            
            // authentication (petstore_auth) required
            
            // oauth required
            if (!String.IsNullOrEmpty(Configuration.AccessToken))
            {
                headerParams["Authorization"] = "Bearer " + Configuration.AccessToken;
            }
            

            // make the HTTP request
            IRestResponse response = (IRestResponse) await Configuration.ApiClient.CallApiAsync(path_, 
                Method.POST, queryParams, postBody, headerParams, formParams, fileParams, 
                pathParams, httpContentType);

            int statusCode = (int) response.StatusCode;
 
            if (statusCode >= 400)
                throw new ApiException (statusCode, "Error calling UpdatePetWithForm: " + response.Content, response.Content);
            else if (statusCode == 0)
                throw new ApiException (statusCode, "Error calling UpdatePetWithForm: " + response.ErrorMessage, response.ErrorMessage);

            
            return new ApiResponse<Object>(statusCode,
                response.Headers.ToDictionary(x => x.Name, x => x.Value.ToString()),
                null);
        }
        
        /// <summary>
        /// Deletes a pet 
        /// </summary>
        /// <param name="petId">Pet id to delete</param> 
        /// <param name="apiKey"></param> 
        /// <returns></returns>
        public void DeletePet (long? petId, string apiKey = null)
        {
             DeletePetWithHttpInfo(petId, apiKey);
        }

        /// <summary>
        /// Deletes a pet 
        /// </summary>
        /// <param name="petId">Pet id to delete</param> 
        /// <param name="apiKey"></param> 
        /// <returns>ApiResponse of Object(void)</returns>
        public ApiResponse<Object> DeletePetWithHttpInfo (long? petId, string apiKey = null)
        {
            
            // verify the required parameter 'petId' is set
            if (petId == null)
                throw new ApiException(400, "Missing required parameter 'petId' when calling PetApi->DeletePet");
            
    
            var path_ = "/pet/{petId}";
    
            var pathParams = new Dictionary<String, String>();
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>(Configuration.DefaultHeader);
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            Object postBody = null;

            // to determine the Content-Type header
            String[] httpContentTypes = new String[] {
                
            };
            String httpContentType = Configuration.ApiClient.SelectHeaderContentType(httpContentTypes);

            // to determine the Accept header
            String[] httpHeaderAccepts = new String[] {
                "application/json", "application/xml"
            };
            String httpHeaderAccept = Configuration.ApiClient.SelectHeaderAccept(httpHeaderAccepts);
            if (httpHeaderAccept != null)
                headerParams.Add("Accept", httpHeaderAccept);

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            pathParams.Add("format", "json");
            if (petId != null) pathParams.Add("petId", Configuration.ApiClient.ParameterToString(petId)); // path parameter
            
            
            if (apiKey != null) headerParams.Add("api_key", Configuration.ApiClient.ParameterToString(apiKey)); // header parameter
            
            
            

            // authentication (petstore_auth) required
            
            // oauth required
            if (!String.IsNullOrEmpty(Configuration.AccessToken))
            {
                headerParams["Authorization"] = "Bearer " + Configuration.AccessToken;
            }
            
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) Configuration.ApiClient.CallApi(path_, 
                Method.DELETE, queryParams, postBody, headerParams, formParams, fileParams,
                pathParams, httpContentType);

            int statusCode = (int) response.StatusCode;
    
            if (statusCode >= 400)
                throw new ApiException (statusCode, "Error calling DeletePet: " + response.Content, response.Content);
            else if (statusCode == 0)
                throw new ApiException (statusCode, "Error calling DeletePet: " + response.ErrorMessage, response.ErrorMessage);
    
            
            return new ApiResponse<Object>(statusCode,
                response.Headers.ToDictionary(x => x.Name, x => x.Value.ToString()),
                null);
        }
    
        /// <summary>
        /// Deletes a pet 
        /// </summary>
        /// <param name="petId">Pet id to delete</param>
        /// <param name="apiKey"></param>
        /// <returns>Task of void</returns>
        public async System.Threading.Tasks.Task DeletePetAsync (long? petId, string apiKey = null)
        {
             await DeletePetAsyncWithHttpInfo(petId, apiKey);

        }

        /// <summary>
        /// Deletes a pet 
        /// </summary>
        /// <param name="petId">Pet id to delete</param>
        /// <param name="apiKey"></param>
        /// <returns>Task of ApiResponse</returns>
        public async System.Threading.Tasks.Task<ApiResponse<Object>> DeletePetAsyncWithHttpInfo (long? petId, string apiKey = null)
        {
            // verify the required parameter 'petId' is set
            if (petId == null) throw new ApiException(400, "Missing required parameter 'petId' when calling DeletePet");
            
    
            var path_ = "/pet/{petId}";
    
            var pathParams = new Dictionary<String, String>();
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            Object postBody = null;

            // to determine the Content-Type header
            String[] httpContentTypes = new String[] {
                
            };
            String httpContentType = Configuration.ApiClient.SelectHeaderContentType(httpContentTypes);

            // to determine the Accept header
            String[] httpHeaderAccepts = new String[] {
                "application/json", "application/xml"
            };
            String httpHeaderAccept = Configuration.ApiClient.SelectHeaderAccept(httpHeaderAccepts);
            if (httpHeaderAccept != null)
                headerParams.Add("Accept", httpHeaderAccept);

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            pathParams.Add("format", "json");
            if (petId != null) pathParams.Add("petId", Configuration.ApiClient.ParameterToString(petId)); // path parameter
            
            
            if (apiKey != null) headerParams.Add("api_key", Configuration.ApiClient.ParameterToString(apiKey)); // header parameter
            
            
            

            
            // authentication (petstore_auth) required
            
            // oauth required
            if (!String.IsNullOrEmpty(Configuration.AccessToken))
            {
                headerParams["Authorization"] = "Bearer " + Configuration.AccessToken;
            }
            

            // make the HTTP request
            IRestResponse response = (IRestResponse) await Configuration.ApiClient.CallApiAsync(path_, 
                Method.DELETE, queryParams, postBody, headerParams, formParams, fileParams, 
                pathParams, httpContentType);

            int statusCode = (int) response.StatusCode;
 
            if (statusCode >= 400)
                throw new ApiException (statusCode, "Error calling DeletePet: " + response.Content, response.Content);
            else if (statusCode == 0)
                throw new ApiException (statusCode, "Error calling DeletePet: " + response.ErrorMessage, response.ErrorMessage);

            
            return new ApiResponse<Object>(statusCode,
                response.Headers.ToDictionary(x => x.Name, x => x.Value.ToString()),
                null);
        }
        
        /// <summary>
        /// uploads an image 
        /// </summary>
        /// <param name="petId">ID of pet to update</param> 
        /// <param name="additionalMetadata">Additional data to pass to server</param> 
        /// <param name="file">file to upload</param> 
        /// <returns></returns>
        public void UploadFile (long? petId, string additionalMetadata = null, Stream file = null)
        {
             UploadFileWithHttpInfo(petId, additionalMetadata, file);
        }

        /// <summary>
        /// uploads an image 
        /// </summary>
        /// <param name="petId">ID of pet to update</param> 
        /// <param name="additionalMetadata">Additional data to pass to server</param> 
        /// <param name="file">file to upload</param> 
        /// <returns>ApiResponse of Object(void)</returns>
        public ApiResponse<Object> UploadFileWithHttpInfo (long? petId, string additionalMetadata = null, Stream file = null)
        {
            
            // verify the required parameter 'petId' is set
            if (petId == null)
                throw new ApiException(400, "Missing required parameter 'petId' when calling PetApi->UploadFile");
            
    
            var path_ = "/pet/{petId}/uploadImage";
    
            var pathParams = new Dictionary<String, String>();
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>(Configuration.DefaultHeader);
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            Object postBody = null;

            // to determine the Content-Type header
            String[] httpContentTypes = new String[] {
                "multipart/form-data"
            };
            String httpContentType = Configuration.ApiClient.SelectHeaderContentType(httpContentTypes);

            // to determine the Accept header
            String[] httpHeaderAccepts = new String[] {
                "application/json", "application/xml"
            };
            String httpHeaderAccept = Configuration.ApiClient.SelectHeaderAccept(httpHeaderAccepts);
            if (httpHeaderAccept != null)
                headerParams.Add("Accept", httpHeaderAccept);

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            pathParams.Add("format", "json");
            if (petId != null) pathParams.Add("petId", Configuration.ApiClient.ParameterToString(petId)); // path parameter
            
            
            
            if (additionalMetadata != null) formParams.Add("additionalMetadata", Configuration.ApiClient.ParameterToString(additionalMetadata)); // form parameter
            if (file != null) fileParams.Add("file", Configuration.ApiClient.ParameterToFile("file", file));
            
            

            // authentication (petstore_auth) required
            
            // oauth required
            if (!String.IsNullOrEmpty(Configuration.AccessToken))
            {
                headerParams["Authorization"] = "Bearer " + Configuration.AccessToken;
            }
            
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) Configuration.ApiClient.CallApi(path_, 
                Method.POST, queryParams, postBody, headerParams, formParams, fileParams,
                pathParams, httpContentType);

            int statusCode = (int) response.StatusCode;
    
            if (statusCode >= 400)
                throw new ApiException (statusCode, "Error calling UploadFile: " + response.Content, response.Content);
            else if (statusCode == 0)
                throw new ApiException (statusCode, "Error calling UploadFile: " + response.ErrorMessage, response.ErrorMessage);
    
            
            return new ApiResponse<Object>(statusCode,
                response.Headers.ToDictionary(x => x.Name, x => x.Value.ToString()),
                null);
        }
    
        /// <summary>
        /// uploads an image 
        /// </summary>
        /// <param name="petId">ID of pet to update</param>
        /// <param name="additionalMetadata">Additional data to pass to server</param>
        /// <param name="file">file to upload</param>
        /// <returns>Task of void</returns>
        public async System.Threading.Tasks.Task UploadFileAsync (long? petId, string additionalMetadata = null, Stream file = null)
        {
             await UploadFileAsyncWithHttpInfo(petId, additionalMetadata, file);

        }

        /// <summary>
        /// uploads an image 
        /// </summary>
        /// <param name="petId">ID of pet to update</param>
        /// <param name="additionalMetadata">Additional data to pass to server</param>
        /// <param name="file">file to upload</param>
        /// <returns>Task of ApiResponse</returns>
        public async System.Threading.Tasks.Task<ApiResponse<Object>> UploadFileAsyncWithHttpInfo (long? petId, string additionalMetadata = null, Stream file = null)
        {
            // verify the required parameter 'petId' is set
            if (petId == null) throw new ApiException(400, "Missing required parameter 'petId' when calling UploadFile");
            
    
            var path_ = "/pet/{petId}/uploadImage";
    
            var pathParams = new Dictionary<String, String>();
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            Object postBody = null;

            // to determine the Content-Type header
            String[] httpContentTypes = new String[] {
                "multipart/form-data"
            };
            String httpContentType = Configuration.ApiClient.SelectHeaderContentType(httpContentTypes);

            // to determine the Accept header
            String[] httpHeaderAccepts = new String[] {
                "application/json", "application/xml"
            };
            String httpHeaderAccept = Configuration.ApiClient.SelectHeaderAccept(httpHeaderAccepts);
            if (httpHeaderAccept != null)
                headerParams.Add("Accept", httpHeaderAccept);

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            pathParams.Add("format", "json");
            if (petId != null) pathParams.Add("petId", Configuration.ApiClient.ParameterToString(petId)); // path parameter
            
            
            
            if (additionalMetadata != null) formParams.Add("additionalMetadata", Configuration.ApiClient.ParameterToString(additionalMetadata)); // form parameter
            if (file != null) fileParams.Add("file", Configuration.ApiClient.ParameterToFile("file", file));
            
            

            
            // authentication (petstore_auth) required
            
            // oauth required
            if (!String.IsNullOrEmpty(Configuration.AccessToken))
            {
                headerParams["Authorization"] = "Bearer " + Configuration.AccessToken;
            }
            

            // make the HTTP request
            IRestResponse response = (IRestResponse) await Configuration.ApiClient.CallApiAsync(path_, 
                Method.POST, queryParams, postBody, headerParams, formParams, fileParams, 
                pathParams, httpContentType);

            int statusCode = (int) response.StatusCode;
 
            if (statusCode >= 400)
                throw new ApiException (statusCode, "Error calling UploadFile: " + response.Content, response.Content);
            else if (statusCode == 0)
                throw new ApiException (statusCode, "Error calling UploadFile: " + response.ErrorMessage, response.ErrorMessage);

            
            return new ApiResponse<Object>(statusCode,
                response.Headers.ToDictionary(x => x.Name, x => x.Value.ToString()),
                null);
        }
        
        /// <summary>
        /// Fake endpoint to test byte array return by &#39;Find pet by ID&#39; Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
        /// </summary>
        /// <param name="petId">ID of pet that needs to be fetched</param> 
        /// <returns>byte[]</returns>
        public byte[] GetPetByIdWithByteArray (long? petId)
        {
             ApiResponse<byte[]> response = GetPetByIdWithByteArrayWithHttpInfo(petId);
             return response.Data;
        }

        /// <summary>
        /// Fake endpoint to test byte array return by &#39;Find pet by ID&#39; Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
        /// </summary>
        /// <param name="petId">ID of pet that needs to be fetched</param> 
        /// <returns>ApiResponse of byte[]</returns>
        public ApiResponse< byte[] > GetPetByIdWithByteArrayWithHttpInfo (long? petId)
        {
            
            // verify the required parameter 'petId' is set
            if (petId == null)
                throw new ApiException(400, "Missing required parameter 'petId' when calling PetApi->GetPetByIdWithByteArray");
            
    
            var path_ = "/pet/{petId}?testing_byte_array=true";
    
            var pathParams = new Dictionary<String, String>();
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>(Configuration.DefaultHeader);
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            Object postBody = null;

            // to determine the Content-Type header
            String[] httpContentTypes = new String[] {
                
            };
            String httpContentType = Configuration.ApiClient.SelectHeaderContentType(httpContentTypes);

            // to determine the Accept header
            String[] httpHeaderAccepts = new String[] {
                "application/json", "application/xml"
            };
            String httpHeaderAccept = Configuration.ApiClient.SelectHeaderAccept(httpHeaderAccepts);
            if (httpHeaderAccept != null)
                headerParams.Add("Accept", httpHeaderAccept);

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            pathParams.Add("format", "json");
            if (petId != null) pathParams.Add("petId", Configuration.ApiClient.ParameterToString(petId)); // path parameter
            
            
            
            
            

            // authentication (api_key) required
            
            var apiKeyValue = Configuration.GetApiKeyWithPrefix("api_key");
            if (!String.IsNullOrEmpty(apiKeyValue))
            {
                headerParams["api_key"] = apiKeyValue;
            }
            
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) Configuration.ApiClient.CallApi(path_, 
                Method.GET, queryParams, postBody, headerParams, formParams, fileParams,
                pathParams, httpContentType);

            int statusCode = (int) response.StatusCode;
    
            if (statusCode >= 400)
                throw new ApiException (statusCode, "Error calling GetPetByIdWithByteArray: " + response.Content, response.Content);
            else if (statusCode == 0)
                throw new ApiException (statusCode, "Error calling GetPetByIdWithByteArray: " + response.ErrorMessage, response.ErrorMessage);
    
            return new ApiResponse<byte[]>(statusCode,
                response.Headers.ToDictionary(x => x.Name, x => x.Value.ToString()),
                (byte[]) Configuration.ApiClient.Deserialize(response, typeof(byte[])));
            
        }
    
        /// <summary>
        /// Fake endpoint to test byte array return by &#39;Find pet by ID&#39; Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
        /// </summary>
        /// <param name="petId">ID of pet that needs to be fetched</param>
        /// <returns>Task of byte[]</returns>
        public async System.Threading.Tasks.Task<byte[]> GetPetByIdWithByteArrayAsync (long? petId)
        {
             ApiResponse<byte[]> response = await GetPetByIdWithByteArrayAsyncWithHttpInfo(petId);
             return response.Data;

        }

        /// <summary>
        /// Fake endpoint to test byte array return by &#39;Find pet by ID&#39; Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
        /// </summary>
        /// <param name="petId">ID of pet that needs to be fetched</param>
        /// <returns>Task of ApiResponse (byte[])</returns>
        public async System.Threading.Tasks.Task<ApiResponse<byte[]>> GetPetByIdWithByteArrayAsyncWithHttpInfo (long? petId)
        {
            // verify the required parameter 'petId' is set
            if (petId == null) throw new ApiException(400, "Missing required parameter 'petId' when calling GetPetByIdWithByteArray");
            
    
            var path_ = "/pet/{petId}?testing_byte_array=true";
    
            var pathParams = new Dictionary<String, String>();
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            Object postBody = null;

            // to determine the Content-Type header
            String[] httpContentTypes = new String[] {
                
            };
            String httpContentType = Configuration.ApiClient.SelectHeaderContentType(httpContentTypes);

            // to determine the Accept header
            String[] httpHeaderAccepts = new String[] {
                "application/json", "application/xml"
            };
            String httpHeaderAccept = Configuration.ApiClient.SelectHeaderAccept(httpHeaderAccepts);
            if (httpHeaderAccept != null)
                headerParams.Add("Accept", httpHeaderAccept);

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            pathParams.Add("format", "json");
            if (petId != null) pathParams.Add("petId", Configuration.ApiClient.ParameterToString(petId)); // path parameter
            
            
            
            
            

            
            // authentication (api_key) required
            
            var apiKeyValue = Configuration.GetApiKeyWithPrefix("api_key");
            if (!String.IsNullOrEmpty(apiKeyValue))
            {
                headerParams["api_key"] = apiKeyValue;
            }
            

            // make the HTTP request
            IRestResponse response = (IRestResponse) await Configuration.ApiClient.CallApiAsync(path_, 
                Method.GET, queryParams, postBody, headerParams, formParams, fileParams, 
                pathParams, httpContentType);

            int statusCode = (int) response.StatusCode;
 
            if (statusCode >= 400)
                throw new ApiException (statusCode, "Error calling GetPetByIdWithByteArray: " + response.Content, response.Content);
            else if (statusCode == 0)
                throw new ApiException (statusCode, "Error calling GetPetByIdWithByteArray: " + response.ErrorMessage, response.ErrorMessage);

            return new ApiResponse<byte[]>(statusCode,
                response.Headers.ToDictionary(x => x.Name, x => x.Value.ToString()),
                (byte[]) Configuration.ApiClient.Deserialize(response, typeof(byte[])));
            
        }
        
        /// <summary>
        /// Fake endpoint to test byte array in body parameter for adding a new pet to the store 
        /// </summary>
        /// <param name="body">Pet object in the form of byte array</param> 
        /// <returns></returns>
        public void AddPetUsingByteArray (byte[] body = null)
        {
             AddPetUsingByteArrayWithHttpInfo(body);
        }

        /// <summary>
        /// Fake endpoint to test byte array in body parameter for adding a new pet to the store 
        /// </summary>
        /// <param name="body">Pet object in the form of byte array</param> 
        /// <returns>ApiResponse of Object(void)</returns>
        public ApiResponse<Object> AddPetUsingByteArrayWithHttpInfo (byte[] body = null)
        {
            
    
            var path_ = "/pet?testing_byte_array=true";
    
            var pathParams = new Dictionary<String, String>();
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>(Configuration.DefaultHeader);
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            Object postBody = null;

            // to determine the Content-Type header
            String[] httpContentTypes = new String[] {
                "application/json", "application/xml"
            };
            String httpContentType = Configuration.ApiClient.SelectHeaderContentType(httpContentTypes);

            // to determine the Accept header
            String[] httpHeaderAccepts = new String[] {
                "application/json", "application/xml"
            };
            String httpHeaderAccept = Configuration.ApiClient.SelectHeaderAccept(httpHeaderAccepts);
            if (httpHeaderAccept != null)
                headerParams.Add("Accept", httpHeaderAccept);

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            pathParams.Add("format", "json");
            
            
            
            
            if (body.GetType() != typeof(byte[]))
            {
                postBody = Configuration.ApiClient.Serialize(body); // http body (model) parameter
            }
            else
            {
                postBody = body; // byte array
            }

            // authentication (petstore_auth) required
            
            // oauth required
            if (!String.IsNullOrEmpty(Configuration.AccessToken))
            {
                headerParams["Authorization"] = "Bearer " + Configuration.AccessToken;
            }
            
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) Configuration.ApiClient.CallApi(path_, 
                Method.POST, queryParams, postBody, headerParams, formParams, fileParams,
                pathParams, httpContentType);

            int statusCode = (int) response.StatusCode;
    
            if (statusCode >= 400)
                throw new ApiException (statusCode, "Error calling AddPetUsingByteArray: " + response.Content, response.Content);
            else if (statusCode == 0)
                throw new ApiException (statusCode, "Error calling AddPetUsingByteArray: " + response.ErrorMessage, response.ErrorMessage);
    
            
            return new ApiResponse<Object>(statusCode,
                response.Headers.ToDictionary(x => x.Name, x => x.Value.ToString()),
                null);
        }
    
        /// <summary>
        /// Fake endpoint to test byte array in body parameter for adding a new pet to the store 
        /// </summary>
        /// <param name="body">Pet object in the form of byte array</param>
        /// <returns>Task of void</returns>
        public async System.Threading.Tasks.Task AddPetUsingByteArrayAsync (byte[] body = null)
        {
             await AddPetUsingByteArrayAsyncWithHttpInfo(body);

        }

        /// <summary>
        /// Fake endpoint to test byte array in body parameter for adding a new pet to the store 
        /// </summary>
        /// <param name="body">Pet object in the form of byte array</param>
        /// <returns>Task of ApiResponse</returns>
        public async System.Threading.Tasks.Task<ApiResponse<Object>> AddPetUsingByteArrayAsyncWithHttpInfo (byte[] body = null)
        {
            
    
            var path_ = "/pet?testing_byte_array=true";
    
            var pathParams = new Dictionary<String, String>();
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            Object postBody = null;

            // to determine the Content-Type header
            String[] httpContentTypes = new String[] {
                "application/json", "application/xml"
            };
            String httpContentType = Configuration.ApiClient.SelectHeaderContentType(httpContentTypes);

            // to determine the Accept header
            String[] httpHeaderAccepts = new String[] {
                "application/json", "application/xml"
            };
            String httpHeaderAccept = Configuration.ApiClient.SelectHeaderAccept(httpHeaderAccepts);
            if (httpHeaderAccept != null)
                headerParams.Add("Accept", httpHeaderAccept);

            // set "format" to json by default
            // e.g. /pet/{petId}.{format} becomes /pet/{petId}.json
            pathParams.Add("format", "json");
            
            
            
            
            postBody = Configuration.ApiClient.Serialize(body); // http body (model) parameter
            

            
            // authentication (petstore_auth) required
            
            // oauth required
            if (!String.IsNullOrEmpty(Configuration.AccessToken))
            {
                headerParams["Authorization"] = "Bearer " + Configuration.AccessToken;
            }
            

            // make the HTTP request
            IRestResponse response = (IRestResponse) await Configuration.ApiClient.CallApiAsync(path_, 
                Method.POST, queryParams, postBody, headerParams, formParams, fileParams, 
                pathParams, httpContentType);

            int statusCode = (int) response.StatusCode;
 
            if (statusCode >= 400)
                throw new ApiException (statusCode, "Error calling AddPetUsingByteArray: " + response.Content, response.Content);
            else if (statusCode == 0)
                throw new ApiException (statusCode, "Error calling AddPetUsingByteArray: " + response.ErrorMessage, response.ErrorMessage);

            
            return new ApiResponse<Object>(statusCode,
                response.Headers.ToDictionary(x => x.Name, x => x.Value.ToString()),
                null);
        }
        
    }
    
}
