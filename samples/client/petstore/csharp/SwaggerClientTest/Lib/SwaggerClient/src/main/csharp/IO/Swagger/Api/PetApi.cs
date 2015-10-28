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
        void UpdatePet (Pet body);
  
        /// <summary>
        /// Update an existing pet
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <param name="body">Pet object that needs to be added to the store</param>
        /// <returns></returns>
        System.Threading.Tasks.Task UpdatePetAsync (Pet body);
        
        /// <summary>
        /// Add a new pet to the store
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <param name="body">Pet object that needs to be added to the store</param>
        /// <returns></returns>
        void AddPet (Pet body);
  
        /// <summary>
        /// Add a new pet to the store
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <param name="body">Pet object that needs to be added to the store</param>
        /// <returns></returns>
        System.Threading.Tasks.Task AddPetAsync (Pet body);
        
        /// <summary>
        /// Finds Pets by status
        /// </summary>
        /// <remarks>
        /// Multiple status values can be provided with comma seperated strings
        /// </remarks>
        /// <param name="status">Status values that need to be considered for filter</param>
        /// <returns></returns>
        List<Pet> FindPetsByStatus (List<string> status);
  
        /// <summary>
        /// Finds Pets by status
        /// </summary>
        /// <remarks>
        /// Multiple status values can be provided with comma seperated strings
        /// </remarks>
        /// <param name="status">Status values that need to be considered for filter</param>
        /// <returns></returns>
        System.Threading.Tasks.Task<List<Pet>> FindPetsByStatusAsync (List<string> status);
        
        /// <summary>
        /// Finds Pets by tags
        /// </summary>
        /// <remarks>
        /// Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
        /// </remarks>
        /// <param name="tags">Tags to filter by</param>
        /// <returns></returns>
        List<Pet> FindPetsByTags (List<string> tags);
  
        /// <summary>
        /// Finds Pets by tags
        /// </summary>
        /// <remarks>
        /// Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
        /// </remarks>
        /// <param name="tags">Tags to filter by</param>
        /// <returns></returns>
        System.Threading.Tasks.Task<List<Pet>> FindPetsByTagsAsync (List<string> tags);
        
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
        /// <returns>Pet</returns>
        System.Threading.Tasks.Task<Pet> GetPetByIdAsync (long? petId);
        
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
        void UpdatePetWithForm (string petId, string name, string status);
  
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
        System.Threading.Tasks.Task UpdatePetWithFormAsync (string petId, string name, string status);
        
        /// <summary>
        /// Deletes a pet
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <param name="petId">Pet id to delete</param>
        /// <param name="apiKey"></param>
        /// <returns></returns>
        void DeletePet (long? petId, string apiKey);
  
        /// <summary>
        /// Deletes a pet
        /// </summary>
        /// <remarks>
        /// 
        /// </remarks>
        /// <param name="petId">Pet id to delete</param>
        /// <param name="apiKey"></param>
        /// <returns></returns>
        System.Threading.Tasks.Task DeletePetAsync (long? petId, string apiKey);
        
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
        void UploadFile (long? petId, string additionalMetadata, Stream file);
  
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
        System.Threading.Tasks.Task UploadFileAsync (long? petId, string additionalMetadata, Stream file);
        
    }
  
    /// <summary>
    /// Represents a collection of functions to interact with the API endpoints
    /// </summary>
    public class PetApi : IPetApi
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="PetApi"/> class.
        /// </summary>
        /// <param name="apiClient"> an instance of ApiClient (optional)</param>
        /// <returns></returns>
        public PetApi(ApiClient apiClient = null)
        {
            if (apiClient == null) // use the default one in Configuration
                this.ApiClient = Configuration.DefaultApiClient; 
            else
                this.ApiClient = apiClient;
        }
    
        /// <summary>
        /// Initializes a new instance of the <see cref="PetApi"/> class.
        /// </summary>
        /// <returns></returns>
        public PetApi(String basePath)
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
        /// Update an existing pet 
        /// </summary>
        /// <param name="body">Pet object that needs to be added to the store</param> 
        /// <returns></returns>            
        public void UpdatePet (Pet body)
        {
            
    
            var path_ = "/pet";
    
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
            String[] authSettings = new String[] { "petstore_auth" };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) ApiClient.CallApi(path_, Method.PUT, queryParams, postBody, headerParams, formParams, fileParams, pathParams, authSettings);
    
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling UpdatePet: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException ((int)response.StatusCode, "Error calling UpdatePet: " + response.ErrorMessage, response.ErrorMessage);
    
            return;
        }
    
        /// <summary>
        /// Update an existing pet 
        /// </summary>
        /// <param name="body">Pet object that needs to be added to the store</param>
        /// <returns></returns>
        public async System.Threading.Tasks.Task UpdatePetAsync (Pet body)
        {
            
    
            var path_ = "/pet";
    
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
            String[] authSettings = new String[] { "petstore_auth" };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) await ApiClient.CallApiAsync(path_, Method.PUT, queryParams, postBody, headerParams, formParams, fileParams, pathParams, authSettings);
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling UpdatePet: " + response.Content, response.Content);

            
            return;
        }
        
        /// <summary>
        /// Add a new pet to the store 
        /// </summary>
        /// <param name="body">Pet object that needs to be added to the store</param> 
        /// <returns></returns>            
        public void AddPet (Pet body)
        {
            
    
            var path_ = "/pet";
    
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
            String[] authSettings = new String[] { "petstore_auth" };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) ApiClient.CallApi(path_, Method.POST, queryParams, postBody, headerParams, formParams, fileParams, pathParams, authSettings);
    
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling AddPet: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException ((int)response.StatusCode, "Error calling AddPet: " + response.ErrorMessage, response.ErrorMessage);
    
            return;
        }
    
        /// <summary>
        /// Add a new pet to the store 
        /// </summary>
        /// <param name="body">Pet object that needs to be added to the store</param>
        /// <returns></returns>
        public async System.Threading.Tasks.Task AddPetAsync (Pet body)
        {
            
    
            var path_ = "/pet";
    
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
            String[] authSettings = new String[] { "petstore_auth" };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) await ApiClient.CallApiAsync(path_, Method.POST, queryParams, postBody, headerParams, formParams, fileParams, pathParams, authSettings);
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling AddPet: " + response.Content, response.Content);

            
            return;
        }
        
        /// <summary>
        /// Finds Pets by status Multiple status values can be provided with comma seperated strings
        /// </summary>
        /// <param name="status">Status values that need to be considered for filter</param> 
        /// <returns></returns>            
        public List<Pet> FindPetsByStatus (List<string> status)
        {
            
    
            var path_ = "/pet/findByStatus";
    
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
            
            if (status != null) queryParams.Add("status", ApiClient.ParameterToString(status)); // query parameter
            
            
            
            
    
            // authentication setting, if any
            String[] authSettings = new String[] { "petstore_auth" };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) ApiClient.CallApi(path_, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, pathParams, authSettings);
    
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling FindPetsByStatus: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException ((int)response.StatusCode, "Error calling FindPetsByStatus: " + response.ErrorMessage, response.ErrorMessage);
    
            return (List<Pet>) ApiClient.Deserialize(response, typeof(List<Pet>));
        }
    
        /// <summary>
        /// Finds Pets by status Multiple status values can be provided with comma seperated strings
        /// </summary>
        /// <param name="status">Status values that need to be considered for filter</param>
        /// <returns></returns>
        public async System.Threading.Tasks.Task<List<Pet>> FindPetsByStatusAsync (List<string> status)
        {
            
    
            var path_ = "/pet/findByStatus";
    
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
            
            if (status != null) queryParams.Add("status", ApiClient.ParameterToString(status)); // query parameter
            
            
            
            
    
            // authentication setting, if any
            String[] authSettings = new String[] { "petstore_auth" };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) await ApiClient.CallApiAsync(path_, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, pathParams, authSettings);
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling FindPetsByStatus: " + response.Content, response.Content);

            return (List<Pet>) ApiClient.Deserialize(response, typeof(List<Pet>));
        }
        
        /// <summary>
        /// Finds Pets by tags Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
        /// </summary>
        /// <param name="tags">Tags to filter by</param> 
        /// <returns></returns>            
        public List<Pet> FindPetsByTags (List<string> tags)
        {
            
    
            var path_ = "/pet/findByTags";
    
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
            
            if (tags != null) queryParams.Add("tags", ApiClient.ParameterToString(tags)); // query parameter
            
            
            
            
    
            // authentication setting, if any
            String[] authSettings = new String[] { "petstore_auth" };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) ApiClient.CallApi(path_, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, pathParams, authSettings);
    
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling FindPetsByTags: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException ((int)response.StatusCode, "Error calling FindPetsByTags: " + response.ErrorMessage, response.ErrorMessage);
    
            return (List<Pet>) ApiClient.Deserialize(response, typeof(List<Pet>));
        }
    
        /// <summary>
        /// Finds Pets by tags Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
        /// </summary>
        /// <param name="tags">Tags to filter by</param>
        /// <returns></returns>
        public async System.Threading.Tasks.Task<List<Pet>> FindPetsByTagsAsync (List<string> tags)
        {
            
    
            var path_ = "/pet/findByTags";
    
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
            
            if (tags != null) queryParams.Add("tags", ApiClient.ParameterToString(tags)); // query parameter
            
            
            
            
    
            // authentication setting, if any
            String[] authSettings = new String[] { "petstore_auth" };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) await ApiClient.CallApiAsync(path_, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, pathParams, authSettings);
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling FindPetsByTags: " + response.Content, response.Content);

            return (List<Pet>) ApiClient.Deserialize(response, typeof(List<Pet>));
        }
        
        /// <summary>
        /// Find pet by ID Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
        /// </summary>
        /// <param name="petId">ID of pet that needs to be fetched</param> 
        /// <returns>Pet</returns>            
        public Pet GetPetById (long? petId)
        {
            
            // verify the required parameter 'petId' is set
            if (petId == null) throw new ApiException(400, "Missing required parameter 'petId' when calling GetPetById");
            
    
            var path_ = "/pet/{petId}";
    
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
            if (petId != null) pathParams.Add("petId", ApiClient.ParameterToString(petId)); // path parameter
            
            
            
            
            
    
            // authentication setting, if any
            String[] authSettings = new String[] { "api_key" };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) ApiClient.CallApi(path_, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, pathParams, authSettings);
    
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling GetPetById: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException ((int)response.StatusCode, "Error calling GetPetById: " + response.ErrorMessage, response.ErrorMessage);
    
            return (Pet) ApiClient.Deserialize(response, typeof(Pet));
        }
    
        /// <summary>
        /// Find pet by ID Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
        /// </summary>
        /// <param name="petId">ID of pet that needs to be fetched</param>
        /// <returns>Pet</returns>
        public async System.Threading.Tasks.Task<Pet> GetPetByIdAsync (long? petId)
        {
            // verify the required parameter 'petId' is set
            if (petId == null) throw new ApiException(400, "Missing required parameter 'petId' when calling GetPetById");
            
    
            var path_ = "/pet/{petId}";
    
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
            if (petId != null) pathParams.Add("petId", ApiClient.ParameterToString(petId)); // path parameter
            
            
            
            
            
    
            // authentication setting, if any
            String[] authSettings = new String[] { "api_key" };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) await ApiClient.CallApiAsync(path_, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, pathParams, authSettings);
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling GetPetById: " + response.Content, response.Content);

            return (Pet) ApiClient.Deserialize(response, typeof(Pet));
        }
        
        /// <summary>
        /// Updates a pet in the store with form data 
        /// </summary>
        /// <param name="petId">ID of pet that needs to be updated</param> 
        /// <param name="name">Updated name of the pet</param> 
        /// <param name="status">Updated status of the pet</param> 
        /// <returns></returns>            
        public void UpdatePetWithForm (string petId, string name, string status)
        {
            
            // verify the required parameter 'petId' is set
            if (petId == null) throw new ApiException(400, "Missing required parameter 'petId' when calling UpdatePetWithForm");
            
    
            var path_ = "/pet/{petId}";
    
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
            if (petId != null) pathParams.Add("petId", ApiClient.ParameterToString(petId)); // path parameter
            
            
            
            if (name != null) formParams.Add("name", ApiClient.ParameterToString(name)); // form parameter
            if (status != null) formParams.Add("status", ApiClient.ParameterToString(status)); // form parameter
            
            
    
            // authentication setting, if any
            String[] authSettings = new String[] { "petstore_auth" };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) ApiClient.CallApi(path_, Method.POST, queryParams, postBody, headerParams, formParams, fileParams, pathParams, authSettings);
    
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling UpdatePetWithForm: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException ((int)response.StatusCode, "Error calling UpdatePetWithForm: " + response.ErrorMessage, response.ErrorMessage);
    
            return;
        }
    
        /// <summary>
        /// Updates a pet in the store with form data 
        /// </summary>
        /// <param name="petId">ID of pet that needs to be updated</param>
        /// <param name="name">Updated name of the pet</param>
        /// <param name="status">Updated status of the pet</param>
        /// <returns></returns>
        public async System.Threading.Tasks.Task UpdatePetWithFormAsync (string petId, string name, string status)
        {
            // verify the required parameter 'petId' is set
            if (petId == null) throw new ApiException(400, "Missing required parameter 'petId' when calling UpdatePetWithForm");
            
    
            var path_ = "/pet/{petId}";
    
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
            if (petId != null) pathParams.Add("petId", ApiClient.ParameterToString(petId)); // path parameter
            
            
            
            if (name != null) formParams.Add("name", ApiClient.ParameterToString(name)); // form parameter
            if (status != null) formParams.Add("status", ApiClient.ParameterToString(status)); // form parameter
            
            
    
            // authentication setting, if any
            String[] authSettings = new String[] { "petstore_auth" };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) await ApiClient.CallApiAsync(path_, Method.POST, queryParams, postBody, headerParams, formParams, fileParams, pathParams, authSettings);
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling UpdatePetWithForm: " + response.Content, response.Content);

            
            return;
        }
        
        /// <summary>
        /// Deletes a pet 
        /// </summary>
        /// <param name="petId">Pet id to delete</param> 
        /// <param name="apiKey"></param> 
        /// <returns></returns>            
        public void DeletePet (long? petId, string apiKey)
        {
            
            // verify the required parameter 'petId' is set
            if (petId == null) throw new ApiException(400, "Missing required parameter 'petId' when calling DeletePet");
            
    
            var path_ = "/pet/{petId}";
    
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
            if (petId != null) pathParams.Add("petId", ApiClient.ParameterToString(petId)); // path parameter
            
            
            if (apiKey != null) headerParams.Add("api_key", ApiClient.ParameterToString(apiKey)); // header parameter
            
            
            
    
            // authentication setting, if any
            String[] authSettings = new String[] { "petstore_auth" };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) ApiClient.CallApi(path_, Method.DELETE, queryParams, postBody, headerParams, formParams, fileParams, pathParams, authSettings);
    
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling DeletePet: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException ((int)response.StatusCode, "Error calling DeletePet: " + response.ErrorMessage, response.ErrorMessage);
    
            return;
        }
    
        /// <summary>
        /// Deletes a pet 
        /// </summary>
        /// <param name="petId">Pet id to delete</param>
        /// <param name="apiKey"></param>
        /// <returns></returns>
        public async System.Threading.Tasks.Task DeletePetAsync (long? petId, string apiKey)
        {
            // verify the required parameter 'petId' is set
            if (petId == null) throw new ApiException(400, "Missing required parameter 'petId' when calling DeletePet");
            
    
            var path_ = "/pet/{petId}";
    
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
            if (petId != null) pathParams.Add("petId", ApiClient.ParameterToString(petId)); // path parameter
            
            
            if (apiKey != null) headerParams.Add("api_key", ApiClient.ParameterToString(apiKey)); // header parameter
            
            
            
    
            // authentication setting, if any
            String[] authSettings = new String[] { "petstore_auth" };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) await ApiClient.CallApiAsync(path_, Method.DELETE, queryParams, postBody, headerParams, formParams, fileParams, pathParams, authSettings);
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling DeletePet: " + response.Content, response.Content);

            
            return;
        }
        
        /// <summary>
        /// uploads an image 
        /// </summary>
        /// <param name="petId">ID of pet to update</param> 
        /// <param name="additionalMetadata">Additional data to pass to server</param> 
        /// <param name="file">file to upload</param> 
        /// <returns></returns>            
        public void UploadFile (long? petId, string additionalMetadata, Stream file)
        {
            
            // verify the required parameter 'petId' is set
            if (petId == null) throw new ApiException(400, "Missing required parameter 'petId' when calling UploadFile");
            
    
            var path_ = "/pet/{petId}/uploadImage";
    
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
            if (petId != null) pathParams.Add("petId", ApiClient.ParameterToString(petId)); // path parameter
            
            
            
            if (additionalMetadata != null) formParams.Add("additionalMetadata", ApiClient.ParameterToString(additionalMetadata)); // form parameter
            if (file != null) fileParams.Add("file", ApiClient.ParameterToFile("file", file));
            
            
    
            // authentication setting, if any
            String[] authSettings = new String[] { "petstore_auth" };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) ApiClient.CallApi(path_, Method.POST, queryParams, postBody, headerParams, formParams, fileParams, pathParams, authSettings);
    
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling UploadFile: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException ((int)response.StatusCode, "Error calling UploadFile: " + response.ErrorMessage, response.ErrorMessage);
    
            return;
        }
    
        /// <summary>
        /// uploads an image 
        /// </summary>
        /// <param name="petId">ID of pet to update</param>
        /// <param name="additionalMetadata">Additional data to pass to server</param>
        /// <param name="file">file to upload</param>
        /// <returns></returns>
        public async System.Threading.Tasks.Task UploadFileAsync (long? petId, string additionalMetadata, Stream file)
        {
            // verify the required parameter 'petId' is set
            if (petId == null) throw new ApiException(400, "Missing required parameter 'petId' when calling UploadFile");
            
    
            var path_ = "/pet/{petId}/uploadImage";
    
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
            if (petId != null) pathParams.Add("petId", ApiClient.ParameterToString(petId)); // path parameter
            
            
            
            if (additionalMetadata != null) formParams.Add("additionalMetadata", ApiClient.ParameterToString(additionalMetadata)); // form parameter
            if (file != null) fileParams.Add("file", ApiClient.ParameterToFile("file", file));
            
            
    
            // authentication setting, if any
            String[] authSettings = new String[] { "petstore_auth" };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) await ApiClient.CallApiAsync(path_, Method.POST, queryParams, postBody, headerParams, formParams, fileParams, pathParams, authSettings);
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling UploadFile: " + response.Content, response.Content);

            
            return;
        }
        
    }
    
}
