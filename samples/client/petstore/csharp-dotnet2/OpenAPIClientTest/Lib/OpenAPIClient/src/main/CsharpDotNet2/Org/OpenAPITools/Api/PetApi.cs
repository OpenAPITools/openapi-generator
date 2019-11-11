using System;
using System.Collections.Generic;
using RestSharp;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Org.OpenAPITools.Api
{
    /// <summary>
    /// Represents a collection of functions to interact with the API endpoints
    /// </summary>
    public interface IPetApi
    {
        /// <summary>
        /// Add a new pet to the store 
        /// </summary>
        /// <param name="pet">Pet object that needs to be added to the store</param>
        /// <returns></returns>
        void AddPet (Pet pet);
        /// <summary>
        /// Deletes a pet 
        /// </summary>
        /// <param name="petId">Pet id to delete</param>
        /// <param name="apiKey"></param>
        /// <returns></returns>
        void DeletePet (long? petId, string apiKey);
        /// <summary>
        /// Finds Pets by status Multiple status values can be provided with comma separated strings
        /// </summary>
        /// <param name="status">Status values that need to be considered for filter</param>
        /// <returns>List&lt;Pet&gt;</returns>
        List<Pet> FindPetsByStatus (List<string> status);
        /// <summary>
        /// Finds Pets by tags Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
        /// </summary>
        /// <param name="tags">Tags to filter by</param>
        /// <returns>List&lt;Pet&gt;</returns>
        List<Pet> FindPetsByTags (List<string> tags);
        /// <summary>
        /// Find pet by ID Returns a single pet
        /// </summary>
        /// <param name="petId">ID of pet to return</param>
        /// <returns>Pet</returns>
        Pet GetPetById (long? petId);
        /// <summary>
        /// Update an existing pet 
        /// </summary>
        /// <param name="pet">Pet object that needs to be added to the store</param>
        /// <returns></returns>
        void UpdatePet (Pet pet);
        /// <summary>
        /// Updates a pet in the store with form data 
        /// </summary>
        /// <param name="petId">ID of pet that needs to be updated</param>
        /// <param name="name">Updated name of the pet</param>
        /// <param name="status">Updated status of the pet</param>
        /// <returns></returns>
        void UpdatePetWithForm (long? petId, string name, string status);
        /// <summary>
        /// uploads an image 
        /// </summary>
        /// <param name="petId">ID of pet to update</param>
        /// <param name="additionalMetadata">Additional data to pass to server</param>
        /// <param name="file">file to upload</param>
        /// <returns>ApiResponse</returns>
        ApiResponse UploadFile (long? petId, string additionalMetadata, System.IO.Stream file);
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
        /// Add a new pet to the store 
        /// </summary>
        /// <param name="pet">Pet object that needs to be added to the store</param> 
        /// <returns></returns>            
        public void AddPet (Pet pet)
        {
            
            // verify the required parameter 'pet' is set
            if (pet == null) throw new ApiException(400, "Missing required parameter 'pet' when calling AddPet");
            
    
            var path = "/pet";
            path = path.Replace("{format}", "json");
                
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;
    
                                                postBody = ApiClient.Serialize(pet); // http body (model) parameter
    
            // authentication setting, if any
            String[] authSettings = new String[] { "petstore_auth" };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) ApiClient.CallApi(path, Method.POST, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
    
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling AddPet: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException ((int)response.StatusCode, "Error calling AddPet: " + response.ErrorMessage, response.ErrorMessage);
    
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
            
    
            var path = "/pet/{petId}";
            path = path.Replace("{format}", "json");
            path = path.Replace("{" + "petId" + "}", ApiClient.ParameterToString(petId));
    
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;
    
                         if (apiKey != null) headerParams.Add("api_key", ApiClient.ParameterToString(apiKey)); // header parameter
                            
            // authentication setting, if any
            String[] authSettings = new String[] { "petstore_auth" };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) ApiClient.CallApi(path, Method.DELETE, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
    
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling DeletePet: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException ((int)response.StatusCode, "Error calling DeletePet: " + response.ErrorMessage, response.ErrorMessage);
    
            return;
        }
    
        /// <summary>
        /// Finds Pets by status Multiple status values can be provided with comma separated strings
        /// </summary>
        /// <param name="status">Status values that need to be considered for filter</param> 
        /// <returns>List&lt;Pet&gt;</returns>            
        public List<Pet> FindPetsByStatus (List<string> status)
        {
            
            // verify the required parameter 'status' is set
            if (status == null) throw new ApiException(400, "Missing required parameter 'status' when calling FindPetsByStatus");
            
    
            var path = "/pet/findByStatus";
            path = path.Replace("{format}", "json");
                
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;
    
             if (status != null) queryParams.Add("status", ApiClient.ParameterToString(status)); // query parameter
                                        
            // authentication setting, if any
            String[] authSettings = new String[] { "petstore_auth" };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) ApiClient.CallApi(path, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
    
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling FindPetsByStatus: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException ((int)response.StatusCode, "Error calling FindPetsByStatus: " + response.ErrorMessage, response.ErrorMessage);
    
            return (List<Pet>) ApiClient.Deserialize(response.Content, typeof(List<Pet>), response.Headers);
        }
    
        /// <summary>
        /// Finds Pets by tags Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
        /// </summary>
        /// <param name="tags">Tags to filter by</param> 
        /// <returns>List&lt;Pet&gt;</returns>            
        public List<Pet> FindPetsByTags (List<string> tags)
        {
            
            // verify the required parameter 'tags' is set
            if (tags == null) throw new ApiException(400, "Missing required parameter 'tags' when calling FindPetsByTags");
            
    
            var path = "/pet/findByTags";
            path = path.Replace("{format}", "json");
                
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;
    
             if (tags != null) queryParams.Add("tags", ApiClient.ParameterToString(tags)); // query parameter
                                        
            // authentication setting, if any
            String[] authSettings = new String[] { "petstore_auth" };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) ApiClient.CallApi(path, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
    
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling FindPetsByTags: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException ((int)response.StatusCode, "Error calling FindPetsByTags: " + response.ErrorMessage, response.ErrorMessage);
    
            return (List<Pet>) ApiClient.Deserialize(response.Content, typeof(List<Pet>), response.Headers);
        }
    
        /// <summary>
        /// Find pet by ID Returns a single pet
        /// </summary>
        /// <param name="petId">ID of pet to return</param> 
        /// <returns>Pet</returns>            
        public Pet GetPetById (long? petId)
        {
            
            // verify the required parameter 'petId' is set
            if (petId == null) throw new ApiException(400, "Missing required parameter 'petId' when calling GetPetById");
            
    
            var path = "/pet/{petId}";
            path = path.Replace("{format}", "json");
            path = path.Replace("{" + "petId" + "}", ApiClient.ParameterToString(petId));
    
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
                throw new ApiException ((int)response.StatusCode, "Error calling GetPetById: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException ((int)response.StatusCode, "Error calling GetPetById: " + response.ErrorMessage, response.ErrorMessage);
    
            return (Pet) ApiClient.Deserialize(response.Content, typeof(Pet), response.Headers);
        }
    
        /// <summary>
        /// Update an existing pet 
        /// </summary>
        /// <param name="pet">Pet object that needs to be added to the store</param> 
        /// <returns></returns>            
        public void UpdatePet (Pet pet)
        {
            
            // verify the required parameter 'pet' is set
            if (pet == null) throw new ApiException(400, "Missing required parameter 'pet' when calling UpdatePet");
            
    
            var path = "/pet";
            path = path.Replace("{format}", "json");
                
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;
    
                                                postBody = ApiClient.Serialize(pet); // http body (model) parameter
    
            // authentication setting, if any
            String[] authSettings = new String[] { "petstore_auth" };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) ApiClient.CallApi(path, Method.PUT, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
    
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling UpdatePet: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException ((int)response.StatusCode, "Error calling UpdatePet: " + response.ErrorMessage, response.ErrorMessage);
    
            return;
        }
    
        /// <summary>
        /// Updates a pet in the store with form data 
        /// </summary>
        /// <param name="petId">ID of pet that needs to be updated</param> 
        /// <param name="name">Updated name of the pet</param> 
        /// <param name="status">Updated status of the pet</param> 
        /// <returns></returns>            
        public void UpdatePetWithForm (long? petId, string name, string status)
        {
            
            // verify the required parameter 'petId' is set
            if (petId == null) throw new ApiException(400, "Missing required parameter 'petId' when calling UpdatePetWithForm");
            
    
            var path = "/pet/{petId}";
            path = path.Replace("{format}", "json");
            path = path.Replace("{" + "petId" + "}", ApiClient.ParameterToString(petId));
    
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;
    
                                    if (name != null) formParams.Add("name", ApiClient.ParameterToString(name)); // form parameter
if (status != null) formParams.Add("status", ApiClient.ParameterToString(status)); // form parameter
                
            // authentication setting, if any
            String[] authSettings = new String[] { "petstore_auth" };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) ApiClient.CallApi(path, Method.POST, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
    
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling UpdatePetWithForm: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException ((int)response.StatusCode, "Error calling UpdatePetWithForm: " + response.ErrorMessage, response.ErrorMessage);
    
            return;
        }
    
        /// <summary>
        /// uploads an image 
        /// </summary>
        /// <param name="petId">ID of pet to update</param> 
        /// <param name="additionalMetadata">Additional data to pass to server</param> 
        /// <param name="file">file to upload</param> 
        /// <returns>ApiResponse</returns>            
        public ApiResponse UploadFile (long? petId, string additionalMetadata, System.IO.Stream file)
        {
            
            // verify the required parameter 'petId' is set
            if (petId == null) throw new ApiException(400, "Missing required parameter 'petId' when calling UploadFile");
            
    
            var path = "/pet/{petId}/uploadImage";
            path = path.Replace("{format}", "json");
            path = path.Replace("{" + "petId" + "}", ApiClient.ParameterToString(petId));
    
            var queryParams = new Dictionary<String, String>();
            var headerParams = new Dictionary<String, String>();
            var formParams = new Dictionary<String, String>();
            var fileParams = new Dictionary<String, FileParameter>();
            String postBody = null;
    
                                    if (additionalMetadata != null) formParams.Add("additionalMetadata", ApiClient.ParameterToString(additionalMetadata)); // form parameter
if (file != null) fileParams.Add("file", ApiClient.ParameterToFile("file", file));
                
            // authentication setting, if any
            String[] authSettings = new String[] { "petstore_auth" };
    
            // make the HTTP request
            IRestResponse response = (IRestResponse) ApiClient.CallApi(path, Method.POST, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
    
            if (((int)response.StatusCode) >= 400)
                throw new ApiException ((int)response.StatusCode, "Error calling UploadFile: " + response.Content, response.Content);
            else if (((int)response.StatusCode) == 0)
                throw new ApiException ((int)response.StatusCode, "Error calling UploadFile: " + response.ErrorMessage, response.ErrorMessage);
    
            return (ApiResponse) ApiClient.Deserialize(response.Content, typeof(ApiResponse), response.Headers);
        }
    
    }
}
