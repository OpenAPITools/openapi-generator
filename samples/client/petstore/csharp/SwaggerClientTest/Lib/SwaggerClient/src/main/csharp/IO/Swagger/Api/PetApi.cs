using System;
using System.IO;
using System.Collections.Generic;
using System.Threading.Tasks;
using RestSharp;
using IO.Swagger.Client;
using IO.Swagger.Model;

namespace IO.Swagger.Api {
  

  public interface IPetApi {
    
    /// <summary>
    /// Update an existing pet 
    /// </summary>
    /// <param name="body">Pet object that needs to be added to the store</param>
    /// <returns></returns>
    void UpdatePet (Pet body);

    /// <summary>
    /// Update an existing pet 
    /// </summary>
    /// <param name="body">Pet object that needs to be added to the store</param>
    /// <returns></returns>
    Task UpdatePetAsync (Pet body);
    
    /// <summary>
    /// Add a new pet to the store 
    /// </summary>
    /// <param name="body">Pet object that needs to be added to the store</param>
    /// <returns></returns>
    void AddPet (Pet body);

    /// <summary>
    /// Add a new pet to the store 
    /// </summary>
    /// <param name="body">Pet object that needs to be added to the store</param>
    /// <returns></returns>
    Task AddPetAsync (Pet body);
    
    /// <summary>
    /// Finds Pets by status Multiple status values can be provided with comma seperated strings
    /// </summary>
    /// <param name="status">Status values that need to be considered for filter</param>
    /// <returns>List<Pet></returns>
    List<Pet> FindPetsByStatus (List<string> status);

    /// <summary>
    /// Finds Pets by status Multiple status values can be provided with comma seperated strings
    /// </summary>
    /// <param name="status">Status values that need to be considered for filter</param>
    /// <returns>List<Pet></returns>
    Task<List<Pet>> FindPetsByStatusAsync (List<string> status);
    
    /// <summary>
    /// Finds Pets by tags Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
    /// </summary>
    /// <param name="tags">Tags to filter by</param>
    /// <returns>List<Pet></returns>
    List<Pet> FindPetsByTags (List<string> tags);

    /// <summary>
    /// Finds Pets by tags Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
    /// </summary>
    /// <param name="tags">Tags to filter by</param>
    /// <returns>List<Pet></returns>
    Task<List<Pet>> FindPetsByTagsAsync (List<string> tags);
    
    /// <summary>
    /// Find pet by ID Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
    /// </summary>
    /// <param name="petId">ID of pet that needs to be fetched</param>
    /// <returns>Pet</returns>
    Pet GetPetById (long? petId);

    /// <summary>
    /// Find pet by ID Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
    /// </summary>
    /// <param name="petId">ID of pet that needs to be fetched</param>
    /// <returns>Pet</returns>
    Task<Pet> GetPetByIdAsync (long? petId);
    
    /// <summary>
    /// Updates a pet in the store with form data 
    /// </summary>
    /// <param name="petId">ID of pet that needs to be updated</param>
    /// <param name="name">Updated name of the pet</param>
    /// <param name="status">Updated status of the pet</param>
    /// <returns></returns>
    void UpdatePetWithForm (string petId, string name, string status);

    /// <summary>
    /// Updates a pet in the store with form data 
    /// </summary>
    /// <param name="petId">ID of pet that needs to be updated</param>
    /// <param name="name">Updated name of the pet</param>
    /// <param name="status">Updated status of the pet</param>
    /// <returns></returns>
    Task UpdatePetWithFormAsync (string petId, string name, string status);
    
    /// <summary>
    /// Deletes a pet 
    /// </summary>
    /// <param name="apiKey"></param>
    /// <param name="petId">Pet id to delete</param>
    /// <returns></returns>
    void DeletePet (string apiKey, long? petId);

    /// <summary>
    /// Deletes a pet 
    /// </summary>
    /// <param name="apiKey"></param>
    /// <param name="petId">Pet id to delete</param>
    /// <returns></returns>
    Task DeletePetAsync (string apiKey, long? petId);
    
    /// <summary>
    /// uploads an image 
    /// </summary>
    /// <param name="petId">ID of pet to update</param>
    /// <param name="additionalMetadata">Additional data to pass to server</param>
    /// <param name="file">file to upload</param>
    /// <returns></returns>
    void UploadFile (long? petId, string additionalMetadata, String file);

    /// <summary>
    /// uploads an image 
    /// </summary>
    /// <param name="petId">ID of pet to update</param>
    /// <param name="additionalMetadata">Additional data to pass to server</param>
    /// <param name="file">file to upload</param>
    /// <returns></returns>
    Task UploadFileAsync (long? petId, string additionalMetadata, String file);
    
  }

  /// <summary>
  /// Represents a collection of functions to interact with the API endpoints
  /// </summary>
  public class PetApi : IPetApi {

    /// <summary>
    /// Initializes a new instance of the <see cref="PetApi"/> class.
    /// </summary>
    /// <param name="apiClient"> an instance of ApiClient (optional)
    /// <returns></returns>
    public PetApi(ApiClient apiClient = null) {
      if (apiClient == null) { // use the default one in Configuration
        this.ApiClient = Configuration.DefaultApiClient; 
      } else {
        this.ApiClient = apiClient;
      }
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
    /// <value>The base path</value>
    public void SetBasePath(String basePath) {
      this.ApiClient.BasePath = basePath;
    }

    /// <summary>
    /// Gets the base path of the API client.
    /// </summary>
    /// <value>The base path</value>
    public String GetBasePath(String basePath) {
      return this.ApiClient.BasePath;
    }

    /// <summary>
    /// Gets or sets the API client.
    /// </summary>
    /// <value>The API client</value>
    public ApiClient ApiClient {get; set;}


    
    /// <summary>
    /// Update an existing pet 
    /// </summary>
    /// <param name="body">Pet object that needs to be added to the store</param>
    /// <returns></returns>
    public void UpdatePet (Pet body) {

      

      var path = "/pet";
      path = path.Replace("{format}", "json");
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

      
      
      
      postBody = ApiClient.Serialize(body); // http body (model) parameter
      

      // authentication setting, if any
      String[] authSettings = new String[] { "petstore_auth" };

      // make the HTTP request
      IRestResponse response = (IRestResponse) ApiClient.CallApi(path, Method.PUT, queryParams, postBody, headerParams, formParams, fileParams, authSettings);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling UpdatePet: " + response.Content, response.Content);
      }

      return;
    }

    /// <summary>
    /// Update an existing pet 
    /// </summary>
    /// <param name="body">Pet object that needs to be added to the store</param>
    /// <returns></returns>
    public async Task UpdatePetAsync (Pet body) {

      

      var path = "/pet";
      path = path.Replace("{format}", "json");
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

      
      
      
      postBody = ApiClient.Serialize(body); // http body (model) parameter
      

      // authentication setting, if any
      String[] authSettings = new String[] { "petstore_auth" };

      // make the HTTP request
      IRestResponse response = (IRestResponse) await ApiClient.CallApiAsync(path, Method.PUT, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling UpdatePet: " + response.Content, response.Content);
      }
      
      return;
    }
    
    /// <summary>
    /// Add a new pet to the store 
    /// </summary>
    /// <param name="body">Pet object that needs to be added to the store</param>
    /// <returns></returns>
    public void AddPet (Pet body) {

      

      var path = "/pet";
      path = path.Replace("{format}", "json");
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

      
      
      
      postBody = ApiClient.Serialize(body); // http body (model) parameter
      

      // authentication setting, if any
      String[] authSettings = new String[] { "petstore_auth" };

      // make the HTTP request
      IRestResponse response = (IRestResponse) ApiClient.CallApi(path, Method.POST, queryParams, postBody, headerParams, formParams, fileParams, authSettings);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling AddPet: " + response.Content, response.Content);
      }

      return;
    }

    /// <summary>
    /// Add a new pet to the store 
    /// </summary>
    /// <param name="body">Pet object that needs to be added to the store</param>
    /// <returns></returns>
    public async Task AddPetAsync (Pet body) {

      

      var path = "/pet";
      path = path.Replace("{format}", "json");
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

      
      
      
      postBody = ApiClient.Serialize(body); // http body (model) parameter
      

      // authentication setting, if any
      String[] authSettings = new String[] { "petstore_auth" };

      // make the HTTP request
      IRestResponse response = (IRestResponse) await ApiClient.CallApiAsync(path, Method.POST, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling AddPet: " + response.Content, response.Content);
      }
      
      return;
    }
    
    /// <summary>
    /// Finds Pets by status Multiple status values can be provided with comma seperated strings
    /// </summary>
    /// <param name="status">Status values that need to be considered for filter</param>
    /// <returns>List<Pet></returns>
    public List<Pet> FindPetsByStatus (List<string> status) {

      

      var path = "/pet/findByStatus";
      path = path.Replace("{format}", "json");
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

       if (status != null) queryParams.Add("status", ApiClient.ParameterToString(status)); // query parameter
      
      
      
      

      // authentication setting, if any
      String[] authSettings = new String[] { "petstore_auth" };

      // make the HTTP request
      IRestResponse response = (IRestResponse) ApiClient.CallApi(path, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, authSettings);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling FindPetsByStatus: " + response.Content, response.Content);
      }

       // if return type is "String" (not "string"), it implies a Filestream and should return the file path
      String returnTypeString = "List<Pet>";
      Type returnType = returnTypeString == "String" ? typeof(FileStream) : typeof(List<Pet>);
      return (List<Pet>) ApiClient.Deserialize(response.Content, returnType, response.Headers);
    }

    /// <summary>
    /// Finds Pets by status Multiple status values can be provided with comma seperated strings
    /// </summary>
    /// <param name="status">Status values that need to be considered for filter</param>
    /// <returns>List<Pet></returns>
    public async Task<List<Pet>> FindPetsByStatusAsync (List<string> status) {

      

      var path = "/pet/findByStatus";
      path = path.Replace("{format}", "json");
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

       if (status != null) queryParams.Add("status", ApiClient.ParameterToString(status)); // query parameter
      
      
      
      

      // authentication setting, if any
      String[] authSettings = new String[] { "petstore_auth" };

      // make the HTTP request
      IRestResponse response = (IRestResponse) await ApiClient.CallApiAsync(path, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling FindPetsByStatus: " + response.Content, response.Content);
      }
      return (List<Pet>) ApiClient.Deserialize(response.Content, typeof(List<Pet>), response.Headers);
    }
    
    /// <summary>
    /// Finds Pets by tags Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
    /// </summary>
    /// <param name="tags">Tags to filter by</param>
    /// <returns>List<Pet></returns>
    public List<Pet> FindPetsByTags (List<string> tags) {

      

      var path = "/pet/findByTags";
      path = path.Replace("{format}", "json");
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

       if (tags != null) queryParams.Add("tags", ApiClient.ParameterToString(tags)); // query parameter
      
      
      
      

      // authentication setting, if any
      String[] authSettings = new String[] { "petstore_auth" };

      // make the HTTP request
      IRestResponse response = (IRestResponse) ApiClient.CallApi(path, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, authSettings);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling FindPetsByTags: " + response.Content, response.Content);
      }

       // if return type is "String" (not "string"), it implies a Filestream and should return the file path
      String returnTypeString = "List<Pet>";
      Type returnType = returnTypeString == "String" ? typeof(FileStream) : typeof(List<Pet>);
      return (List<Pet>) ApiClient.Deserialize(response.Content, returnType, response.Headers);
    }

    /// <summary>
    /// Finds Pets by tags Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
    /// </summary>
    /// <param name="tags">Tags to filter by</param>
    /// <returns>List<Pet></returns>
    public async Task<List<Pet>> FindPetsByTagsAsync (List<string> tags) {

      

      var path = "/pet/findByTags";
      path = path.Replace("{format}", "json");
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

       if (tags != null) queryParams.Add("tags", ApiClient.ParameterToString(tags)); // query parameter
      
      
      
      

      // authentication setting, if any
      String[] authSettings = new String[] { "petstore_auth" };

      // make the HTTP request
      IRestResponse response = (IRestResponse) await ApiClient.CallApiAsync(path, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling FindPetsByTags: " + response.Content, response.Content);
      }
      return (List<Pet>) ApiClient.Deserialize(response.Content, typeof(List<Pet>), response.Headers);
    }
    
    /// <summary>
    /// Find pet by ID Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
    /// </summary>
    /// <param name="petId">ID of pet that needs to be fetched</param>
    /// <returns>Pet</returns>
    public Pet GetPetById (long? petId) {

      
      // verify the required parameter 'petId' is set
      if (petId == null) throw new ApiException(400, "Missing required parameter 'petId' when calling GetPetById");
      

      var path = "/pet/{petId}";
      path = path.Replace("{format}", "json");
      path = path.Replace("{" + "petId" + "}", ApiClient.ParameterToString(petId));
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

      
      
      
      

      // authentication setting, if any
      String[] authSettings = new String[] { "api_key", "petstore_auth" };

      // make the HTTP request
      IRestResponse response = (IRestResponse) ApiClient.CallApi(path, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, authSettings);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling GetPetById: " + response.Content, response.Content);
      }

       // if return type is "String" (not "string"), it implies a Filestream and should return the file path
      String returnTypeString = "Pet";
      Type returnType = returnTypeString == "String" ? typeof(FileStream) : typeof(Pet);
      return (Pet) ApiClient.Deserialize(response.Content, returnType, response.Headers);
    }

    /// <summary>
    /// Find pet by ID Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
    /// </summary>
    /// <param name="petId">ID of pet that needs to be fetched</param>
    /// <returns>Pet</returns>
    public async Task<Pet> GetPetByIdAsync (long? petId) {

      
          // verify the required parameter 'petId' is set
          if (petId == null) throw new ApiException(400, "Missing required parameter 'petId' when calling GetPetById");
      

      var path = "/pet/{petId}";
      path = path.Replace("{format}", "json");
      path = path.Replace("{" + "petId" + "}", ApiClient.ParameterToString(petId));
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

      
      
      
      

      // authentication setting, if any
      String[] authSettings = new String[] { "api_key", "petstore_auth" };

      // make the HTTP request
      IRestResponse response = (IRestResponse) await ApiClient.CallApiAsync(path, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling GetPetById: " + response.Content, response.Content);
      }
      return (Pet) ApiClient.Deserialize(response.Content, typeof(Pet), response.Headers);
    }
    
    /// <summary>
    /// Updates a pet in the store with form data 
    /// </summary>
    /// <param name="petId">ID of pet that needs to be updated</param>
    /// <param name="name">Updated name of the pet</param>
    /// <param name="status">Updated status of the pet</param>
    /// <returns></returns>
    public void UpdatePetWithForm (string petId, string name, string status) {

      
      // verify the required parameter 'petId' is set
      if (petId == null) throw new ApiException(400, "Missing required parameter 'petId' when calling UpdatePetWithForm");
      

      var path = "/pet/{petId}";
      path = path.Replace("{format}", "json");
      path = path.Replace("{" + "petId" + "}", ApiClient.ParameterToString(petId));
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

      
      
      if (name != null) formParams.Add("name", ApiClient.ParameterToString(name)); // form parameter
      if (status != null) formParams.Add("status", ApiClient.ParameterToString(status)); // form parameter
      
      

      // authentication setting, if any
      String[] authSettings = new String[] { "petstore_auth" };

      // make the HTTP request
      IRestResponse response = (IRestResponse) ApiClient.CallApi(path, Method.POST, queryParams, postBody, headerParams, formParams, fileParams, authSettings);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling UpdatePetWithForm: " + response.Content, response.Content);
      }

      return;
    }

    /// <summary>
    /// Updates a pet in the store with form data 
    /// </summary>
    /// <param name="petId">ID of pet that needs to be updated</param>
    /// <param name="name">Updated name of the pet</param>
    /// <param name="status">Updated status of the pet</param>
    /// <returns></returns>
    public async Task UpdatePetWithFormAsync (string petId, string name, string status) {

      
          // verify the required parameter 'petId' is set
          if (petId == null) throw new ApiException(400, "Missing required parameter 'petId' when calling UpdatePetWithForm");
      

      var path = "/pet/{petId}";
      path = path.Replace("{format}", "json");
      path = path.Replace("{" + "petId" + "}", ApiClient.ParameterToString(petId));
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

      
      
      if (name != null) formParams.Add("name", ApiClient.ParameterToString(name)); // form parameter
      if (status != null) formParams.Add("status", ApiClient.ParameterToString(status)); // form parameter
      
      

      // authentication setting, if any
      String[] authSettings = new String[] { "petstore_auth" };

      // make the HTTP request
      IRestResponse response = (IRestResponse) await ApiClient.CallApiAsync(path, Method.POST, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling UpdatePetWithForm: " + response.Content, response.Content);
      }
      
      return;
    }
    
    /// <summary>
    /// Deletes a pet 
    /// </summary>
    /// <param name="apiKey"></param>
    /// <param name="petId">Pet id to delete</param>
    /// <returns></returns>
    public void DeletePet (string apiKey, long? petId) {

      
      // verify the required parameter 'petId' is set
      if (petId == null) throw new ApiException(400, "Missing required parameter 'petId' when calling DeletePet");
      

      var path = "/pet/{petId}";
      path = path.Replace("{format}", "json");
      path = path.Replace("{" + "petId" + "}", ApiClient.ParameterToString(petId));
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

      
       if (apiKey != null) headerParams.Add("api_key", ApiClient.ParameterToString(apiKey)); // header parameter
      
      
      

      // authentication setting, if any
      String[] authSettings = new String[] { "petstore_auth" };

      // make the HTTP request
      IRestResponse response = (IRestResponse) ApiClient.CallApi(path, Method.DELETE, queryParams, postBody, headerParams, formParams, fileParams, authSettings);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling DeletePet: " + response.Content, response.Content);
      }

      return;
    }

    /// <summary>
    /// Deletes a pet 
    /// </summary>
    /// <param name="apiKey"></param>
    /// <param name="petId">Pet id to delete</param>
    /// <returns></returns>
    public async Task DeletePetAsync (string apiKey, long? petId) {

      
          // verify the required parameter 'petId' is set
          if (petId == null) throw new ApiException(400, "Missing required parameter 'petId' when calling DeletePet");
      

      var path = "/pet/{petId}";
      path = path.Replace("{format}", "json");
      path = path.Replace("{" + "petId" + "}", ApiClient.ParameterToString(petId));
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

      
       if (apiKey != null) headerParams.Add("api_key", ApiClient.ParameterToString(apiKey)); // header parameter
      
      
      

      // authentication setting, if any
      String[] authSettings = new String[] { "petstore_auth" };

      // make the HTTP request
      IRestResponse response = (IRestResponse) await ApiClient.CallApiAsync(path, Method.DELETE, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling DeletePet: " + response.Content, response.Content);
      }
      
      return;
    }
    
    /// <summary>
    /// uploads an image 
    /// </summary>
    /// <param name="petId">ID of pet to update</param>
    /// <param name="additionalMetadata">Additional data to pass to server</param>
    /// <param name="file">file to upload</param>
    /// <returns></returns>
    public void UploadFile (long? petId, string additionalMetadata, String file) {

      
      // verify the required parameter 'petId' is set
      if (petId == null) throw new ApiException(400, "Missing required parameter 'petId' when calling UploadFile");
      

      var path = "/pet/{petId}/uploadImage";
      path = path.Replace("{format}", "json");
      path = path.Replace("{" + "petId" + "}", ApiClient.ParameterToString(petId));
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

      
      
      if (additionalMetadata != null) formParams.Add("additionalMetadata", ApiClient.ParameterToString(additionalMetadata)); // form parameter
      if (file != null) fileParams.Add("file", ApiClient.ParameterToString(file));
      
      

      // authentication setting, if any
      String[] authSettings = new String[] { "petstore_auth" };

      // make the HTTP request
      IRestResponse response = (IRestResponse) ApiClient.CallApi(path, Method.POST, queryParams, postBody, headerParams, formParams, fileParams, authSettings);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling UploadFile: " + response.Content, response.Content);
      }

      return;
    }

    /// <summary>
    /// uploads an image 
    /// </summary>
    /// <param name="petId">ID of pet to update</param>
    /// <param name="additionalMetadata">Additional data to pass to server</param>
    /// <param name="file">file to upload</param>
    /// <returns></returns>
    public async Task UploadFileAsync (long? petId, string additionalMetadata, String file) {

      
          // verify the required parameter 'petId' is set
          if (petId == null) throw new ApiException(400, "Missing required parameter 'petId' when calling UploadFile");
      

      var path = "/pet/{petId}/uploadImage";
      path = path.Replace("{format}", "json");
      path = path.Replace("{" + "petId" + "}", ApiClient.ParameterToString(petId));
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

      
      
      if (additionalMetadata != null) formParams.Add("additionalMetadata", ApiClient.ParameterToString(additionalMetadata)); // form parameter
      if (file != null) fileParams.Add("file", ApiClient.ParameterToString(file));
      
      

      // authentication setting, if any
      String[] authSettings = new String[] { "petstore_auth" };

      // make the HTTP request
      IRestResponse response = (IRestResponse) await ApiClient.CallApiAsync(path, Method.POST, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling UploadFile: " + response.Content, response.Content);
      }
      
      return;
    }
    
  }  
  
}
