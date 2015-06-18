using System;
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
    /// <param name="Body">Pet object that needs to be added to the store</param>
    /// <returns></returns>
    void UpdatePet (Pet Body);

    /// <summary>
    /// Update an existing pet 
    /// </summary>
    /// <param name="Body">Pet object that needs to be added to the store</param>
    /// <returns></returns>
    Task UpdatePetAsync (Pet Body);
    
    /// <summary>
    /// Add a new pet to the store 
    /// </summary>
    /// <param name="Body">Pet object that needs to be added to the store</param>
    /// <returns></returns>
    void AddPet (Pet Body);

    /// <summary>
    /// Add a new pet to the store 
    /// </summary>
    /// <param name="Body">Pet object that needs to be added to the store</param>
    /// <returns></returns>
    Task AddPetAsync (Pet Body);
    
    /// <summary>
    /// Finds Pets by status Multiple status values can be provided with comma seperated strings
    /// </summary>
    /// <param name="Status">Status values that need to be considered for filter</param>
    /// <returns>List<Pet></returns>
    List<Pet> FindPetsByStatus (List<string> Status);

    /// <summary>
    /// Finds Pets by status Multiple status values can be provided with comma seperated strings
    /// </summary>
    /// <param name="Status">Status values that need to be considered for filter</param>
    /// <returns>List<Pet></returns>
    Task<List<Pet>> FindPetsByStatusAsync (List<string> Status);
    
    /// <summary>
    /// Finds Pets by tags Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
    /// </summary>
    /// <param name="Tags">Tags to filter by</param>
    /// <returns>List<Pet></returns>
    List<Pet> FindPetsByTags (List<string> Tags);

    /// <summary>
    /// Finds Pets by tags Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
    /// </summary>
    /// <param name="Tags">Tags to filter by</param>
    /// <returns>List<Pet></returns>
    Task<List<Pet>> FindPetsByTagsAsync (List<string> Tags);
    
    /// <summary>
    /// Find pet by ID Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
    /// </summary>
    /// <param name="PetId">ID of pet that needs to be fetched</param>
    /// <returns>Pet</returns>
    Pet GetPetById (long? PetId);

    /// <summary>
    /// Find pet by ID Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
    /// </summary>
    /// <param name="PetId">ID of pet that needs to be fetched</param>
    /// <returns>Pet</returns>
    Task<Pet> GetPetByIdAsync (long? PetId);
    
    /// <summary>
    /// Updates a pet in the store with form data 
    /// </summary>
    /// <param name="PetId">ID of pet that needs to be updated</param>/// <param name="Name">Updated name of the pet</param>/// <param name="Status">Updated status of the pet</param>
    /// <returns></returns>
    void UpdatePetWithForm (string PetId, string Name, string Status);

    /// <summary>
    /// Updates a pet in the store with form data 
    /// </summary>
    /// <param name="PetId">ID of pet that needs to be updated</param>/// <param name="Name">Updated name of the pet</param>/// <param name="Status">Updated status of the pet</param>
    /// <returns></returns>
    Task UpdatePetWithFormAsync (string PetId, string Name, string Status);
    
    /// <summary>
    /// Deletes a pet 
    /// </summary>
    /// <param name="ApiKey"></param>/// <param name="PetId">Pet id to delete</param>
    /// <returns></returns>
    void DeletePet (string ApiKey, long? PetId);

    /// <summary>
    /// Deletes a pet 
    /// </summary>
    /// <param name="ApiKey"></param>/// <param name="PetId">Pet id to delete</param>
    /// <returns></returns>
    Task DeletePetAsync (string ApiKey, long? PetId);
    
    /// <summary>
    /// uploads an image 
    /// </summary>
    /// <param name="PetId">ID of pet to update</param>/// <param name="AdditionalMetadata">Additional data to pass to server</param>/// <param name="File">file to upload</param>
    /// <returns></returns>
    void UploadFile (long? PetId, string AdditionalMetadata, string File);

    /// <summary>
    /// uploads an image 
    /// </summary>
    /// <param name="PetId">ID of pet to update</param>/// <param name="AdditionalMetadata">Additional data to pass to server</param>/// <param name="File">file to upload</param>
    /// <returns></returns>
    Task UploadFileAsync (long? PetId, string AdditionalMetadata, string File);
    
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
        this.apiClient = Configuration.apiClient; 
      } else {
        this.apiClient = apiClient;
      }
    }

    /// <summary>
    /// Initializes a new instance of the <see cref="PetApi"/> class.
    /// </summary>
    /// <returns></returns>
    public PetApi(String basePath)
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
    /// Update an existing pet 
    /// </summary>
    /// <param name="Body">Pet object that needs to be added to the store</param>
    /// <returns></returns>
    public void UpdatePet (Pet Body) {

      

      var path = "/pet";
      path = path.Replace("{format}", "json");
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

      
      
      
      postBody = apiClient.Serialize(Body); // http body (model) parameter
      

      // authentication setting, if any
      String[] authSettings = new String[] { "petstore_auth" };

      // make the HTTP request
      IRestResponse response = (IRestResponse) apiClient.CallApi(path, Method.PUT, queryParams, postBody, headerParams, formParams, fileParams, authSettings);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling UpdatePet: " + response.Content, response.Content);
      }
      
      return;
    }
	
	 /// <summary>
    /// Update an existing pet 
    /// </summary>
    /// <param name="Body">Pet object that needs to be added to the store</param>
    /// <returns></returns>
    public async Task UpdatePetAsync (Pet Body) {

      

      var path = "/pet";
      path = path.Replace("{format}", "json");
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

      
      
      
      postBody = apiClient.Serialize(Body); // http body (model) parameter
      

      // authentication setting, if any
      String[] authSettings = new String[] { "petstore_auth" };

      // make the HTTP request
      IRestResponse response = (IRestResponse) await apiClient.CallApiAsync(path, Method.PUT, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling UpdatePet: " + response.Content, response.Content);
      }
      
      return;
    }
    
    /// <summary>
    /// Add a new pet to the store 
    /// </summary>
    /// <param name="Body">Pet object that needs to be added to the store</param>
    /// <returns></returns>
    public void AddPet (Pet Body) {

      

      var path = "/pet";
      path = path.Replace("{format}", "json");
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

      
      
      
      postBody = apiClient.Serialize(Body); // http body (model) parameter
      

      // authentication setting, if any
      String[] authSettings = new String[] { "petstore_auth" };

      // make the HTTP request
      IRestResponse response = (IRestResponse) apiClient.CallApi(path, Method.POST, queryParams, postBody, headerParams, formParams, fileParams, authSettings);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling AddPet: " + response.Content, response.Content);
      }
      
      return;
    }
	
	 /// <summary>
    /// Add a new pet to the store 
    /// </summary>
    /// <param name="Body">Pet object that needs to be added to the store</param>
    /// <returns></returns>
    public async Task AddPetAsync (Pet Body) {

      

      var path = "/pet";
      path = path.Replace("{format}", "json");
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

      
      
      
      postBody = apiClient.Serialize(Body); // http body (model) parameter
      

      // authentication setting, if any
      String[] authSettings = new String[] { "petstore_auth" };

      // make the HTTP request
      IRestResponse response = (IRestResponse) await apiClient.CallApiAsync(path, Method.POST, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling AddPet: " + response.Content, response.Content);
      }
      
      return;
    }
    
    /// <summary>
    /// Finds Pets by status Multiple status values can be provided with comma seperated strings
    /// </summary>
    /// <param name="Status">Status values that need to be considered for filter</param>
    /// <returns>List<Pet></returns>
    public List<Pet> FindPetsByStatus (List<string> Status) {

      

      var path = "/pet/findByStatus";
      path = path.Replace("{format}", "json");
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

       if (Status != null) queryParams.Add("status", apiClient.ParameterToString(Status)); // query parameter
      
      
      
      

      // authentication setting, if any
      String[] authSettings = new String[] { "petstore_auth" };

      // make the HTTP request
      IRestResponse response = (IRestResponse) apiClient.CallApi(path, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, authSettings);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling FindPetsByStatus: " + response.Content, response.Content);
      }
      return (List<Pet>) apiClient.Deserialize(response.Content, typeof(List<Pet>));
    }
	
	 /// <summary>
    /// Finds Pets by status Multiple status values can be provided with comma seperated strings
    /// </summary>
    /// <param name="Status">Status values that need to be considered for filter</param>
    /// <returns>List<Pet></returns>
    public async Task<List<Pet>> FindPetsByStatusAsync (List<string> Status) {

      

      var path = "/pet/findByStatus";
      path = path.Replace("{format}", "json");
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

       if (Status != null) queryParams.Add("status", apiClient.ParameterToString(Status)); // query parameter
      
      
      
      

      // authentication setting, if any
      String[] authSettings = new String[] { "petstore_auth" };

      // make the HTTP request
      IRestResponse response = (IRestResponse) await apiClient.CallApiAsync(path, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling FindPetsByStatus: " + response.Content, response.Content);
      }
      return (List<Pet>) apiClient.Deserialize(response.Content, typeof(List<Pet>));
    }
    
    /// <summary>
    /// Finds Pets by tags Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
    /// </summary>
    /// <param name="Tags">Tags to filter by</param>
    /// <returns>List<Pet></returns>
    public List<Pet> FindPetsByTags (List<string> Tags) {

      

      var path = "/pet/findByTags";
      path = path.Replace("{format}", "json");
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

       if (Tags != null) queryParams.Add("tags", apiClient.ParameterToString(Tags)); // query parameter
      
      
      
      

      // authentication setting, if any
      String[] authSettings = new String[] { "petstore_auth" };

      // make the HTTP request
      IRestResponse response = (IRestResponse) apiClient.CallApi(path, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, authSettings);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling FindPetsByTags: " + response.Content, response.Content);
      }
      return (List<Pet>) apiClient.Deserialize(response.Content, typeof(List<Pet>));
    }
	
	 /// <summary>
    /// Finds Pets by tags Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
    /// </summary>
    /// <param name="Tags">Tags to filter by</param>
    /// <returns>List<Pet></returns>
    public async Task<List<Pet>> FindPetsByTagsAsync (List<string> Tags) {

      

      var path = "/pet/findByTags";
      path = path.Replace("{format}", "json");
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

       if (Tags != null) queryParams.Add("tags", apiClient.ParameterToString(Tags)); // query parameter
      
      
      
      

      // authentication setting, if any
      String[] authSettings = new String[] { "petstore_auth" };

      // make the HTTP request
      IRestResponse response = (IRestResponse) await apiClient.CallApiAsync(path, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling FindPetsByTags: " + response.Content, response.Content);
      }
      return (List<Pet>) apiClient.Deserialize(response.Content, typeof(List<Pet>));
    }
    
    /// <summary>
    /// Find pet by ID Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
    /// </summary>
    /// <param name="PetId">ID of pet that needs to be fetched</param>
    /// <returns>Pet</returns>
    public Pet GetPetById (long? PetId) {

      
      // verify the required parameter 'PetId' is set
      if (PetId == null) throw new ApiException(400, "Missing required parameter 'PetId' when calling GetPetById");
      

      var path = "/pet/{petId}";
      path = path.Replace("{format}", "json");
      path = path.Replace("{" + "petId" + "}", apiClient.ParameterToString(PetId));
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

      
      
      
      

      // authentication setting, if any
      String[] authSettings = new String[] { "api_key", "petstore_auth" };

      // make the HTTP request
      IRestResponse response = (IRestResponse) apiClient.CallApi(path, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, authSettings);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling GetPetById: " + response.Content, response.Content);
      }
      return (Pet) apiClient.Deserialize(response.Content, typeof(Pet));
    }
	
	 /// <summary>
    /// Find pet by ID Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
    /// </summary>
    /// <param name="PetId">ID of pet that needs to be fetched</param>
    /// <returns>Pet</returns>
    public async Task<Pet> GetPetByIdAsync (long? PetId) {

      
          // verify the required parameter 'PetId' is set
          if (PetId == null) throw new ApiException(400, "Missing required parameter 'PetId' when calling GetPetById");
      

      var path = "/pet/{petId}";
      path = path.Replace("{format}", "json");
      path = path.Replace("{" + "petId" + "}", apiClient.ParameterToString(PetId));
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

      
      
      
      

      // authentication setting, if any
      String[] authSettings = new String[] { "api_key", "petstore_auth" };

      // make the HTTP request
      IRestResponse response = (IRestResponse) await apiClient.CallApiAsync(path, Method.GET, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling GetPetById: " + response.Content, response.Content);
      }
      return (Pet) apiClient.Deserialize(response.Content, typeof(Pet));
    }
    
    /// <summary>
    /// Updates a pet in the store with form data 
    /// </summary>
    /// <param name="PetId">ID of pet that needs to be updated</param>/// <param name="Name">Updated name of the pet</param>/// <param name="Status">Updated status of the pet</param>
    /// <returns></returns>
    public void UpdatePetWithForm (string PetId, string Name, string Status) {

      
      // verify the required parameter 'PetId' is set
      if (PetId == null) throw new ApiException(400, "Missing required parameter 'PetId' when calling UpdatePetWithForm");
      

      var path = "/pet/{petId}";
      path = path.Replace("{format}", "json");
      path = path.Replace("{" + "petId" + "}", apiClient.ParameterToString(PetId));
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

      
      
      if (Name != null) formParams.Add("name", apiClient.ParameterToString(Name)); // form parameter
      if (Status != null) formParams.Add("status", apiClient.ParameterToString(Status)); // form parameter
      
      

      // authentication setting, if any
      String[] authSettings = new String[] { "petstore_auth" };

      // make the HTTP request
      IRestResponse response = (IRestResponse) apiClient.CallApi(path, Method.POST, queryParams, postBody, headerParams, formParams, fileParams, authSettings);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling UpdatePetWithForm: " + response.Content, response.Content);
      }
      
      return;
    }
	
	 /// <summary>
    /// Updates a pet in the store with form data 
    /// </summary>
    /// <param name="PetId">ID of pet that needs to be updated</param>/// <param name="Name">Updated name of the pet</param>/// <param name="Status">Updated status of the pet</param>
    /// <returns></returns>
    public async Task UpdatePetWithFormAsync (string PetId, string Name, string Status) {

      
          // verify the required parameter 'PetId' is set
          if (PetId == null) throw new ApiException(400, "Missing required parameter 'PetId' when calling UpdatePetWithForm");
      

      var path = "/pet/{petId}";
      path = path.Replace("{format}", "json");
      path = path.Replace("{" + "petId" + "}", apiClient.ParameterToString(PetId));
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

      
      
      if (Name != null) formParams.Add("name", apiClient.ParameterToString(Name)); // form parameter
      if (Status != null) formParams.Add("status", apiClient.ParameterToString(Status)); // form parameter
      
      

      // authentication setting, if any
      String[] authSettings = new String[] { "petstore_auth" };

      // make the HTTP request
      IRestResponse response = (IRestResponse) await apiClient.CallApiAsync(path, Method.POST, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling UpdatePetWithForm: " + response.Content, response.Content);
      }
      
      return;
    }
    
    /// <summary>
    /// Deletes a pet 
    /// </summary>
    /// <param name="ApiKey"></param>/// <param name="PetId">Pet id to delete</param>
    /// <returns></returns>
    public void DeletePet (string ApiKey, long? PetId) {

      
      // verify the required parameter 'PetId' is set
      if (PetId == null) throw new ApiException(400, "Missing required parameter 'PetId' when calling DeletePet");
      

      var path = "/pet/{petId}";
      path = path.Replace("{format}", "json");
      path = path.Replace("{" + "petId" + "}", apiClient.ParameterToString(PetId));
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

      
       if (ApiKey != null) headerParams.Add("api_key", apiClient.ParameterToString(ApiKey)); // header parameter
      
      
      

      // authentication setting, if any
      String[] authSettings = new String[] { "petstore_auth" };

      // make the HTTP request
      IRestResponse response = (IRestResponse) apiClient.CallApi(path, Method.DELETE, queryParams, postBody, headerParams, formParams, fileParams, authSettings);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling DeletePet: " + response.Content, response.Content);
      }
      
      return;
    }
	
	 /// <summary>
    /// Deletes a pet 
    /// </summary>
    /// <param name="ApiKey"></param>/// <param name="PetId">Pet id to delete</param>
    /// <returns></returns>
    public async Task DeletePetAsync (string ApiKey, long? PetId) {

      
          // verify the required parameter 'PetId' is set
          if (PetId == null) throw new ApiException(400, "Missing required parameter 'PetId' when calling DeletePet");
      

      var path = "/pet/{petId}";
      path = path.Replace("{format}", "json");
      path = path.Replace("{" + "petId" + "}", apiClient.ParameterToString(PetId));
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

      
       if (ApiKey != null) headerParams.Add("api_key", apiClient.ParameterToString(ApiKey)); // header parameter
      
      
      

      // authentication setting, if any
      String[] authSettings = new String[] { "petstore_auth" };

      // make the HTTP request
      IRestResponse response = (IRestResponse) await apiClient.CallApiAsync(path, Method.DELETE, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling DeletePet: " + response.Content, response.Content);
      }
      
      return;
    }
    
    /// <summary>
    /// uploads an image 
    /// </summary>
    /// <param name="PetId">ID of pet to update</param>/// <param name="AdditionalMetadata">Additional data to pass to server</param>/// <param name="File">file to upload</param>
    /// <returns></returns>
    public void UploadFile (long? PetId, string AdditionalMetadata, string File) {

      
      // verify the required parameter 'PetId' is set
      if (PetId == null) throw new ApiException(400, "Missing required parameter 'PetId' when calling UploadFile");
      

      var path = "/pet/{petId}/uploadImage";
      path = path.Replace("{format}", "json");
      path = path.Replace("{" + "petId" + "}", apiClient.ParameterToString(PetId));
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

      
      
      if (AdditionalMetadata != null) formParams.Add("additionalMetadata", apiClient.ParameterToString(AdditionalMetadata)); // form parameter
      if (File != null) fileParams.Add("file", File);
      
      

      // authentication setting, if any
      String[] authSettings = new String[] { "petstore_auth" };

      // make the HTTP request
      IRestResponse response = (IRestResponse) apiClient.CallApi(path, Method.POST, queryParams, postBody, headerParams, formParams, fileParams, authSettings);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling UploadFile: " + response.Content, response.Content);
      }
      
      return;
    }
	
	 /// <summary>
    /// uploads an image 
    /// </summary>
    /// <param name="PetId">ID of pet to update</param>/// <param name="AdditionalMetadata">Additional data to pass to server</param>/// <param name="File">file to upload</param>
    /// <returns></returns>
    public async Task UploadFileAsync (long? PetId, string AdditionalMetadata, string File) {

      
          // verify the required parameter 'PetId' is set
          if (PetId == null) throw new ApiException(400, "Missing required parameter 'PetId' when calling UploadFile");
      

      var path = "/pet/{petId}/uploadImage";
      path = path.Replace("{format}", "json");
      path = path.Replace("{" + "petId" + "}", apiClient.ParameterToString(PetId));
      

      var queryParams = new Dictionary<String, String>();
      var headerParams = new Dictionary<String, String>();
      var formParams = new Dictionary<String, String>();
      var fileParams = new Dictionary<String, String>();
      String postBody = null;

      
      
      if (AdditionalMetadata != null) formParams.Add("additionalMetadata", apiClient.ParameterToString(AdditionalMetadata)); // form parameter
      if (File != null) fileParams.Add("file", File);
      
      

      // authentication setting, if any
      String[] authSettings = new String[] { "petstore_auth" };

      // make the HTTP request
      IRestResponse response = (IRestResponse) await apiClient.CallApiAsync(path, Method.POST, queryParams, postBody, headerParams, formParams, fileParams, authSettings);
      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling UploadFile: " + response.Content, response.Content);
      }
      
      return;
    }
    
  }  
  
}
