using System;
using System.Collections.Generic;
using RestSharp;
using IO.Swagger.Client;
using IO.Swagger.Model;

namespace IO.Swagger.Api {
  
  public class PetApi {
    string basePath;
    public ApiClient apiClient {get; set;}

    public PetApi(String basePath = "http://petstore.swagger.io/v2")
    {
      this.basePath = basePath;
      this.apiClient = new ApiClient(basePath);
    }

    /// <summary>
    /// Create a new object 
    /// </summary>
    /// <param name="apiClient"> an instance of ApiClient
    /// <returns></returns>
    public PetApi(ApiClient apiClient = null) {
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
      

      // make the HTTP request
      IRestResponse response = (IRestResponse) apiClient.CallApi(path, Method.PUT, queryParams, postBody, headerParams, formParams, fileParams);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling UpdatePet: " + response.Content);
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
      

      // make the HTTP request
      IRestResponse response = (IRestResponse) apiClient.CallApi(path, Method.POST, queryParams, postBody, headerParams, formParams, fileParams);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling AddPet: " + response.Content);
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
      
      
      
      

      // make the HTTP request
      IRestResponse response = (IRestResponse) apiClient.CallApi(path, Method.GET, queryParams, postBody, headerParams, formParams, fileParams);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling FindPetsByStatus: " + response.Content);
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
      
      
      
      

      // make the HTTP request
      IRestResponse response = (IRestResponse) apiClient.CallApi(path, Method.GET, queryParams, postBody, headerParams, formParams, fileParams);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling FindPetsByTags: " + response.Content);
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

      
      
      
      

      // make the HTTP request
      IRestResponse response = (IRestResponse) apiClient.CallApi(path, Method.GET, queryParams, postBody, headerParams, formParams, fileParams);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling GetPetById: " + response.Content);
      }
      return (Pet) apiClient.Deserialize(response.Content, typeof(Pet));
    }
    
    /// <summary>
    /// Updates a pet in the store with form data 
    /// </summary>
    /// <param name="PetId">ID of pet that needs to be updated</param>
    /// <param name="Name">Updated name of the pet</param>
    /// <param name="Status">Updated status of the pet</param>
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
      
      

      // make the HTTP request
      IRestResponse response = (IRestResponse) apiClient.CallApi(path, Method.POST, queryParams, postBody, headerParams, formParams, fileParams);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling UpdatePetWithForm: " + response.Content);
      }
      
      return;
    }
    
    /// <summary>
    /// Deletes a pet 
    /// </summary>
    /// <param name="ApiKey"></param>
    /// <param name="PetId">Pet id to delete</param>
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
      
      
      

      // make the HTTP request
      IRestResponse response = (IRestResponse) apiClient.CallApi(path, Method.DELETE, queryParams, postBody, headerParams, formParams, fileParams);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling DeletePet: " + response.Content);
      }
      
      return;
    }
    
    /// <summary>
    /// uploads an image 
    /// </summary>
    /// <param name="PetId">ID of pet to update</param>
    /// <param name="AdditionalMetadata">Additional data to pass to server</param>
    /// <param name="File">file to upload</param>
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
      
      

      // make the HTTP request
      IRestResponse response = (IRestResponse) apiClient.CallApi(path, Method.POST, queryParams, postBody, headerParams, formParams, fileParams);

      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling UploadFile: " + response.Content);
      }
      
      return;
    }
    
  }
  
}
