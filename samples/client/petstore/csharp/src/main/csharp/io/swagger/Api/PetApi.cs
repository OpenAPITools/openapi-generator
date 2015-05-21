using System;
using System.Collections.Generic;
using RestSharp;
using IO.Swagger.Client;
using IO.Swagger.Model;

namespace IO.Swagger.Api {
  
  public class PetApi {
    string basePath;
    protected RestClient restClient;

    public PetApi(String basePath = "http://petstore.swagger.io/v2")
    {
      this.basePath = basePath;
      this.restClient = new RestClient(basePath);
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

      var _request = new RestRequest("/pet", Method.PUT);

      

      // add default header, if any
      foreach(KeyValuePair<string, string> defaultHeader in ApiInvoker.GetDefaultHeader())
      {
        _request.AddHeader(defaultHeader.Key, defaultHeader.Value);
      }

      _request.AddUrlSegment("format", "json"); // set format to json by default
      
      
      
      
      _request.AddParameter("application/json", ApiInvoker.Serialize(Body), ParameterType.RequestBody); // http body (model) parameter
      

      // make the HTTP request
      IRestResponse response = restClient.Execute(_request);
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

      var _request = new RestRequest("/pet", Method.POST);

      

      // add default header, if any
      foreach(KeyValuePair<string, string> defaultHeader in ApiInvoker.GetDefaultHeader())
      {
        _request.AddHeader(defaultHeader.Key, defaultHeader.Value);
      }

      _request.AddUrlSegment("format", "json"); // set format to json by default
      
      
      
      
      _request.AddParameter("application/json", ApiInvoker.Serialize(Body), ParameterType.RequestBody); // http body (model) parameter
      

      // make the HTTP request
      IRestResponse response = restClient.Execute(_request);
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

      var _request = new RestRequest("/pet/findByStatus", Method.GET);

      

      // add default header, if any
      foreach(KeyValuePair<string, string> defaultHeader in ApiInvoker.GetDefaultHeader())
      {
        _request.AddHeader(defaultHeader.Key, defaultHeader.Value);
      }

      _request.AddUrlSegment("format", "json"); // set format to json by default
      
       if (Status != null) _request.AddParameter("status", ApiInvoker.ParameterToString(Status)); // query parameter
      
      
      
      

      // make the HTTP request
      IRestResponse response = restClient.Execute(_request);
      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling FindPetsByStatus: " + response.Content);
      }
      return (List<Pet>) ApiInvoker.Deserialize(response.Content, typeof(List<Pet>));
    }
    
    
    /// <summary>
    /// Finds Pets by tags Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
    /// </summary>
    /// <param name="Tags">Tags to filter by</param>
    /// <returns>List<Pet></returns>
    public List<Pet> FindPetsByTags (List<string> Tags) {

      var _request = new RestRequest("/pet/findByTags", Method.GET);

      

      // add default header, if any
      foreach(KeyValuePair<string, string> defaultHeader in ApiInvoker.GetDefaultHeader())
      {
        _request.AddHeader(defaultHeader.Key, defaultHeader.Value);
      }

      _request.AddUrlSegment("format", "json"); // set format to json by default
      
       if (Tags != null) _request.AddParameter("tags", ApiInvoker.ParameterToString(Tags)); // query parameter
      
      
      
      

      // make the HTTP request
      IRestResponse response = restClient.Execute(_request);
      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling FindPetsByTags: " + response.Content);
      }
      return (List<Pet>) ApiInvoker.Deserialize(response.Content, typeof(List<Pet>));
    }
    
    
    /// <summary>
    /// Find pet by ID Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
    /// </summary>
    /// <param name="PetId">ID of pet that needs to be fetched</param>
    /// <returns>Pet</returns>
    public Pet GetPetById (long? PetId) {

      var _request = new RestRequest("/pet/{petId}", Method.GET);

      
      // verify the required parameter 'PetId' is set
      if (PetId == null) throw new ApiException(400, "Missing required parameter 'PetId' when calling GetPetById");
      

      // add default header, if any
      foreach(KeyValuePair<string, string> defaultHeader in ApiInvoker.GetDefaultHeader())
      {
        _request.AddHeader(defaultHeader.Key, defaultHeader.Value);
      }

      _request.AddUrlSegment("format", "json"); // set format to json by default
      _request.AddUrlSegment("petId", ApiInvoker.ParameterToString(PetId)); // path (url segment) parameter
      
      
      
      
      

      // make the HTTP request
      IRestResponse response = restClient.Execute(_request);
      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling GetPetById: " + response.Content);
      }
      return (Pet) ApiInvoker.Deserialize(response.Content, typeof(Pet));
    }
    
    
    /// <summary>
    /// Updates a pet in the store with form data 
    /// </summary>
    /// <param name="PetId">ID of pet that needs to be updated</param>
    /// <param name="Name">Updated name of the pet</param>
    /// <param name="Status">Updated status of the pet</param>
    /// <returns></returns>
    public void UpdatePetWithForm (string PetId, string Name, string Status) {

      var _request = new RestRequest("/pet/{petId}", Method.POST);

      
      // verify the required parameter 'PetId' is set
      if (PetId == null) throw new ApiException(400, "Missing required parameter 'PetId' when calling UpdatePetWithForm");
      

      // add default header, if any
      foreach(KeyValuePair<string, string> defaultHeader in ApiInvoker.GetDefaultHeader())
      {
        _request.AddHeader(defaultHeader.Key, defaultHeader.Value);
      }

      _request.AddUrlSegment("format", "json"); // set format to json by default
      _request.AddUrlSegment("petId", ApiInvoker.ParameterToString(PetId)); // path (url segment) parameter
      
      
      
      if (Name != null) _request.AddParameter("name", ApiInvoker.ParameterToString(Name)); // form parameter
      if (Status != null) _request.AddParameter("status", ApiInvoker.ParameterToString(Status)); // form parameter
      
      

      // make the HTTP request
      IRestResponse response = restClient.Execute(_request);
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

      var _request = new RestRequest("/pet/{petId}", Method.DELETE);

      
      // verify the required parameter 'PetId' is set
      if (PetId == null) throw new ApiException(400, "Missing required parameter 'PetId' when calling DeletePet");
      

      // add default header, if any
      foreach(KeyValuePair<string, string> defaultHeader in ApiInvoker.GetDefaultHeader())
      {
        _request.AddHeader(defaultHeader.Key, defaultHeader.Value);
      }

      _request.AddUrlSegment("format", "json"); // set format to json by default
      _request.AddUrlSegment("petId", ApiInvoker.ParameterToString(PetId)); // path (url segment) parameter
      
      
       if (ApiKey != null) _request.AddHeader("api_key", ApiInvoker.ParameterToString(ApiKey)); // header parameter
      
      
      

      // make the HTTP request
      IRestResponse response = restClient.Execute(_request);
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

      var _request = new RestRequest("/pet/{petId}/uploadImage", Method.POST);

      
      // verify the required parameter 'PetId' is set
      if (PetId == null) throw new ApiException(400, "Missing required parameter 'PetId' when calling UploadFile");
      

      // add default header, if any
      foreach(KeyValuePair<string, string> defaultHeader in ApiInvoker.GetDefaultHeader())
      {
        _request.AddHeader(defaultHeader.Key, defaultHeader.Value);
      }

      _request.AddUrlSegment("format", "json"); // set format to json by default
      _request.AddUrlSegment("petId", ApiInvoker.ParameterToString(PetId)); // path (url segment) parameter
      
      
      
      if (AdditionalMetadata != null) _request.AddParameter("additionalMetadata", ApiInvoker.ParameterToString(AdditionalMetadata)); // form parameter
      if (File != null) _request.AddFile("file", File);
      
      

      // make the HTTP request
      IRestResponse response = restClient.Execute(_request);
      if (((int)response.StatusCode) >= 400) {
        throw new ApiException ((int)response.StatusCode, "Error calling UploadFile: " + response.Content);
      }
      
      return;
    }
    
  }
  
}
