using System;
using System.Collections.Generic;
using RestSharp;
using io.swagger.client;
using io.swagger.Model;

namespace io.swagger.Api {
  
  public class PetApi {
    string basePath;
    private readonly ApiInvoker apiInvoker = ApiInvoker.GetInstance();
    protected RestClient _client;

    public PetApi(String basePath = "http://petstore.swagger.io/v2")
    {
      this.basePath = basePath;
      _client = new RestClient(basePath);
    }

    public ApiInvoker getInvoker() {
      return apiInvoker;
    }

    // Sets the endpoint base url for the services being accessed
    public void setBasePath(string basePath) {
      this.basePath = basePath;
    }

    // Gets the endpoint base url for the services being accessed
    public String getBasePath() {
      return basePath;
    }

    

    /// <summary>
    /// Update an existing pet 
    /// </summary>
    /// <param name="Body">Pet object that needs to be added to the store</param>
    
    /// <returns></returns>
    public void  UpdatePet (Pet Body) {
      // create path and map variables
      var path = "/pet".Replace("{format}","json");

      var _request = new RestRequest("/pet", Method.PUT);

      

      // path (url segment) parameters
      _request.AddUrlSegment("format", "json"); // set format to json by default
      
      // query parameters, if any
      
      // header parameters, if any
      
      // form parameters, if any
      

      try {
        
        _client.Execute(_request);
        return;
      } catch (Exception ex) {
        if(ex != null) {
          return ;
        }
        else {
          throw ex;
        }
      }
    }
    

    /// <summary>
    /// Add a new pet to the store 
    /// </summary>
    /// <param name="Body">Pet object that needs to be added to the store</param>
    
    /// <returns></returns>
    public void  AddPet (Pet Body) {
      // create path and map variables
      var path = "/pet".Replace("{format}","json");

      var _request = new RestRequest("/pet", Method.POST);

      

      // path (url segment) parameters
      _request.AddUrlSegment("format", "json"); // set format to json by default
      
      // query parameters, if any
      
      // header parameters, if any
      
      // form parameters, if any
      

      try {
        
        _client.Execute(_request);
        return;
      } catch (Exception ex) {
        if(ex != null) {
          return ;
        }
        else {
          throw ex;
        }
      }
    }
    

    /// <summary>
    /// Finds Pets by status Multiple status values can be provided with comma seperated strings
    /// </summary>
    /// <param name="Status">Status values that need to be considered for filter</param>
    
    /// <returns></returns>
    public List<Pet>  FindPetsByStatus (List<string> Status) {
      // create path and map variables
      var path = "/pet/findByStatus".Replace("{format}","json");

      var _request = new RestRequest("/pet/findByStatus", Method.GET);

      

      // path (url segment) parameters
      _request.AddUrlSegment("format", "json"); // set format to json by default
      
      // query parameters, if any
       if (Status != null) _request.AddParameter("status", Status);
      // header parameters, if any
      
      // form parameters, if any
      

      try {
        IRestResponse response = _client.Execute(_request);
        return (List<Pet>) ApiInvoker.deserialize(response.Content, typeof(List<Pet>));
        //return ((object)response) as List<Pet>;
        
      } catch (Exception ex) {
        if(ex != null) {
          return null;
        }
        else {
          throw ex;
        }
      }
    }
    

    /// <summary>
    /// Finds Pets by tags Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
    /// </summary>
    /// <param name="Tags">Tags to filter by</param>
    
    /// <returns></returns>
    public List<Pet>  FindPetsByTags (List<string> Tags) {
      // create path and map variables
      var path = "/pet/findByTags".Replace("{format}","json");

      var _request = new RestRequest("/pet/findByTags", Method.GET);

      

      // path (url segment) parameters
      _request.AddUrlSegment("format", "json"); // set format to json by default
      
      // query parameters, if any
       if (Tags != null) _request.AddParameter("tags", Tags);
      // header parameters, if any
      
      // form parameters, if any
      

      try {
        IRestResponse response = _client.Execute(_request);
        return (List<Pet>) ApiInvoker.deserialize(response.Content, typeof(List<Pet>));
        //return ((object)response) as List<Pet>;
        
      } catch (Exception ex) {
        if(ex != null) {
          return null;
        }
        else {
          throw ex;
        }
      }
    }
    

    /// <summary>
    /// Find pet by ID Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
    /// </summary>
    /// <param name="PetId">ID of pet that needs to be fetched</param>
    
    /// <returns></returns>
    public Pet  GetPetById (long? PetId) {
      // create path and map variables
      var path = "/pet/{petId}".Replace("{format}","json").Replace("{" + "petId" + "}", apiInvoker.ParameterToString(PetId));

      var _request = new RestRequest("/pet/{petId}", Method.GET);

      

      // path (url segment) parameters
      _request.AddUrlSegment("format", "json"); // set format to json by default
      _request.AddUrlSegment("petId", apiInvoker.ParameterToString(PetId));
      // query parameters, if any
      
      // header parameters, if any
      
      // form parameters, if any
      

      try {
        IRestResponse response = _client.Execute(_request);
        return (Pet) ApiInvoker.deserialize(response.Content, typeof(Pet));
        //return ((object)response) as Pet;
        
      } catch (Exception ex) {
        if(ex != null) {
          return null;
        }
        else {
          throw ex;
        }
      }
    }
    

    /// <summary>
    /// Updates a pet in the store with form data 
    /// </summary>
    /// <param name="PetId">ID of pet that needs to be updated</param>
     /// <param name="Name">Updated name of the pet</param>
     /// <param name="Status">Updated status of the pet</param>
    
    /// <returns></returns>
    public void  UpdatePetWithForm (string PetId, string Name, string Status) {
      // create path and map variables
      var path = "/pet/{petId}".Replace("{format}","json").Replace("{" + "petId" + "}", apiInvoker.ParameterToString(PetId));

      var _request = new RestRequest("/pet/{petId}", Method.POST);

      

      // path (url segment) parameters
      _request.AddUrlSegment("format", "json"); // set format to json by default
      _request.AddUrlSegment("petId", apiInvoker.ParameterToString(PetId));
      // query parameters, if any
      
      // header parameters, if any
      
      // form parameters, if any
      if (Name != null) _request.AddFile("name", Name);
      if (Status != null) _request.AddFile("status", Status);
      

      try {
        
        _client.Execute(_request);
        return;
      } catch (Exception ex) {
        if(ex != null) {
          return ;
        }
        else {
          throw ex;
        }
      }
    }
    

    /// <summary>
    /// Deletes a pet 
    /// </summary>
    /// <param name="ApiKey"></param>
     /// <param name="PetId">Pet id to delete</param>
    
    /// <returns></returns>
    public void  DeletePet (string ApiKey, long? PetId) {
      // create path and map variables
      var path = "/pet/{petId}".Replace("{format}","json").Replace("{" + "petId" + "}", apiInvoker.ParameterToString(PetId));

      var _request = new RestRequest("/pet/{petId}", Method.DELETE);

      

      // path (url segment) parameters
      _request.AddUrlSegment("format", "json"); // set format to json by default
      _request.AddUrlSegment("petId", apiInvoker.ParameterToString(PetId));
      // query parameters, if any
      
      // header parameters, if any
       if (ApiKey != null) _request.AddHeader("api_key", ApiKey);
      // form parameters, if any
      

      try {
        
        _client.Execute(_request);
        return;
      } catch (Exception ex) {
        if(ex != null) {
          return ;
        }
        else {
          throw ex;
        }
      }
    }
    

    /// <summary>
    /// uploads an image 
    /// </summary>
    /// <param name="PetId">ID of pet to update</param>
     /// <param name="AdditionalMetadata">Additional data to pass to server</param>
     /// <param name="File">file to upload</param>
    
    /// <returns></returns>
    public void  UploadFile (long? PetId, string AdditionalMetadata, byte[] File) {
      // create path and map variables
      var path = "/pet/{petId}/uploadImage".Replace("{format}","json").Replace("{" + "petId" + "}", apiInvoker.ParameterToString(PetId));

      var _request = new RestRequest("/pet/{petId}/uploadImage", Method.POST);

      

      // path (url segment) parameters
      _request.AddUrlSegment("format", "json"); // set format to json by default
      _request.AddUrlSegment("petId", apiInvoker.ParameterToString(PetId));
      // query parameters, if any
      
      // header parameters, if any
      
      // form parameters, if any
      if (AdditionalMetadata != null) _request.AddFile("additionalMetadata", AdditionalMetadata);
      if (File != null) _request.AddParameter("file", File);
      

      try {
        
        _client.Execute(_request);
        return;
      } catch (Exception ex) {
        if(ex != null) {
          return ;
        }
        else {
          throw ex;
        }
      }
    }
    
  }
  
}
