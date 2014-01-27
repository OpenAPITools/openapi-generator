  using System;
  using System.Collections.Generic;
  using Com.Wordnik.Petstore;
  using Com.Wordnik.Petstore.Model;
  namespace Com.Wordnik.Petstore.Api {
    public class PetApi {
      string basePath = "http://petstore.swagger.wordnik.com/api";
      private readonly ApiInvoker apiInvoker = ApiInvoker.GetInstance();

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
      /// uploads an image 
      /// </summary>
      /// <param name="additionalMetadata">Additional data to pass to server</param>
      /// <param name="body">file to upload</param>
      /// <returns></returns>
      public void uploadFile (string additionalMetadata, File body) {
        // create path and map variables
        var path = "/pet/uploadImage".Replace("{format}","json");

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();

        string paramStr = null;
        try {
          var response = apiInvoker.invokeAPI(basePath, path, "POST", queryParams, body, headerParams);
          if(response != null){
             return ;
          }
          else {
            return ;
          }
        } catch (ApiException ex) {
          if(ex.ErrorCode == 404) {
          	return ;
          }
          else {
            throw ex;
          }
        }
      }
      /// <summary>
      /// Find pet by ID Returns a pet based on ID
      /// </summary>
      /// <param name="petId">ID of pet that needs to be fetched</param>
      /// <returns></returns>
      public Pet getPetById (long petId) {
        // create path and map variables
        var path = "/pet/{petId}".Replace("{format}","json").Replace("{" + "petId" + "}", apiInvoker.escapeString(petId.ToString()));

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();

        // verify required params are set
        if (petId == null ) {
           throw new ApiException(400, "missing required params");
        }
        string paramStr = null;
        try {
          var response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, null, headerParams);
          if(response != null){
             return (Pet) ApiInvoker.deserialize(response, typeof(Pet));
          }
          else {
            return null;
          }
        } catch (ApiException ex) {
          if(ex.ErrorCode == 404) {
          	return null;
          }
          else {
            throw ex;
          }
        }
      }
      /// <summary>
      /// Deletes a pet 
      /// </summary>
      /// <param name="petId">Pet id to delete</param>
      /// <returns></returns>
      public void deletePet (string petId) {
        // create path and map variables
        var path = "/pet/{petId}".Replace("{format}","json").Replace("{" + "petId" + "}", apiInvoker.escapeString(petId.ToString()));

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();

        // verify required params are set
        if (petId == null ) {
           throw new ApiException(400, "missing required params");
        }
        string paramStr = null;
        try {
          var response = apiInvoker.invokeAPI(basePath, path, "DELETE", queryParams, null, headerParams);
          if(response != null){
             return ;
          }
          else {
            return ;
          }
        } catch (ApiException ex) {
          if(ex.ErrorCode == 404) {
          	return ;
          }
          else {
            throw ex;
          }
        }
      }
      /// <summary>
      /// partial updates to a pet 
      /// </summary>
      /// <param name="petId">ID of pet that needs to be fetched</param>
      /// <param name="body">Pet object that needs to be added to the store</param>
      /// <returns></returns>
      public List<Pet> partialUpdate (string petId, Pet body) {
        // create path and map variables
        var path = "/pet/{petId}".Replace("{format}","json").Replace("{" + "petId" + "}", apiInvoker.escapeString(petId.ToString()));

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();

        // verify required params are set
        if (petId == null || body == null ) {
           throw new ApiException(400, "missing required params");
        }
        string paramStr = null;
        try {
          var response = apiInvoker.invokeAPI(basePath, path, "PATCH", queryParams, body, headerParams);
          if(response != null){
             return (List<Pet>) ApiInvoker.deserialize(response, typeof(List<Pet>));
          }
          else {
            return null;
          }
        } catch (ApiException ex) {
          if(ex.ErrorCode == 404) {
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
      /// <param name="petId">ID of pet that needs to be updated</param>
      /// <param name="name">Updated name of the pet</param>
      /// <param name="status">Updated status of the pet</param>
      /// <returns></returns>
      public void updatePetWithForm (string petId, string name, string status) {
        // create path and map variables
        var path = "/pet/{petId}".Replace("{format}","json").Replace("{" + "petId" + "}", apiInvoker.escapeString(petId.ToString()));

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();

        // verify required params are set
        if (petId == null ) {
           throw new ApiException(400, "missing required params");
        }
        string paramStr = null;
        try {
          var response = apiInvoker.invokeAPI(basePath, path, "POST", queryParams, null, headerParams);
          if(response != null){
             return ;
          }
          else {
            return ;
          }
        } catch (ApiException ex) {
          if(ex.ErrorCode == 404) {
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
      /// <param name="body">Pet object that needs to be added to the store</param>
      /// <returns></returns>
      public void addPet (Pet body) {
        // create path and map variables
        var path = "/pet".Replace("{format}","json");

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();

        // verify required params are set
        if (body == null ) {
           throw new ApiException(400, "missing required params");
        }
        string paramStr = null;
        try {
          var response = apiInvoker.invokeAPI(basePath, path, "POST", queryParams, body, headerParams);
          if(response != null){
             return ;
          }
          else {
            return ;
          }
        } catch (ApiException ex) {
          if(ex.ErrorCode == 404) {
          	return ;
          }
          else {
            throw ex;
          }
        }
      }
      /// <summary>
      /// Update an existing pet 
      /// </summary>
      /// <param name="body">Pet object that needs to be updated in the store</param>
      /// <returns></returns>
      public void updatePet (Pet body) {
        // create path and map variables
        var path = "/pet".Replace("{format}","json");

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();

        // verify required params are set
        if (body == null ) {
           throw new ApiException(400, "missing required params");
        }
        string paramStr = null;
        try {
          var response = apiInvoker.invokeAPI(basePath, path, "PUT", queryParams, body, headerParams);
          if(response != null){
             return ;
          }
          else {
            return ;
          }
        } catch (ApiException ex) {
          if(ex.ErrorCode == 404) {
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
      /// <param name="status">Status values that need to be considered for filter</param>
      /// <returns></returns>
      public List<Pet> findPetsByStatus (string status) {
        // create path and map variables
        var path = "/pet/findByStatus".Replace("{format}","json");

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();

        // verify required params are set
        if (status == null ) {
           throw new ApiException(400, "missing required params");
        }
        string paramStr = null;
        if (status != null){
          paramStr = (status != null && status is DateTime) ? ((DateTime)(object)status).ToString("u") : Convert.ToString(status);
          queryParams.Add("status", paramStr);
		}
        try {
          var response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, null, headerParams);
          if(response != null){
             return (List<Pet>) ApiInvoker.deserialize(response, typeof(List<Pet>));
          }
          else {
            return null;
          }
        } catch (ApiException ex) {
          if(ex.ErrorCode == 404) {
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
      /// <param name="tags">Tags to filter by</param>
      /// <returns></returns>
      public List<Pet> findPetsByTags (string tags) {
        // create path and map variables
        var path = "/pet/findByTags".Replace("{format}","json");

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();

        // verify required params are set
        if (tags == null ) {
           throw new ApiException(400, "missing required params");
        }
        string paramStr = null;
        if (tags != null){
          paramStr = (tags != null && tags is DateTime) ? ((DateTime)(object)tags).ToString("u") : Convert.ToString(tags);
          queryParams.Add("tags", paramStr);
		}
        try {
          var response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, null, headerParams);
          if(response != null){
             return (List<Pet>) ApiInvoker.deserialize(response, typeof(List<Pet>));
          }
          else {
            return null;
          }
        } catch (ApiException ex) {
          if(ex.ErrorCode == 404) {
          	return null;
          }
          else {
            throw ex;
          }
        }
      }
      }
    }
