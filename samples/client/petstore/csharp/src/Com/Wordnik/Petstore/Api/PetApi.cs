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
      
      public void setBasePath(string basePath) {
        this.basePath = basePath;
      }
      
      public String getBasePath() {
        return basePath;
      }

      public Pet getPetById (string Petid) {
        // create path and map variables
        var path = "/pet.{format}/{petId}".Replace("{format}","json").Replace("{" + "Petid" + "}", apiInvoker.escapeString(Petid.ToString()));

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();

        // verify required params are set
        if (Petid == null ) {
           throw new ApiException(400, "missing required params");
        }
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
      public void addPet (Pet body) {
        // create path and map variables
        var path = "/pet.{format}".Replace("{format}","json");

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();

        // verify required params are set
        if (body == null ) {
           throw new ApiException(400, "missing required params");
        }
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
      public void updatePet (Pet body) {
        // create path and map variables
        var path = "/pet.{format}".Replace("{format}","json");

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();

        // verify required params are set
        if (body == null ) {
           throw new ApiException(400, "missing required params");
        }
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
      public List<Pet> findPetsByStatus (string Status) {
        // create path and map variables
        var path = "/pet.{format}/findByStatus".Replace("{format}","json");

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();

        // verify required params are set
        if (Status == null ) {
           throw new ApiException(400, "missing required params");
        }
        if (Status != null)
          queryParams.Add("Status", Status);
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
      public List<Pet> findPetsByTags (string Tags) {
        // create path and map variables
        var path = "/pet.{format}/findByTags".Replace("{format}","json");

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();

        // verify required params are set
        if (Tags == null ) {
           throw new ApiException(400, "missing required params");
        }
        if (Tags != null)
          queryParams.Add("Tags", Tags);
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
