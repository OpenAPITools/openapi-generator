  using System;
  using System.Collections.Generic;
  using io.swagger.client;
  using io.swagger.Model;
  
  
  
  
  

  namespace io.swagger.Api {
    
    public class PetApi {
      string basePath;
      private readonly ApiInvoker apiInvoker = ApiInvoker.GetInstance();

      public PetApi(String basePath = "http://petstore.swagger.io/v2")
      {
        this.basePath = basePath;
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
      /// <param name="body">Pet object that needs to be added to the store</param>
      
      /// <returns></returns>
      public void  updatePet (Pet body) {
        // create path and map variables
        var path = "/pet".Replace("{format}","json");

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();
        var formParams = new Dictionary<String, object>();

        

        

        

        

        try {
          if (typeof(void) == typeof(byte[])) {
            var response = apiInvoker.invokeBinaryAPI(basePath, path, "GET", queryParams, null, headerParams, formParams);
            return ;
          } else {
            var response = apiInvoker.invokeAPI(basePath, path, "PUT", queryParams, com.wordnik.swagger.codegen.CodegenParameter@7ea4461e, headerParams, formParams);
            if(response != null){
               return ;
            }
            else {
              return ;
            }
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
      public void  addPet (Pet body) {
        // create path and map variables
        var path = "/pet".Replace("{format}","json");

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();
        var formParams = new Dictionary<String, object>();

        

        

        

        

        try {
          if (typeof(void) == typeof(byte[])) {
            var response = apiInvoker.invokeBinaryAPI(basePath, path, "GET", queryParams, null, headerParams, formParams);
            return ;
          } else {
            var response = apiInvoker.invokeAPI(basePath, path, "POST", queryParams, com.wordnik.swagger.codegen.CodegenParameter@52f79c86, headerParams, formParams);
            if(response != null){
               return ;
            }
            else {
              return ;
            }
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
      public array<Pet>  findPetsByStatus (array<string> status) {
        // create path and map variables
        var path = "/pet/findByStatus".Replace("{format}","json");

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();
        var formParams = new Dictionary<String, object>();

        

        if (status != null){
          string paramStr = (status is DateTime) ? ((DateTime)(object)status).ToString("u") : Convert.ToString(status);
          queryParams.Add("status", paramStr);
		}
        

        

        

        try {
          if (typeof(array<Pet>) == typeof(byte[])) {
            var response = apiInvoker.invokeBinaryAPI(basePath, path, "GET", queryParams, null, headerParams, formParams);
            return ((object)response) as array<Pet>;
          } else {
            var response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, null, headerParams, formParams);
            if(response != null){
               return (array<Pet>) ApiInvoker.deserialize(response, typeof(array<Pet>));
            }
            else {
              return null;
            }
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
      public array<Pet>  findPetsByTags (array<string> tags) {
        // create path and map variables
        var path = "/pet/findByTags".Replace("{format}","json");

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();
        var formParams = new Dictionary<String, object>();

        

        if (tags != null){
          string paramStr = (tags is DateTime) ? ((DateTime)(object)tags).ToString("u") : Convert.ToString(tags);
          queryParams.Add("tags", paramStr);
		}
        

        

        

        try {
          if (typeof(array<Pet>) == typeof(byte[])) {
            var response = apiInvoker.invokeBinaryAPI(basePath, path, "GET", queryParams, null, headerParams, formParams);
            return ((object)response) as array<Pet>;
          } else {
            var response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, null, headerParams, formParams);
            if(response != null){
               return (array<Pet>) ApiInvoker.deserialize(response, typeof(array<Pet>));
            }
            else {
              return null;
            }
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
      /// Find pet by ID Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
      /// </summary>
      /// <param name="petId">ID of pet that needs to be fetched</param>
      
      /// <returns></returns>
      public Pet  getPetById (long? petId) {
        // create path and map variables
        var path = "/pet/{petId}".Replace("{format}","json").Replace("{" + "petId" + "}", apiInvoker.escapeString(petId.ToString()));

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();
        var formParams = new Dictionary<String, object>();

        

        

        

        

        try {
          if (typeof(Pet) == typeof(byte[])) {
            var response = apiInvoker.invokeBinaryAPI(basePath, path, "GET", queryParams, null, headerParams, formParams);
            return ((object)response) as Pet;
          } else {
            var response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, null, headerParams, formParams);
            if(response != null){
               return (Pet) ApiInvoker.deserialize(response, typeof(Pet));
            }
            else {
              return null;
            }
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
      public void  updatePetWithForm (string petId, string name, string status) {
        // create path and map variables
        var path = "/pet/{petId}".Replace("{format}","json").Replace("{" + "petId" + "}", apiInvoker.escapeString(petId.ToString()));

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();
        var formParams = new Dictionary<String, object>();

        

        

        

        if (name != null){
          if(name is byte[]) {
            formParams.Add("name", name);
          } else {
            string paramStr = (name is DateTime) ? ((DateTime)(object)name).ToString("u") : Convert.ToString(name);
            formParams.Add("name", paramStr);
          }
		}
        if (status != null){
          if(status is byte[]) {
            formParams.Add("status", status);
          } else {
            string paramStr = (status is DateTime) ? ((DateTime)(object)status).ToString("u") : Convert.ToString(status);
            formParams.Add("status", paramStr);
          }
		}
        

        try {
          if (typeof(void) == typeof(byte[])) {
            var response = apiInvoker.invokeBinaryAPI(basePath, path, "GET", queryParams, null, headerParams, formParams);
            return ;
          } else {
            var response = apiInvoker.invokeAPI(basePath, path, "POST", queryParams, null, headerParams, formParams);
            if(response != null){
               return ;
            }
            else {
              return ;
            }
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
      /// Deletes a pet 
      /// </summary>
      /// <param name="apiKey"></param>
       /// <param name="petId">Pet id to delete</param>
      
      /// <returns></returns>
      public void  deletePet (string apiKey, long? petId) {
        // create path and map variables
        var path = "/pet/{petId}".Replace("{format}","json").Replace("{" + "petId" + "}", apiInvoker.escapeString(petId.ToString()));

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();
        var formParams = new Dictionary<String, object>();

        

        

        headerParams.Add("apiKey", apiKey);
        

        

        try {
          if (typeof(void) == typeof(byte[])) {
            var response = apiInvoker.invokeBinaryAPI(basePath, path, "GET", queryParams, null, headerParams, formParams);
            return ;
          } else {
            var response = apiInvoker.invokeAPI(basePath, path, "DELETE", queryParams, null, headerParams, formParams);
            if(response != null){
               return ;
            }
            else {
              return ;
            }
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
      /// uploads an image 
      /// </summary>
      /// <param name="petId">ID of pet to update</param>
       /// <param name="additionalMetadata">Additional data to pass to server</param>
       /// <param name="file">file to upload</param>
      
      /// <returns></returns>
      public void  uploadFile (long? petId, string additionalMetadata, file file) {
        // create path and map variables
        var path = "/pet/{petId}/uploadImage".Replace("{format}","json").Replace("{" + "petId" + "}", apiInvoker.escapeString(petId.ToString()));

        // query params
        var queryParams = new Dictionary<String, String>();
        var headerParams = new Dictionary<String, String>();
        var formParams = new Dictionary<String, object>();

        

        

        

        if (additionalMetadata != null){
          if(additionalMetadata is byte[]) {
            formParams.Add("additionalMetadata", additionalMetadata);
          } else {
            string paramStr = (additionalMetadata is DateTime) ? ((DateTime)(object)additionalMetadata).ToString("u") : Convert.ToString(additionalMetadata);
            formParams.Add("additionalMetadata", paramStr);
          }
		}
        if (file != null){
          if(file is byte[]) {
            formParams.Add("file", file);
          } else {
            string paramStr = (file is DateTime) ? ((DateTime)(object)file).ToString("u") : Convert.ToString(file);
            formParams.Add("file", paramStr);
          }
		}
        

        try {
          if (typeof(void) == typeof(byte[])) {
            var response = apiInvoker.invokeBinaryAPI(basePath, path, "GET", queryParams, null, headerParams, formParams);
            return ;
          } else {
            var response = apiInvoker.invokeAPI(basePath, path, "POST", queryParams, null, headerParams, formParams);
            if(response != null){
               return ;
            }
            else {
              return ;
            }
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
      
    }
    
  }