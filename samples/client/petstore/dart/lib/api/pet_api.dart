part of api;



class PetApi {
  String basePath = "http://petstore.swagger.io/v2";
  ApiClient apiClient = ApiClient.defaultApiClient;

  PetApi([ApiClient apiClient]) {
    if (apiClient != null) {
      this.apiClient = apiClient;
    }
  }

  
  /// Update an existing pet
  ///
  /// 
  Future updatePet(Pet body) {
    Object postBody = body;
    

    // create path and map variables
    String path = "/pet".replaceAll("{format}","json");

    // query params
    Map<String, String> queryParams = {};
    Map<String, String> headerParams = {};
    Map<String, String> formParams = {};
    
    

    List<String> contentTypes = ["application/json","application/xml"];

    String contentType = contentTypes.length > 0 ? contentTypes[0] : "application/json";
    List<String> authNames = ["petstore_auth"];

    if(contentType.startsWith("multipart/form-data")) {
      bool hasFields = false;
      MultipartRequest mp = new MultipartRequest(null, null);
      
      if(hasFields)
        postBody = mp;
    }
    else {
      
    }

    return apiClient.invokeAPI(basePath, path, 'PUT', queryParams, postBody, headerParams, formParams, contentType, authNames).then((response) {
      if(response.statusCode >= 400) {
        throw new ApiException(response.statusCode, response.body);
      }
      else if(response.body != null){
        return ;
      }
      else {
        return ;
      }
    });
  }
  
  /// Add a new pet to the store
  ///
  /// 
  Future addPet(Pet body) {
    Object postBody = body;
    

    // create path and map variables
    String path = "/pet".replaceAll("{format}","json");

    // query params
    Map<String, String> queryParams = {};
    Map<String, String> headerParams = {};
    Map<String, String> formParams = {};
    
    

    List<String> contentTypes = ["application/json","application/xml"];

    String contentType = contentTypes.length > 0 ? contentTypes[0] : "application/json";
    List<String> authNames = ["petstore_auth"];

    if(contentType.startsWith("multipart/form-data")) {
      bool hasFields = false;
      MultipartRequest mp = new MultipartRequest(null, null);
      
      if(hasFields)
        postBody = mp;
    }
    else {
      
    }

    return apiClient.invokeAPI(basePath, path, 'POST', queryParams, postBody, headerParams, formParams, contentType, authNames).then((response) {
      if(response.statusCode >= 400) {
        throw new ApiException(response.statusCode, response.body);
      }
      else if(response.body != null){
        return ;
      }
      else {
        return ;
      }
    });
  }
  
  /// Finds Pets by status
  ///
  /// Multiple status values can be provided with comma seperated strings
  Future<List<Pet>> findPetsByStatus(List<String> status) {
    Object postBody = null;
    

    // create path and map variables
    String path = "/pet/findByStatus".replaceAll("{format}","json");

    // query params
    Map<String, String> queryParams = {};
    Map<String, String> headerParams = {};
    Map<String, String> formParams = {};
    if("null" != status)
      queryParams["status"] = status is List ? status.join(',') : status;
    
    

    List<String> contentTypes = [];

    String contentType = contentTypes.length > 0 ? contentTypes[0] : "application/json";
    List<String> authNames = ["petstore_auth"];

    if(contentType.startsWith("multipart/form-data")) {
      bool hasFields = false;
      MultipartRequest mp = new MultipartRequest(null, null);
      
      if(hasFields)
        postBody = mp;
    }
    else {
      
    }

    return apiClient.invokeAPI(basePath, path, 'GET', queryParams, postBody, headerParams, formParams, contentType, authNames).then((response) {
      if(response.statusCode >= 400) {
        throw new ApiException(response.statusCode, response.body);
      }
      else if(response.body != null){
        return ApiClient.deserialize(response.body, Pet);
      }
      else {
        return null;
      }
    });
  }
  
  /// Finds Pets by tags
  ///
  /// Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
  Future<List<Pet>> findPetsByTags(List<String> tags) {
    Object postBody = null;
    

    // create path and map variables
    String path = "/pet/findByTags".replaceAll("{format}","json");

    // query params
    Map<String, String> queryParams = {};
    Map<String, String> headerParams = {};
    Map<String, String> formParams = {};
    if("null" != tags)
      queryParams["tags"] = tags is List ? tags.join(',') : tags;
    
    

    List<String> contentTypes = [];

    String contentType = contentTypes.length > 0 ? contentTypes[0] : "application/json";
    List<String> authNames = ["petstore_auth"];

    if(contentType.startsWith("multipart/form-data")) {
      bool hasFields = false;
      MultipartRequest mp = new MultipartRequest(null, null);
      
      if(hasFields)
        postBody = mp;
    }
    else {
      
    }

    return apiClient.invokeAPI(basePath, path, 'GET', queryParams, postBody, headerParams, formParams, contentType, authNames).then((response) {
      if(response.statusCode >= 400) {
        throw new ApiException(response.statusCode, response.body);
      }
      else if(response.body != null){
        return ApiClient.deserialize(response.body, Pet);
      }
      else {
        return null;
      }
    });
  }
  
  /// Find pet by ID
  ///
  /// Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
  Future<Pet> getPetById(int petId) {
    Object postBody = null;
    

    // create path and map variables
    String path = "/pet/{petId}".replaceAll("{format}","json").replaceAll("{" + "petId" + "}", petId.toString());

    // query params
    Map<String, String> queryParams = {};
    Map<String, String> headerParams = {};
    Map<String, String> formParams = {};
    
    

    List<String> contentTypes = [];

    String contentType = contentTypes.length > 0 ? contentTypes[0] : "application/json";
    List<String> authNames = ["petstore_auth", "api_key"];

    if(contentType.startsWith("multipart/form-data")) {
      bool hasFields = false;
      MultipartRequest mp = new MultipartRequest(null, null);
      
      if(hasFields)
        postBody = mp;
    }
    else {
      
    }

    return apiClient.invokeAPI(basePath, path, 'GET', queryParams, postBody, headerParams, formParams, contentType, authNames).then((response) {
      if(response.statusCode >= 400) {
        throw new ApiException(response.statusCode, response.body);
      }
      else if(response.body != null){
        return ApiClient.deserialize(response.body, Pet);
      }
      else {
        return null;
      }
    });
  }
  
  /// Updates a pet in the store with form data
  ///
  /// 
  Future updatePetWithForm(String petId, String name, String status) {
    Object postBody = null;
    

    // create path and map variables
    String path = "/pet/{petId}".replaceAll("{format}","json").replaceAll("{" + "petId" + "}", petId.toString());

    // query params
    Map<String, String> queryParams = {};
    Map<String, String> headerParams = {};
    Map<String, String> formParams = {};
    
    

    List<String> contentTypes = ["application/x-www-form-urlencoded"];

    String contentType = contentTypes.length > 0 ? contentTypes[0] : "application/json";
    List<String> authNames = ["petstore_auth"];

    if(contentType.startsWith("multipart/form-data")) {
      bool hasFields = false;
      MultipartRequest mp = new MultipartRequest(null, null);
      
      if (name != null) {
        hasFields = true;
        mp.fields['name'] = apiClient.parameterToString(name);
      }
      
      if (status != null) {
        hasFields = true;
        mp.fields['status'] = apiClient.parameterToString(status);
      }
      
      if(hasFields)
        postBody = mp;
    }
    else {
      if (name != null)
        formParams['name'] = apiClient.parameterToString(name);
      if (status != null)
        formParams['status'] = apiClient.parameterToString(status);
      
    }

    return apiClient.invokeAPI(basePath, path, 'POST', queryParams, postBody, headerParams, formParams, contentType, authNames).then((response) {
      if(response.statusCode >= 400) {
        throw new ApiException(response.statusCode, response.body);
      }
      else if(response.body != null){
        return ;
      }
      else {
        return ;
      }
    });
  }
  
  /// Deletes a pet
  ///
  /// 
  Future deletePet(int petId, String apiKey) {
    Object postBody = null;
    

    // create path and map variables
    String path = "/pet/{petId}".replaceAll("{format}","json").replaceAll("{" + "petId" + "}", petId.toString());

    // query params
    Map<String, String> queryParams = {};
    Map<String, String> headerParams = {};
    Map<String, String> formParams = {};
    
    headerParams["api_key"] = apiKey;
    

    List<String> contentTypes = [];

    String contentType = contentTypes.length > 0 ? contentTypes[0] : "application/json";
    List<String> authNames = ["petstore_auth"];

    if(contentType.startsWith("multipart/form-data")) {
      bool hasFields = false;
      MultipartRequest mp = new MultipartRequest(null, null);
      
      if(hasFields)
        postBody = mp;
    }
    else {
      
    }

    return apiClient.invokeAPI(basePath, path, 'DELETE', queryParams, postBody, headerParams, formParams, contentType, authNames).then((response) {
      if(response.statusCode >= 400) {
        throw new ApiException(response.statusCode, response.body);
      }
      else if(response.body != null){
        return ;
      }
      else {
        return ;
      }
    });
  }
  
  /// uploads an image
  ///
  /// 
  Future uploadFile(int petId, String additionalMetadata, MultipartFile file) {
    Object postBody = null;
    

    // create path and map variables
    String path = "/pet/{petId}/uploadImage".replaceAll("{format}","json").replaceAll("{" + "petId" + "}", petId.toString());

    // query params
    Map<String, String> queryParams = {};
    Map<String, String> headerParams = {};
    Map<String, String> formParams = {};
    
    

    List<String> contentTypes = ["multipart/form-data"];

    String contentType = contentTypes.length > 0 ? contentTypes[0] : "application/json";
    List<String> authNames = ["petstore_auth"];

    if(contentType.startsWith("multipart/form-data")) {
      bool hasFields = false;
      MultipartRequest mp = new MultipartRequest(null, null);
      
      if (additionalMetadata != null) {
        hasFields = true;
        mp.fields['additionalMetadata'] = apiClient.parameterToString(additionalMetadata);
      }
      
      if (file != null) {
        hasFields = true;
        mp.fields['file'] = file.field;
        mp.files.add(file);
      }
      
      if(hasFields)
        postBody = mp;
    }
    else {
      if (additionalMetadata != null)
        formParams['additionalMetadata'] = apiClient.parameterToString(additionalMetadata);
      
      
    }

    return apiClient.invokeAPI(basePath, path, 'POST', queryParams, postBody, headerParams, formParams, contentType, authNames).then((response) {
      if(response.statusCode >= 400) {
        throw new ApiException(response.statusCode, response.body);
      }
      else if(response.body != null){
        return ;
      }
      else {
        return ;
      }
    });
  }
  
}

