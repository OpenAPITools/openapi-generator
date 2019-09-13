part of openapi.api;



class PetApi {
  final ApiClient apiClient;

  PetApi([ApiClient apiClient]) : apiClient = apiClient ?? defaultApiClient;

  /// Add a new pet to the store with HTTP info returned
  ///
  /// 
  Future addPetWithHttpInfo(Pet body) async {
    Object postBody = body;

    // verify required params are set
    if(body == null) {
     throw ApiException(400, "Missing required param: body");
    }

    // create path and map variables
    String path = "/pet".replaceAll("{format}","json");

    // query params
    List<QueryParam> queryParams = [];
    Map<String, String> headerParams = {};
    Map<String, String> formParams = {};

    List<String> contentTypes = ["application/json","application/xml"];

    String contentType = contentTypes.isNotEmpty ? contentTypes[0] : "application/json";
    List<String> authNames = ["petstore_auth"];

    if(contentType.startsWith("multipart/form-data")) {
      bool hasFields = false;
      MultipartRequest mp = MultipartRequest(null, null);
      if(hasFields)
        postBody = mp;
    }
    else {
    }

    var response = await apiClient.invokeAPI(path,
                                             'POST',
                                             queryParams,
                                             postBody,
                                             headerParams,
                                             formParams,
                                             contentType,
                                             authNames);
    return response;
  }

  /// Add a new pet to the store
  ///
  /// 
  Future addPet(Pet body) async {
    Response response = await addPetWithHttpInfo(body);
    if(response.statusCode >= 400) {
      throw ApiException(response.statusCode, _decodeBodyBytes(response));
    } else if(response.body != null) {
    } else {
      return;
    }
  }

  /// Deletes a pet with HTTP info returned
  ///
  /// 
  Future deletePetWithHttpInfo(int petId, { String apiKey }) async {
    Object postBody;

    // verify required params are set
    if(petId == null) {
     throw ApiException(400, "Missing required param: petId");
    }

    // create path and map variables
    String path = "/pet/{petId}".replaceAll("{format}","json").replaceAll("{" + "petId" + "}", petId.toString());

    // query params
    List<QueryParam> queryParams = [];
    Map<String, String> headerParams = {};
    Map<String, String> formParams = {};
    headerParams["api_key"] = apiKey;

    List<String> contentTypes = [];

    String contentType = contentTypes.isNotEmpty ? contentTypes[0] : "application/json";
    List<String> authNames = ["petstore_auth"];

    if(contentType.startsWith("multipart/form-data")) {
      bool hasFields = false;
      MultipartRequest mp = MultipartRequest(null, null);
      if(hasFields)
        postBody = mp;
    }
    else {
    }

    var response = await apiClient.invokeAPI(path,
                                             'DELETE',
                                             queryParams,
                                             postBody,
                                             headerParams,
                                             formParams,
                                             contentType,
                                             authNames);
    return response;
  }

  /// Deletes a pet
  ///
  /// 
  Future deletePet(int petId, { String apiKey }) async {
    Response response = await deletePetWithHttpInfo(petId,  apiKey: apiKey );
    if(response.statusCode >= 400) {
      throw ApiException(response.statusCode, _decodeBodyBytes(response));
    } else if(response.body != null) {
    } else {
      return;
    }
  }

  /// Finds Pets by status with HTTP info returned
  ///
  /// Multiple status values can be provided with comma separated strings
  Future<Response> findPetsByStatusWithHttpInfo(List<String> status) async {
    Object postBody;

    // verify required params are set
    if(status == null) {
     throw ApiException(400, "Missing required param: status");
    }

    // create path and map variables
    String path = "/pet/findByStatus".replaceAll("{format}","json");

    // query params
    List<QueryParam> queryParams = [];
    Map<String, String> headerParams = {};
    Map<String, String> formParams = {};
      queryParams.addAll(_convertParametersForCollectionFormat("csv", "status", status));

    List<String> contentTypes = [];

    String contentType = contentTypes.isNotEmpty ? contentTypes[0] : "application/json";
    List<String> authNames = ["petstore_auth"];

    if(contentType.startsWith("multipart/form-data")) {
      bool hasFields = false;
      MultipartRequest mp = MultipartRequest(null, null);
      if(hasFields)
        postBody = mp;
    }
    else {
    }

    var response = await apiClient.invokeAPI(path,
                                             'GET',
                                             queryParams,
                                             postBody,
                                             headerParams,
                                             formParams,
                                             contentType,
                                             authNames);
    return response;
  }

  /// Finds Pets by status
  ///
  /// Multiple status values can be provided with comma separated strings
  Future<List<Pet>> findPetsByStatus(List<String> status) async {
    Response response = await findPetsByStatusWithHttpInfo(status);
    if(response.statusCode >= 400) {
      throw ApiException(response.statusCode, _decodeBodyBytes(response));
    } else if(response.body != null) {
      return (apiClient.deserialize(_decodeBodyBytes(response), 'List<Pet>') as List).map((item) => item as Pet).toList();
    } else {
      return null;
    }
  }

  /// Finds Pets by tags with HTTP info returned
  ///
  /// Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
  Future<Response> findPetsByTagsWithHttpInfo(List<String> tags) async {
    Object postBody;

    // verify required params are set
    if(tags == null) {
     throw ApiException(400, "Missing required param: tags");
    }

    // create path and map variables
    String path = "/pet/findByTags".replaceAll("{format}","json");

    // query params
    List<QueryParam> queryParams = [];
    Map<String, String> headerParams = {};
    Map<String, String> formParams = {};
      queryParams.addAll(_convertParametersForCollectionFormat("csv", "tags", tags));

    List<String> contentTypes = [];

    String contentType = contentTypes.isNotEmpty ? contentTypes[0] : "application/json";
    List<String> authNames = ["petstore_auth"];

    if(contentType.startsWith("multipart/form-data")) {
      bool hasFields = false;
      MultipartRequest mp = MultipartRequest(null, null);
      if(hasFields)
        postBody = mp;
    }
    else {
    }

    var response = await apiClient.invokeAPI(path,
                                             'GET',
                                             queryParams,
                                             postBody,
                                             headerParams,
                                             formParams,
                                             contentType,
                                             authNames);
    return response;
  }

  /// Finds Pets by tags
  ///
  /// Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
  Future<List<Pet>> findPetsByTags(List<String> tags) async {
    Response response = await findPetsByTagsWithHttpInfo(tags);
    if(response.statusCode >= 400) {
      throw ApiException(response.statusCode, _decodeBodyBytes(response));
    } else if(response.body != null) {
      return (apiClient.deserialize(_decodeBodyBytes(response), 'List<Pet>') as List).map((item) => item as Pet).toList();
    } else {
      return null;
    }
  }

  /// Find pet by ID with HTTP info returned
  ///
  /// Returns a single pet
  Future<Response> getPetByIdWithHttpInfo(int petId) async {
    Object postBody;

    // verify required params are set
    if(petId == null) {
     throw ApiException(400, "Missing required param: petId");
    }

    // create path and map variables
    String path = "/pet/{petId}".replaceAll("{format}","json").replaceAll("{" + "petId" + "}", petId.toString());

    // query params
    List<QueryParam> queryParams = [];
    Map<String, String> headerParams = {};
    Map<String, String> formParams = {};

    List<String> contentTypes = [];

    String contentType = contentTypes.isNotEmpty ? contentTypes[0] : "application/json";
    List<String> authNames = ["api_key"];

    if(contentType.startsWith("multipart/form-data")) {
      bool hasFields = false;
      MultipartRequest mp = MultipartRequest(null, null);
      if(hasFields)
        postBody = mp;
    }
    else {
    }

    var response = await apiClient.invokeAPI(path,
                                             'GET',
                                             queryParams,
                                             postBody,
                                             headerParams,
                                             formParams,
                                             contentType,
                                             authNames);
    return response;
  }

  /// Find pet by ID
  ///
  /// Returns a single pet
  Future<Pet> getPetById(int petId) async {
    Response response = await getPetByIdWithHttpInfo(petId);
    if(response.statusCode >= 400) {
      throw ApiException(response.statusCode, _decodeBodyBytes(response));
    } else if(response.body != null) {
      return apiClient.deserialize(_decodeBodyBytes(response), 'Pet') as Pet;
    } else {
      return null;
    }
  }

  /// Update an existing pet with HTTP info returned
  ///
  /// 
  Future updatePetWithHttpInfo(Pet body) async {
    Object postBody = body;

    // verify required params are set
    if(body == null) {
     throw ApiException(400, "Missing required param: body");
    }

    // create path and map variables
    String path = "/pet".replaceAll("{format}","json");

    // query params
    List<QueryParam> queryParams = [];
    Map<String, String> headerParams = {};
    Map<String, String> formParams = {};

    List<String> contentTypes = ["application/json","application/xml"];

    String contentType = contentTypes.isNotEmpty ? contentTypes[0] : "application/json";
    List<String> authNames = ["petstore_auth"];

    if(contentType.startsWith("multipart/form-data")) {
      bool hasFields = false;
      MultipartRequest mp = MultipartRequest(null, null);
      if(hasFields)
        postBody = mp;
    }
    else {
    }

    var response = await apiClient.invokeAPI(path,
                                             'PUT',
                                             queryParams,
                                             postBody,
                                             headerParams,
                                             formParams,
                                             contentType,
                                             authNames);
    return response;
  }

  /// Update an existing pet
  ///
  /// 
  Future updatePet(Pet body) async {
    Response response = await updatePetWithHttpInfo(body);
    if(response.statusCode >= 400) {
      throw ApiException(response.statusCode, _decodeBodyBytes(response));
    } else if(response.body != null) {
    } else {
      return;
    }
  }

  /// Updates a pet in the store with form data with HTTP info returned
  ///
  /// 
  Future updatePetWithFormWithHttpInfo(int petId, { String name, String status }) async {
    Object postBody;

    // verify required params are set
    if(petId == null) {
     throw ApiException(400, "Missing required param: petId");
    }

    // create path and map variables
    String path = "/pet/{petId}".replaceAll("{format}","json").replaceAll("{" + "petId" + "}", petId.toString());

    // query params
    List<QueryParam> queryParams = [];
    Map<String, String> headerParams = {};
    Map<String, String> formParams = {};

    List<String> contentTypes = ["application/x-www-form-urlencoded"];

    String contentType = contentTypes.isNotEmpty ? contentTypes[0] : "application/json";
    List<String> authNames = ["petstore_auth"];

    if(contentType.startsWith("multipart/form-data")) {
      bool hasFields = false;
      MultipartRequest mp = MultipartRequest(null, null);
      if (name != null) {
        hasFields = true;
        mp.fields['name'] = parameterToString(name);
      }
      if (status != null) {
        hasFields = true;
        mp.fields['status'] = parameterToString(status);
      }
      if(hasFields)
        postBody = mp;
    }
    else {
      if (name != null)
        formParams['name'] = parameterToString(name);
      if (status != null)
        formParams['status'] = parameterToString(status);
    }

    var response = await apiClient.invokeAPI(path,
                                             'POST',
                                             queryParams,
                                             postBody,
                                             headerParams,
                                             formParams,
                                             contentType,
                                             authNames);
    return response;
  }

  /// Updates a pet in the store with form data
  ///
  /// 
  Future updatePetWithForm(int petId, { String name, String status }) async {
    Response response = await updatePetWithFormWithHttpInfo(petId,  name: name, status: status );
    if(response.statusCode >= 400) {
      throw ApiException(response.statusCode, _decodeBodyBytes(response));
    } else if(response.body != null) {
    } else {
      return;
    }
  }

  /// uploads an image with HTTP info returned
  ///
  /// 
  Future<Response> uploadFileWithHttpInfo(int petId, { String additionalMetadata, MultipartFile file }) async {
    Object postBody;

    // verify required params are set
    if(petId == null) {
     throw ApiException(400, "Missing required param: petId");
    }

    // create path and map variables
    String path = "/pet/{petId}/uploadImage".replaceAll("{format}","json").replaceAll("{" + "petId" + "}", petId.toString());

    // query params
    List<QueryParam> queryParams = [];
    Map<String, String> headerParams = {};
    Map<String, String> formParams = {};

    List<String> contentTypes = ["multipart/form-data"];

    String contentType = contentTypes.isNotEmpty ? contentTypes[0] : "application/json";
    List<String> authNames = ["petstore_auth"];

    if(contentType.startsWith("multipart/form-data")) {
      bool hasFields = false;
      MultipartRequest mp = MultipartRequest(null, null);
      if (additionalMetadata != null) {
        hasFields = true;
        mp.fields['additionalMetadata'] = parameterToString(additionalMetadata);
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
        formParams['additionalMetadata'] = parameterToString(additionalMetadata);
    }

    var response = await apiClient.invokeAPI(path,
                                             'POST',
                                             queryParams,
                                             postBody,
                                             headerParams,
                                             formParams,
                                             contentType,
                                             authNames);
    return response;
  }

  /// uploads an image
  ///
  /// 
  Future<ApiResponse> uploadFile(int petId, { String additionalMetadata, MultipartFile file }) async {
    Response response = await uploadFileWithHttpInfo(petId,  additionalMetadata: additionalMetadata, file: file );
    if(response.statusCode >= 400) {
      throw ApiException(response.statusCode, _decodeBodyBytes(response));
    } else if(response.body != null) {
      return apiClient.deserialize(_decodeBodyBytes(response), 'ApiResponse') as ApiResponse;
    } else {
      return null;
    }
  }

}
