package org.openapitools.client.api;

import org.openapitools.client.ApiClient;

import java.io.File;
import org.openapitools.client.model.ModelApiResponse;
import org.openapitools.client.model.Pet;
import java.util.Set;

import com.fasterxml.jackson.core.type.TypeReference;
import com.google.api.client.http.EmptyContent;
import com.google.api.client.http.GenericUrl;
import com.google.api.client.http.HttpContent;
import com.google.api.client.http.InputStreamContent;
import com.google.api.client.http.HttpMethods;
import com.google.api.client.http.HttpResponse;
import com.google.api.client.json.Json;

import javax.ws.rs.core.UriBuilder;
import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.List;
import java.util.ArrayList;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen")
public class PetApi {
    private ApiClient apiClient;

    public PetApi() {
        this(new ApiClient());
    }

    public PetApi(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    public ApiClient getApiClient() {
        return apiClient;
    }

    public void setApiClient(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

  /**
    * Add a new pet to the store
    * <p><b>200</b> - successful operation
    * <p><b>405</b> - Invalid input
    * @param body Pet object that needs to be added to the store
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public void addPet(Pet body) throws IOException {
        addPetForHttpResponse(body);
    }

  /**
    * Add a new pet to the store
    * <p><b>200</b> - successful operation
    * <p><b>405</b> - Invalid input
    * @param body Pet object that needs to be added to the store
    * @param params Map of query params. A collection will be interpreted as passing in multiple instances of the same query param.
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public void addPet(Pet body, Map<String, Object> params) throws IOException {
        addPetForHttpResponse(body, params);
    }

    public HttpResponse addPetForHttpResponse(Pet body) throws IOException {
        // verify the required parameter 'body' is set
        if (body == null) {
            throw new IllegalArgumentException("Missing the required parameter 'body' when calling addPet");
        }
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/pet");

        String localVarUrl = uriBuilder.build().toString();
        GenericUrl genericUrl = new GenericUrl(localVarUrl);

        HttpContent content = apiClient.new JacksonJsonHttpContent(body);
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.POST, genericUrl, content).execute();
    }

      public HttpResponse addPetForHttpResponse(java.io.InputStream body, String mediaType) throws IOException {
          // verify the required parameter 'body' is set
              if (body == null) {
              throw new IllegalArgumentException("Missing the required parameter 'body' when calling addPet");
              }
              UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/pet");

              String localVarUrl = uriBuilder.build().toString();
              GenericUrl genericUrl = new GenericUrl(localVarUrl);

              HttpContent content = body == null ?
                apiClient.new JacksonJsonHttpContent(null) :
                new InputStreamContent(mediaType == null ? Json.MEDIA_TYPE : mediaType, body);
              return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.POST, genericUrl, content).execute();
      }

    public HttpResponse addPetForHttpResponse(Pet body, Map<String, Object> params) throws IOException {
        // verify the required parameter 'body' is set
        if (body == null) {
            throw new IllegalArgumentException("Missing the required parameter 'body' when calling addPet");
        }
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/pet");

        // Copy the params argument if present, to allow passing in immutable maps
        Map<String, Object> allParams = params == null ? new HashMap<String, Object>() : new HashMap<String, Object>(params);

        for (Map.Entry<String, Object> entry: allParams.entrySet()) {
            String key = entry.getKey();
            Object value = entry.getValue();

            if (key != null && value != null) {
                if (value instanceof Collection) {
                    uriBuilder = uriBuilder.queryParam(key, ((Collection) value).toArray());
                } else if (value instanceof Object[]) {
                    uriBuilder = uriBuilder.queryParam(key, (Object[]) value);
                } else {
                    uriBuilder = uriBuilder.queryParam(key, value);
                }
            }
        }

        String localVarUrl = uriBuilder.build().toString();
        GenericUrl genericUrl = new GenericUrl(localVarUrl);

        HttpContent content = apiClient.new JacksonJsonHttpContent(body);
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.POST, genericUrl, content).execute();
    }


  /**
    * Deletes a pet
    * <p><b>200</b> - successful operation
    * <p><b>400</b> - Invalid pet value
    * @param petId Pet id to delete
    * @param apiKey The apiKey parameter
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public void deletePet(Long petId, String apiKey) throws IOException {
        deletePetForHttpResponse(petId, apiKey);
    }

  /**
    * Deletes a pet
    * <p><b>200</b> - successful operation
    * <p><b>400</b> - Invalid pet value
    * @param petId Pet id to delete
    * @param params Map of query params. A collection will be interpreted as passing in multiple instances of the same query param.
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public void deletePet(Long petId, Map<String, Object> params) throws IOException {
        deletePetForHttpResponse(petId, params);
    }

    public HttpResponse deletePetForHttpResponse(Long petId, String apiKey) throws IOException {
        // verify the required parameter 'petId' is set
        if (petId == null) {
            throw new IllegalArgumentException("Missing the required parameter 'petId' when calling deletePet");
        }
        // create a map of path variables
        final Map<String, Object> uriVariables = new HashMap<String, Object>();
        uriVariables.put("petId", petId);
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/pet/{petId}");

        String localVarUrl = uriBuilder.buildFromMap(uriVariables).toString();
        GenericUrl genericUrl = new GenericUrl(localVarUrl);

        HttpContent content = null;
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.DELETE, genericUrl, content).execute();
    }

    public HttpResponse deletePetForHttpResponse(Long petId, Map<String, Object> params) throws IOException {
        // verify the required parameter 'petId' is set
        if (petId == null) {
            throw new IllegalArgumentException("Missing the required parameter 'petId' when calling deletePet");
        }
        // create a map of path variables
        final Map<String, Object> uriVariables = new HashMap<String, Object>();
        uriVariables.put("petId", petId);
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/pet/{petId}");

        // Copy the params argument if present, to allow passing in immutable maps
        Map<String, Object> allParams = params == null ? new HashMap<String, Object>() : new HashMap<String, Object>(params);

        for (Map.Entry<String, Object> entry: allParams.entrySet()) {
            String key = entry.getKey();
            Object value = entry.getValue();

            if (key != null && value != null) {
                if (value instanceof Collection) {
                    uriBuilder = uriBuilder.queryParam(key, ((Collection) value).toArray());
                } else if (value instanceof Object[]) {
                    uriBuilder = uriBuilder.queryParam(key, (Object[]) value);
                } else {
                    uriBuilder = uriBuilder.queryParam(key, value);
                }
            }
        }

        String localVarUrl = uriBuilder.buildFromMap(uriVariables).toString();
        GenericUrl genericUrl = new GenericUrl(localVarUrl);

        HttpContent content = null;
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.DELETE, genericUrl, content).execute();
    }


  /**
    * Finds Pets by status
    * Multiple status values can be provided with comma separated strings
    * <p><b>200</b> - successful operation
    * <p><b>400</b> - Invalid status value
    * @param status Status values that need to be considered for filter
    * @return List&lt;Pet&gt;
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public List<Pet> findPetsByStatus(List<String> status) throws IOException {
        HttpResponse response = findPetsByStatusForHttpResponse(status);
        TypeReference<List<Pet>> typeRef = new TypeReference<List<Pet>>() {};
        return apiClient.getObjectMapper().readValue(response.getContent(), typeRef);
    }

  /**
    * Finds Pets by status
    * Multiple status values can be provided with comma separated strings
    * <p><b>200</b> - successful operation
    * <p><b>400</b> - Invalid status value
    * @param status Status values that need to be considered for filter
    * @param params Map of query params. A collection will be interpreted as passing in multiple instances of the same query param.
    * @return List&lt;Pet&gt;
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public List<Pet> findPetsByStatus(List<String> status, Map<String, Object> params) throws IOException {
        HttpResponse response = findPetsByStatusForHttpResponse(status, params);
        TypeReference<List<Pet>> typeRef = new TypeReference<List<Pet>>() {};
        return apiClient.getObjectMapper().readValue(response.getContent(), typeRef);
    }

    public HttpResponse findPetsByStatusForHttpResponse(List<String> status) throws IOException {
        // verify the required parameter 'status' is set
        if (status == null) {
            throw new IllegalArgumentException("Missing the required parameter 'status' when calling findPetsByStatus");
        }
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/pet/findByStatus");
        if (status != null) {
            String key = "status";
            Object value = status;
            if (value instanceof Collection) {
                uriBuilder = uriBuilder.queryParam(key, ((Collection) value).toArray());
            } else if (value instanceof Object[]) {
                uriBuilder = uriBuilder.queryParam(key, (Object[]) value);
            } else {
                uriBuilder = uriBuilder.queryParam(key, value);
            }
        }

        String localVarUrl = uriBuilder.build().toString();
        GenericUrl genericUrl = new GenericUrl(localVarUrl);

        HttpContent content = null;
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.GET, genericUrl, content).execute();
    }

    public HttpResponse findPetsByStatusForHttpResponse(List<String> status, Map<String, Object> params) throws IOException {
        // verify the required parameter 'status' is set
        if (status == null) {
            throw new IllegalArgumentException("Missing the required parameter 'status' when calling findPetsByStatus");
        }
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/pet/findByStatus");

        // Copy the params argument if present, to allow passing in immutable maps
        Map<String, Object> allParams = params == null ? new HashMap<String, Object>() : new HashMap<String, Object>(params);
        // Add the required query param 'status' to the map of query params
        allParams.put("status", status);

        for (Map.Entry<String, Object> entry: allParams.entrySet()) {
            String key = entry.getKey();
            Object value = entry.getValue();

            if (key != null && value != null) {
                if (value instanceof Collection) {
                    uriBuilder = uriBuilder.queryParam(key, ((Collection) value).toArray());
                } else if (value instanceof Object[]) {
                    uriBuilder = uriBuilder.queryParam(key, (Object[]) value);
                } else {
                    uriBuilder = uriBuilder.queryParam(key, value);
                }
            }
        }

        String localVarUrl = uriBuilder.build().toString();
        GenericUrl genericUrl = new GenericUrl(localVarUrl);

        HttpContent content = null;
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.GET, genericUrl, content).execute();
    }


  /**
    * Finds Pets by tags
    * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
    * <p><b>200</b> - successful operation
    * <p><b>400</b> - Invalid tag value
    * @param tags Tags to filter by
    * @return Set&lt;Pet&gt;
    * @throws IOException if an error occurs while attempting to invoke the API
    * @deprecated

    **/
    @Deprecated
    public Set<Pet> findPetsByTags(Set<String> tags) throws IOException {
        HttpResponse response = findPetsByTagsForHttpResponse(tags);
        TypeReference<Set<Pet>> typeRef = new TypeReference<Set<Pet>>() {};
        return apiClient.getObjectMapper().readValue(response.getContent(), typeRef);
    }

  /**
    * Finds Pets by tags
    * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
    * <p><b>200</b> - successful operation
    * <p><b>400</b> - Invalid tag value
    * @param tags Tags to filter by
    * @param params Map of query params. A collection will be interpreted as passing in multiple instances of the same query param.
    * @return Set&lt;Pet&gt;
    * @throws IOException if an error occurs while attempting to invoke the API
    * @deprecated

    **/
    @Deprecated
    public Set<Pet> findPetsByTags(Set<String> tags, Map<String, Object> params) throws IOException {
        HttpResponse response = findPetsByTagsForHttpResponse(tags, params);
        TypeReference<Set<Pet>> typeRef = new TypeReference<Set<Pet>>() {};
        return apiClient.getObjectMapper().readValue(response.getContent(), typeRef);
    }

    @Deprecated
    public HttpResponse findPetsByTagsForHttpResponse(Set<String> tags) throws IOException {
        // verify the required parameter 'tags' is set
        if (tags == null) {
            throw new IllegalArgumentException("Missing the required parameter 'tags' when calling findPetsByTags");
        }
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/pet/findByTags");
        if (tags != null) {
            String key = "tags";
            Object value = tags;
            if (value instanceof Collection) {
                uriBuilder = uriBuilder.queryParam(key, ((Collection) value).toArray());
            } else if (value instanceof Object[]) {
                uriBuilder = uriBuilder.queryParam(key, (Object[]) value);
            } else {
                uriBuilder = uriBuilder.queryParam(key, value);
            }
        }

        String localVarUrl = uriBuilder.build().toString();
        GenericUrl genericUrl = new GenericUrl(localVarUrl);

        HttpContent content = null;
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.GET, genericUrl, content).execute();
    }

    @Deprecated
    public HttpResponse findPetsByTagsForHttpResponse(Set<String> tags, Map<String, Object> params) throws IOException {
        // verify the required parameter 'tags' is set
        if (tags == null) {
            throw new IllegalArgumentException("Missing the required parameter 'tags' when calling findPetsByTags");
        }
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/pet/findByTags");

        // Copy the params argument if present, to allow passing in immutable maps
        Map<String, Object> allParams = params == null ? new HashMap<String, Object>() : new HashMap<String, Object>(params);
        // Add the required query param 'tags' to the map of query params
        allParams.put("tags", tags);

        for (Map.Entry<String, Object> entry: allParams.entrySet()) {
            String key = entry.getKey();
            Object value = entry.getValue();

            if (key != null && value != null) {
                if (value instanceof Collection) {
                    uriBuilder = uriBuilder.queryParam(key, ((Collection) value).toArray());
                } else if (value instanceof Object[]) {
                    uriBuilder = uriBuilder.queryParam(key, (Object[]) value);
                } else {
                    uriBuilder = uriBuilder.queryParam(key, value);
                }
            }
        }

        String localVarUrl = uriBuilder.build().toString();
        GenericUrl genericUrl = new GenericUrl(localVarUrl);

        HttpContent content = null;
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.GET, genericUrl, content).execute();
    }


  /**
    * Find pet by ID
    * Returns a single pet
    * <p><b>200</b> - successful operation
    * <p><b>400</b> - Invalid ID supplied
    * <p><b>404</b> - Pet not found
    * @param petId ID of pet to return
    * @return Pet
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public Pet getPetById(Long petId) throws IOException {
        HttpResponse response = getPetByIdForHttpResponse(petId);
        TypeReference<Pet> typeRef = new TypeReference<Pet>() {};
        return apiClient.getObjectMapper().readValue(response.getContent(), typeRef);
    }

  /**
    * Find pet by ID
    * Returns a single pet
    * <p><b>200</b> - successful operation
    * <p><b>400</b> - Invalid ID supplied
    * <p><b>404</b> - Pet not found
    * @param petId ID of pet to return
    * @param params Map of query params. A collection will be interpreted as passing in multiple instances of the same query param.
    * @return Pet
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public Pet getPetById(Long petId, Map<String, Object> params) throws IOException {
        HttpResponse response = getPetByIdForHttpResponse(petId, params);
        TypeReference<Pet> typeRef = new TypeReference<Pet>() {};
        return apiClient.getObjectMapper().readValue(response.getContent(), typeRef);
    }

    public HttpResponse getPetByIdForHttpResponse(Long petId) throws IOException {
        // verify the required parameter 'petId' is set
        if (petId == null) {
            throw new IllegalArgumentException("Missing the required parameter 'petId' when calling getPetById");
        }
        // create a map of path variables
        final Map<String, Object> uriVariables = new HashMap<String, Object>();
        uriVariables.put("petId", petId);
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/pet/{petId}");

        String localVarUrl = uriBuilder.buildFromMap(uriVariables).toString();
        GenericUrl genericUrl = new GenericUrl(localVarUrl);

        HttpContent content = null;
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.GET, genericUrl, content).execute();
    }

    public HttpResponse getPetByIdForHttpResponse(Long petId, Map<String, Object> params) throws IOException {
        // verify the required parameter 'petId' is set
        if (petId == null) {
            throw new IllegalArgumentException("Missing the required parameter 'petId' when calling getPetById");
        }
        // create a map of path variables
        final Map<String, Object> uriVariables = new HashMap<String, Object>();
        uriVariables.put("petId", petId);
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/pet/{petId}");

        // Copy the params argument if present, to allow passing in immutable maps
        Map<String, Object> allParams = params == null ? new HashMap<String, Object>() : new HashMap<String, Object>(params);

        for (Map.Entry<String, Object> entry: allParams.entrySet()) {
            String key = entry.getKey();
            Object value = entry.getValue();

            if (key != null && value != null) {
                if (value instanceof Collection) {
                    uriBuilder = uriBuilder.queryParam(key, ((Collection) value).toArray());
                } else if (value instanceof Object[]) {
                    uriBuilder = uriBuilder.queryParam(key, (Object[]) value);
                } else {
                    uriBuilder = uriBuilder.queryParam(key, value);
                }
            }
        }

        String localVarUrl = uriBuilder.buildFromMap(uriVariables).toString();
        GenericUrl genericUrl = new GenericUrl(localVarUrl);

        HttpContent content = null;
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.GET, genericUrl, content).execute();
    }


  /**
    * Update an existing pet
    * <p><b>200</b> - successful operation
    * <p><b>400</b> - Invalid ID supplied
    * <p><b>404</b> - Pet not found
    * <p><b>405</b> - Validation exception
    * @param body Pet object that needs to be added to the store
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public void updatePet(Pet body) throws IOException {
        updatePetForHttpResponse(body);
    }

  /**
    * Update an existing pet
    * <p><b>200</b> - successful operation
    * <p><b>400</b> - Invalid ID supplied
    * <p><b>404</b> - Pet not found
    * <p><b>405</b> - Validation exception
    * @param body Pet object that needs to be added to the store
    * @param params Map of query params. A collection will be interpreted as passing in multiple instances of the same query param.
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public void updatePet(Pet body, Map<String, Object> params) throws IOException {
        updatePetForHttpResponse(body, params);
    }

    public HttpResponse updatePetForHttpResponse(Pet body) throws IOException {
        // verify the required parameter 'body' is set
        if (body == null) {
            throw new IllegalArgumentException("Missing the required parameter 'body' when calling updatePet");
        }
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/pet");

        String localVarUrl = uriBuilder.build().toString();
        GenericUrl genericUrl = new GenericUrl(localVarUrl);

        HttpContent content = apiClient.new JacksonJsonHttpContent(body);
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.PUT, genericUrl, content).execute();
    }

      public HttpResponse updatePetForHttpResponse(java.io.InputStream body, String mediaType) throws IOException {
          // verify the required parameter 'body' is set
              if (body == null) {
              throw new IllegalArgumentException("Missing the required parameter 'body' when calling updatePet");
              }
              UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/pet");

              String localVarUrl = uriBuilder.build().toString();
              GenericUrl genericUrl = new GenericUrl(localVarUrl);

              HttpContent content = body == null ?
                apiClient.new JacksonJsonHttpContent(null) :
                new InputStreamContent(mediaType == null ? Json.MEDIA_TYPE : mediaType, body);
              return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.PUT, genericUrl, content).execute();
      }

    public HttpResponse updatePetForHttpResponse(Pet body, Map<String, Object> params) throws IOException {
        // verify the required parameter 'body' is set
        if (body == null) {
            throw new IllegalArgumentException("Missing the required parameter 'body' when calling updatePet");
        }
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/pet");

        // Copy the params argument if present, to allow passing in immutable maps
        Map<String, Object> allParams = params == null ? new HashMap<String, Object>() : new HashMap<String, Object>(params);

        for (Map.Entry<String, Object> entry: allParams.entrySet()) {
            String key = entry.getKey();
            Object value = entry.getValue();

            if (key != null && value != null) {
                if (value instanceof Collection) {
                    uriBuilder = uriBuilder.queryParam(key, ((Collection) value).toArray());
                } else if (value instanceof Object[]) {
                    uriBuilder = uriBuilder.queryParam(key, (Object[]) value);
                } else {
                    uriBuilder = uriBuilder.queryParam(key, value);
                }
            }
        }

        String localVarUrl = uriBuilder.build().toString();
        GenericUrl genericUrl = new GenericUrl(localVarUrl);

        HttpContent content = apiClient.new JacksonJsonHttpContent(body);
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.PUT, genericUrl, content).execute();
    }


  /**
    * Updates a pet in the store with form data
    * <p><b>405</b> - Invalid input
    * @param petId ID of pet that needs to be updated
    * @param name Updated name of the pet
    * @param status Updated status of the pet
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public void updatePetWithForm(Long petId, String name, String status) throws IOException {
        updatePetWithFormForHttpResponse(petId, name, status);
    }

  /**
    * Updates a pet in the store with form data
    * <p><b>405</b> - Invalid input
    * @param petId ID of pet that needs to be updated
    * @param params Map of query params. A collection will be interpreted as passing in multiple instances of the same query param.
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public void updatePetWithForm(Long petId, Map<String, Object> params) throws IOException {
        updatePetWithFormForHttpResponse(petId, params);
    }

    public HttpResponse updatePetWithFormForHttpResponse(Long petId, String name, String status) throws IOException {
        // verify the required parameter 'petId' is set
        if (petId == null) {
            throw new IllegalArgumentException("Missing the required parameter 'petId' when calling updatePetWithForm");
        }
        // create a map of path variables
        final Map<String, Object> uriVariables = new HashMap<String, Object>();
        uriVariables.put("petId", petId);
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/pet/{petId}");

        String localVarUrl = uriBuilder.buildFromMap(uriVariables).toString();
        GenericUrl genericUrl = new GenericUrl(localVarUrl);

        HttpContent content = new EmptyContent();
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.POST, genericUrl, content).execute();
    }

    public HttpResponse updatePetWithFormForHttpResponse(Long petId, Map<String, Object> params) throws IOException {
        // verify the required parameter 'petId' is set
        if (petId == null) {
            throw new IllegalArgumentException("Missing the required parameter 'petId' when calling updatePetWithForm");
        }
        // create a map of path variables
        final Map<String, Object> uriVariables = new HashMap<String, Object>();
        uriVariables.put("petId", petId);
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/pet/{petId}");

        // Copy the params argument if present, to allow passing in immutable maps
        Map<String, Object> allParams = params == null ? new HashMap<String, Object>() : new HashMap<String, Object>(params);

        for (Map.Entry<String, Object> entry: allParams.entrySet()) {
            String key = entry.getKey();
            Object value = entry.getValue();

            if (key != null && value != null) {
                if (value instanceof Collection) {
                    uriBuilder = uriBuilder.queryParam(key, ((Collection) value).toArray());
                } else if (value instanceof Object[]) {
                    uriBuilder = uriBuilder.queryParam(key, (Object[]) value);
                } else {
                    uriBuilder = uriBuilder.queryParam(key, value);
                }
            }
        }

        String localVarUrl = uriBuilder.buildFromMap(uriVariables).toString();
        GenericUrl genericUrl = new GenericUrl(localVarUrl);

        HttpContent content = new EmptyContent();
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.POST, genericUrl, content).execute();
    }


  /**
    * uploads an image
    * <p><b>200</b> - successful operation
    * @param petId ID of pet to update
    * @param additionalMetadata Additional data to pass to server
    * @param _file file to upload
    * @return ModelApiResponse
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public ModelApiResponse uploadFile(Long petId, String additionalMetadata, File _file) throws IOException {
        HttpResponse response = uploadFileForHttpResponse(petId, additionalMetadata, _file);
        TypeReference<ModelApiResponse> typeRef = new TypeReference<ModelApiResponse>() {};
        return apiClient.getObjectMapper().readValue(response.getContent(), typeRef);
    }

  /**
    * uploads an image
    * <p><b>200</b> - successful operation
    * @param petId ID of pet to update
    * @param params Map of query params. A collection will be interpreted as passing in multiple instances of the same query param.
    * @return ModelApiResponse
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public ModelApiResponse uploadFile(Long petId, Map<String, Object> params) throws IOException {
        HttpResponse response = uploadFileForHttpResponse(petId, params);
        TypeReference<ModelApiResponse> typeRef = new TypeReference<ModelApiResponse>() {};
        return apiClient.getObjectMapper().readValue(response.getContent(), typeRef);
    }

    public HttpResponse uploadFileForHttpResponse(Long petId, String additionalMetadata, File _file) throws IOException {
        // verify the required parameter 'petId' is set
        if (petId == null) {
            throw new IllegalArgumentException("Missing the required parameter 'petId' when calling uploadFile");
        }
        // create a map of path variables
        final Map<String, Object> uriVariables = new HashMap<String, Object>();
        uriVariables.put("petId", petId);
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/pet/{petId}/uploadImage");

        String localVarUrl = uriBuilder.buildFromMap(uriVariables).toString();
        GenericUrl genericUrl = new GenericUrl(localVarUrl);

        HttpContent content = new EmptyContent();
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.POST, genericUrl, content).execute();
    }

    public HttpResponse uploadFileForHttpResponse(Long petId, Map<String, Object> params) throws IOException {
        // verify the required parameter 'petId' is set
        if (petId == null) {
            throw new IllegalArgumentException("Missing the required parameter 'petId' when calling uploadFile");
        }
        // create a map of path variables
        final Map<String, Object> uriVariables = new HashMap<String, Object>();
        uriVariables.put("petId", petId);
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/pet/{petId}/uploadImage");

        // Copy the params argument if present, to allow passing in immutable maps
        Map<String, Object> allParams = params == null ? new HashMap<String, Object>() : new HashMap<String, Object>(params);

        for (Map.Entry<String, Object> entry: allParams.entrySet()) {
            String key = entry.getKey();
            Object value = entry.getValue();

            if (key != null && value != null) {
                if (value instanceof Collection) {
                    uriBuilder = uriBuilder.queryParam(key, ((Collection) value).toArray());
                } else if (value instanceof Object[]) {
                    uriBuilder = uriBuilder.queryParam(key, (Object[]) value);
                } else {
                    uriBuilder = uriBuilder.queryParam(key, value);
                }
            }
        }

        String localVarUrl = uriBuilder.buildFromMap(uriVariables).toString();
        GenericUrl genericUrl = new GenericUrl(localVarUrl);

        HttpContent content = new EmptyContent();
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.POST, genericUrl, content).execute();
    }


  /**
    * uploads an image (required)
    * <p><b>200</b> - successful operation
    * @param petId ID of pet to update
    * @param requiredFile file to upload
    * @param additionalMetadata Additional data to pass to server
    * @return ModelApiResponse
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public ModelApiResponse uploadFileWithRequiredFile(Long petId, File requiredFile, String additionalMetadata) throws IOException {
        HttpResponse response = uploadFileWithRequiredFileForHttpResponse(petId, requiredFile, additionalMetadata);
        TypeReference<ModelApiResponse> typeRef = new TypeReference<ModelApiResponse>() {};
        return apiClient.getObjectMapper().readValue(response.getContent(), typeRef);
    }

  /**
    * uploads an image (required)
    * <p><b>200</b> - successful operation
    * @param petId ID of pet to update
    * @param requiredFile file to upload
    * @param params Map of query params. A collection will be interpreted as passing in multiple instances of the same query param.
    * @return ModelApiResponse
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public ModelApiResponse uploadFileWithRequiredFile(Long petId, File requiredFile, Map<String, Object> params) throws IOException {
        HttpResponse response = uploadFileWithRequiredFileForHttpResponse(petId, requiredFile, params);
        TypeReference<ModelApiResponse> typeRef = new TypeReference<ModelApiResponse>() {};
        return apiClient.getObjectMapper().readValue(response.getContent(), typeRef);
    }

    public HttpResponse uploadFileWithRequiredFileForHttpResponse(Long petId, File requiredFile, String additionalMetadata) throws IOException {
        // verify the required parameter 'petId' is set
        if (petId == null) {
            throw new IllegalArgumentException("Missing the required parameter 'petId' when calling uploadFileWithRequiredFile");
        }// verify the required parameter 'requiredFile' is set
        if (requiredFile == null) {
            throw new IllegalArgumentException("Missing the required parameter 'requiredFile' when calling uploadFileWithRequiredFile");
        }
        // create a map of path variables
        final Map<String, Object> uriVariables = new HashMap<String, Object>();
        uriVariables.put("petId", petId);
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/fake/{petId}/uploadImageWithRequiredFile");

        String localVarUrl = uriBuilder.buildFromMap(uriVariables).toString();
        GenericUrl genericUrl = new GenericUrl(localVarUrl);

        HttpContent content = new EmptyContent();
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.POST, genericUrl, content).execute();
    }

    public HttpResponse uploadFileWithRequiredFileForHttpResponse(Long petId, File requiredFile, Map<String, Object> params) throws IOException {
        // verify the required parameter 'petId' is set
        if (petId == null) {
            throw new IllegalArgumentException("Missing the required parameter 'petId' when calling uploadFileWithRequiredFile");
        }// verify the required parameter 'requiredFile' is set
        if (requiredFile == null) {
            throw new IllegalArgumentException("Missing the required parameter 'requiredFile' when calling uploadFileWithRequiredFile");
        }
        // create a map of path variables
        final Map<String, Object> uriVariables = new HashMap<String, Object>();
        uriVariables.put("petId", petId);
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/fake/{petId}/uploadImageWithRequiredFile");

        // Copy the params argument if present, to allow passing in immutable maps
        Map<String, Object> allParams = params == null ? new HashMap<String, Object>() : new HashMap<String, Object>(params);

        for (Map.Entry<String, Object> entry: allParams.entrySet()) {
            String key = entry.getKey();
            Object value = entry.getValue();

            if (key != null && value != null) {
                if (value instanceof Collection) {
                    uriBuilder = uriBuilder.queryParam(key, ((Collection) value).toArray());
                } else if (value instanceof Object[]) {
                    uriBuilder = uriBuilder.queryParam(key, (Object[]) value);
                } else {
                    uriBuilder = uriBuilder.queryParam(key, value);
                }
            }
        }

        String localVarUrl = uriBuilder.buildFromMap(uriVariables).toString();
        GenericUrl genericUrl = new GenericUrl(localVarUrl);

        HttpContent content = new EmptyContent();
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.POST, genericUrl, content).execute();
    }


}
