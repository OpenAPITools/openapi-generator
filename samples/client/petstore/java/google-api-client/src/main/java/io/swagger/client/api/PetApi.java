package io.swagger.client.api;

import io.swagger.client.ApiClient;

import java.io.File;
import io.swagger.client.model.ModelApiResponse;
import io.swagger.client.model.Pet;

import com.fasterxml.jackson.core.type.TypeReference;
import com.google.api.client.http.GenericUrl;
import com.google.api.client.http.HttpContent;
import com.google.api.client.http.HttpMethods;
import com.google.api.client.http.HttpResponse;

import javax.ws.rs.core.UriBuilder;
import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.List;
import java.util.ArrayList;


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
    * 
    * <p><b>405</b> - Invalid input
    * @param body Pet object that needs to be added to the store
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public void addPet(Pet body) throws IOException {
        addPetForHttpResponse(body);
    }

  /**
    * Add a new pet to the store
    * 
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

        String url = uriBuilder.build().toString();
        GenericUrl genericUrl = new GenericUrl(url);

        HttpContent content = body == null ? null : apiClient.new JacksonJsonHttpContent(body);
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
                } else {
                    uriBuilder = uriBuilder.queryParam(key, value);
                }
            }
        }

        String url = uriBuilder.build().toString();
        GenericUrl genericUrl = new GenericUrl(url);

        HttpContent content = body == null ? null : apiClient.new JacksonJsonHttpContent(body);
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.POST, genericUrl, content).execute();
    }


  /**
    * Deletes a pet
    * 
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
    * 
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

        String url = uriBuilder.buildFromMap(uriVariables).toString();
        GenericUrl genericUrl = new GenericUrl(url);

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
                } else {
                    uriBuilder = uriBuilder.queryParam(key, value);
                }
            }
        }

        String url = uriBuilder.buildFromMap(uriVariables).toString();
        GenericUrl genericUrl = new GenericUrl(url);

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
        TypeReference typeRef = new TypeReference<List<Pet>>() {};
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
        TypeReference typeRef = new TypeReference<List<Pet>>() {};
        return apiClient.getObjectMapper().readValue(response.getContent(), typeRef);
    }

    public HttpResponse findPetsByStatusForHttpResponse(List<String> status) throws IOException {
        // verify the required parameter 'status' is set
        if (status == null) {
            throw new IllegalArgumentException("Missing the required parameter 'status' when calling findPetsByStatus");
        }
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/pet/findByStatus");
        if (status != null) {
            uriBuilder = uriBuilder.queryParam("status", status);
        }

        String url = uriBuilder.build().toString();
        GenericUrl genericUrl = new GenericUrl(url);

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
                } else {
                    uriBuilder = uriBuilder.queryParam(key, value);
                }
            }
        }

        String url = uriBuilder.build().toString();
        GenericUrl genericUrl = new GenericUrl(url);

        HttpContent content = null;
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.GET, genericUrl, content).execute();
    }


  /**
    * Finds Pets by tags
    * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
    * <p><b>200</b> - successful operation
    * <p><b>400</b> - Invalid tag value
    * @param tags Tags to filter by
    * @return List&lt;Pet&gt;
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public List<Pet> findPetsByTags(List<String> tags) throws IOException {
        HttpResponse response = findPetsByTagsForHttpResponse(tags);
        TypeReference typeRef = new TypeReference<List<Pet>>() {};
        return apiClient.getObjectMapper().readValue(response.getContent(), typeRef);
    }

  /**
    * Finds Pets by tags
    * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
    * <p><b>200</b> - successful operation
    * <p><b>400</b> - Invalid tag value
    * @param tags Tags to filter by
    * @param params Map of query params. A collection will be interpreted as passing in multiple instances of the same query param.
    * @return List&lt;Pet&gt;
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public List<Pet> findPetsByTags(List<String> tags, Map<String, Object> params) throws IOException {
        HttpResponse response = findPetsByTagsForHttpResponse(tags, params);
        TypeReference typeRef = new TypeReference<List<Pet>>() {};
        return apiClient.getObjectMapper().readValue(response.getContent(), typeRef);
    }

    public HttpResponse findPetsByTagsForHttpResponse(List<String> tags) throws IOException {
        // verify the required parameter 'tags' is set
        if (tags == null) {
            throw new IllegalArgumentException("Missing the required parameter 'tags' when calling findPetsByTags");
        }
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/pet/findByTags");
        if (tags != null) {
            uriBuilder = uriBuilder.queryParam("tags", tags);
        }

        String url = uriBuilder.build().toString();
        GenericUrl genericUrl = new GenericUrl(url);

        HttpContent content = null;
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.GET, genericUrl, content).execute();
    }

    public HttpResponse findPetsByTagsForHttpResponse(List<String> tags, Map<String, Object> params) throws IOException {
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
                } else {
                    uriBuilder = uriBuilder.queryParam(key, value);
                }
            }
        }

        String url = uriBuilder.build().toString();
        GenericUrl genericUrl = new GenericUrl(url);

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
        TypeReference typeRef = new TypeReference<Pet>() {};
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
        TypeReference typeRef = new TypeReference<Pet>() {};
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

        String url = uriBuilder.buildFromMap(uriVariables).toString();
        GenericUrl genericUrl = new GenericUrl(url);

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
                } else {
                    uriBuilder = uriBuilder.queryParam(key, value);
                }
            }
        }

        String url = uriBuilder.buildFromMap(uriVariables).toString();
        GenericUrl genericUrl = new GenericUrl(url);

        HttpContent content = null;
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.GET, genericUrl, content).execute();
    }


  /**
    * Update an existing pet
    * 
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
    * 
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

        String url = uriBuilder.build().toString();
        GenericUrl genericUrl = new GenericUrl(url);

        HttpContent content = body == null ? null : apiClient.new JacksonJsonHttpContent(body);
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
                } else {
                    uriBuilder = uriBuilder.queryParam(key, value);
                }
            }
        }

        String url = uriBuilder.build().toString();
        GenericUrl genericUrl = new GenericUrl(url);

        HttpContent content = body == null ? null : apiClient.new JacksonJsonHttpContent(body);
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.PUT, genericUrl, content).execute();
    }


  /**
    * Updates a pet in the store with form data
    * 
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
    * 
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

        String url = uriBuilder.buildFromMap(uriVariables).toString();
        GenericUrl genericUrl = new GenericUrl(url);

        HttpContent content = null;
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
                } else {
                    uriBuilder = uriBuilder.queryParam(key, value);
                }
            }
        }

        String url = uriBuilder.buildFromMap(uriVariables).toString();
        GenericUrl genericUrl = new GenericUrl(url);

        HttpContent content = null;
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.POST, genericUrl, content).execute();
    }


  /**
    * uploads an image
    * 
    * <p><b>200</b> - successful operation
    * @param petId ID of pet to update
    * @param additionalMetadata Additional data to pass to server
    * @param file file to upload
    * @return ModelApiResponse
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public ModelApiResponse uploadFile(Long petId, String additionalMetadata, File file) throws IOException {
        HttpResponse response = uploadFileForHttpResponse(petId, additionalMetadata, file);
        TypeReference typeRef = new TypeReference<ModelApiResponse>() {};
        return apiClient.getObjectMapper().readValue(response.getContent(), typeRef);
    }

  /**
    * uploads an image
    * 
    * <p><b>200</b> - successful operation
    * @param petId ID of pet to update
    * @param params Map of query params. A collection will be interpreted as passing in multiple instances of the same query param.
    * @return ModelApiResponse
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public ModelApiResponse uploadFile(Long petId, Map<String, Object> params) throws IOException {
        HttpResponse response = uploadFileForHttpResponse(petId, params);
        TypeReference typeRef = new TypeReference<ModelApiResponse>() {};
        return apiClient.getObjectMapper().readValue(response.getContent(), typeRef);
    }

    public HttpResponse uploadFileForHttpResponse(Long petId, String additionalMetadata, File file) throws IOException {
        // verify the required parameter 'petId' is set
        if (petId == null) {
            throw new IllegalArgumentException("Missing the required parameter 'petId' when calling uploadFile");
        }
        // create a map of path variables
        final Map<String, Object> uriVariables = new HashMap<String, Object>();
        uriVariables.put("petId", petId);
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/pet/{petId}/uploadImage");

        String url = uriBuilder.buildFromMap(uriVariables).toString();
        GenericUrl genericUrl = new GenericUrl(url);

        HttpContent content = null;
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
                } else {
                    uriBuilder = uriBuilder.queryParam(key, value);
                }
            }
        }

        String url = uriBuilder.buildFromMap(uriVariables).toString();
        GenericUrl genericUrl = new GenericUrl(url);

        HttpContent content = null;
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.POST, genericUrl, content).execute();
    }


}
