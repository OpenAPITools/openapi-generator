package org.openapitools.client.api;

import org.openapitools.client.ApiClient;

import java.io.File;
import org.openapitools.client.model.ModelApiResponse;
import org.openapitools.client.model.Pet;
import java.util.Set;

import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.web.reactive.function.client.WebClient.ResponseSpec;
import org.springframework.web.reactive.function.client.WebClientResponseException;
import org.springframework.core.io.FileSystemResource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Flux;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen")
public class PetApi {
    private ApiClient apiClient;

    public PetApi() {
        this(new ApiClient());
    }

    @Autowired
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
     * <p><b>200</b> - Successful operation
     * <p><b>405</b> - Invalid input
     * @param pet Pet object that needs to be added to the store
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec addPetRequestCreation(Pet pet) throws WebClientResponseException {
        Object postBody = pet;
        // verify the required parameter 'pet' is set
        if (pet == null) {
            throw new WebClientResponseException("Missing the required parameter 'pet' when calling addPet", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json", "application/xml"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] { "petstore_auth" };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return apiClient.invokeAPI("/pet", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Add a new pet to the store
     * 
     * <p><b>200</b> - Successful operation
     * <p><b>405</b> - Invalid input
     * @param pet Pet object that needs to be added to the store
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Void> addPet(Pet pet) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return addPetRequestCreation(pet).bodyToMono(localVarReturnType);
    }

    public Mono<ResponseEntity<Void>> addPetWithHttpInfo(Pet pet) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return addPetRequestCreation(pet).toEntity(localVarReturnType);
    }
    /**
     * Deletes a pet
     * 
     * <p><b>200</b> - Successful operation
     * <p><b>400</b> - Invalid pet value
     * @param petId Pet id to delete
     * @param apiKey The apiKey parameter
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec deletePetRequestCreation(Long petId, String apiKey) throws WebClientResponseException {
        Object postBody = null;
        // verify the required parameter 'petId' is set
        if (petId == null) {
            throw new WebClientResponseException("Missing the required parameter 'petId' when calling deletePet", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        pathParams.put("petId", petId);

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        if (apiKey != null)
        headerParams.add("api_key", apiClient.parameterToString(apiKey));
        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] { "petstore_auth" };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return apiClient.invokeAPI("/pet/{petId}", HttpMethod.DELETE, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Deletes a pet
     * 
     * <p><b>200</b> - Successful operation
     * <p><b>400</b> - Invalid pet value
     * @param petId Pet id to delete
     * @param apiKey The apiKey parameter
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Void> deletePet(Long petId, String apiKey) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return deletePetRequestCreation(petId, apiKey).bodyToMono(localVarReturnType);
    }

    public Mono<ResponseEntity<Void>> deletePetWithHttpInfo(Long petId, String apiKey) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return deletePetRequestCreation(petId, apiKey).toEntity(localVarReturnType);
    }
    /**
     * Finds Pets by status
     * Multiple status values can be provided with comma separated strings
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid status value
     * @param status Status values that need to be considered for filter
     * @return List&lt;Pet&gt;
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec findPetsByStatusRequestCreation(List<String> status) throws WebClientResponseException {
        Object postBody = null;
        // verify the required parameter 'status' is set
        if (status == null) {
            throw new WebClientResponseException("Missing the required parameter 'status' when calling findPetsByStatus", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        queryParams.putAll(apiClient.parameterToMultiValueMap(ApiClient.CollectionFormat.valueOf("csv".toUpperCase(Locale.ROOT)), "status", status));

        final String[] localVarAccepts = { 
            "application/xml", "application/json"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] { "petstore_auth" };

        ParameterizedTypeReference<Pet> localVarReturnType = new ParameterizedTypeReference<Pet>() {};
        return apiClient.invokeAPI("/pet/findByStatus", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Finds Pets by status
     * Multiple status values can be provided with comma separated strings
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid status value
     * @param status Status values that need to be considered for filter
     * @return List&lt;Pet&gt;
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Flux<Pet> findPetsByStatus(List<String> status) throws WebClientResponseException {
        ParameterizedTypeReference<Pet> localVarReturnType = new ParameterizedTypeReference<Pet>() {};
        return findPetsByStatusRequestCreation(status).bodyToFlux(localVarReturnType);
    }

    public Mono<ResponseEntity<List<Pet>>> findPetsByStatusWithHttpInfo(List<String> status) throws WebClientResponseException {
        ParameterizedTypeReference<Pet> localVarReturnType = new ParameterizedTypeReference<Pet>() {};
        return findPetsByStatusRequestCreation(status).toEntityList(localVarReturnType);
    }
    /**
     * Finds Pets by tags
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid tag value
     * @param tags Tags to filter by
     * @return Set&lt;Pet&gt;
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     * @deprecated
     */
    @Deprecated
    private ResponseSpec findPetsByTagsRequestCreation(Set<String> tags) throws WebClientResponseException {
        Object postBody = null;
        // verify the required parameter 'tags' is set
        if (tags == null) {
            throw new WebClientResponseException("Missing the required parameter 'tags' when calling findPetsByTags", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        queryParams.putAll(apiClient.parameterToMultiValueMap(ApiClient.CollectionFormat.valueOf("csv".toUpperCase(Locale.ROOT)), "tags", tags));

        final String[] localVarAccepts = { 
            "application/xml", "application/json"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] { "petstore_auth" };

        ParameterizedTypeReference<Pet> localVarReturnType = new ParameterizedTypeReference<Pet>() {};
        return apiClient.invokeAPI("/pet/findByTags", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Finds Pets by tags
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid tag value
     * @param tags Tags to filter by
     * @return Set&lt;Pet&gt;
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Flux<Pet> findPetsByTags(Set<String> tags) throws WebClientResponseException {
        ParameterizedTypeReference<Pet> localVarReturnType = new ParameterizedTypeReference<Pet>() {};
        return findPetsByTagsRequestCreation(tags).bodyToFlux(localVarReturnType);
    }

    public Mono<ResponseEntity<List<Pet>>> findPetsByTagsWithHttpInfo(Set<String> tags) throws WebClientResponseException {
        ParameterizedTypeReference<Pet> localVarReturnType = new ParameterizedTypeReference<Pet>() {};
        return findPetsByTagsRequestCreation(tags).toEntityList(localVarReturnType);
    }
    /**
     * Find pet by ID
     * Returns a single pet
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid ID supplied
     * <p><b>404</b> - Pet not found
     * @param petId ID of pet to return
     * @return Pet
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec getPetByIdRequestCreation(Long petId) throws WebClientResponseException {
        Object postBody = null;
        // verify the required parameter 'petId' is set
        if (petId == null) {
            throw new WebClientResponseException("Missing the required parameter 'petId' when calling getPetById", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        pathParams.put("petId", petId);

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = { 
            "application/xml", "application/json"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] { "api_key" };

        ParameterizedTypeReference<Pet> localVarReturnType = new ParameterizedTypeReference<Pet>() {};
        return apiClient.invokeAPI("/pet/{petId}", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Find pet by ID
     * Returns a single pet
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid ID supplied
     * <p><b>404</b> - Pet not found
     * @param petId ID of pet to return
     * @return Pet
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Pet> getPetById(Long petId) throws WebClientResponseException {
        ParameterizedTypeReference<Pet> localVarReturnType = new ParameterizedTypeReference<Pet>() {};
        return getPetByIdRequestCreation(petId).bodyToMono(localVarReturnType);
    }

    public Mono<ResponseEntity<Pet>> getPetByIdWithHttpInfo(Long petId) throws WebClientResponseException {
        ParameterizedTypeReference<Pet> localVarReturnType = new ParameterizedTypeReference<Pet>() {};
        return getPetByIdRequestCreation(petId).toEntity(localVarReturnType);
    }
    /**
     * Update an existing pet
     * 
     * <p><b>200</b> - Successful operation
     * <p><b>400</b> - Invalid ID supplied
     * <p><b>404</b> - Pet not found
     * <p><b>405</b> - Validation exception
     * @param pet Pet object that needs to be added to the store
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec updatePetRequestCreation(Pet pet) throws WebClientResponseException {
        Object postBody = pet;
        // verify the required parameter 'pet' is set
        if (pet == null) {
            throw new WebClientResponseException("Missing the required parameter 'pet' when calling updatePet", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json", "application/xml"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] { "petstore_auth" };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return apiClient.invokeAPI("/pet", HttpMethod.PUT, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Update an existing pet
     * 
     * <p><b>200</b> - Successful operation
     * <p><b>400</b> - Invalid ID supplied
     * <p><b>404</b> - Pet not found
     * <p><b>405</b> - Validation exception
     * @param pet Pet object that needs to be added to the store
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Void> updatePet(Pet pet) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return updatePetRequestCreation(pet).bodyToMono(localVarReturnType);
    }

    public Mono<ResponseEntity<Void>> updatePetWithHttpInfo(Pet pet) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return updatePetRequestCreation(pet).toEntity(localVarReturnType);
    }
    /**
     * Updates a pet in the store with form data
     * 
     * <p><b>200</b> - Successful operation
     * <p><b>405</b> - Invalid input
     * @param petId ID of pet that needs to be updated
     * @param name Updated name of the pet
     * @param status Updated status of the pet
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec updatePetWithFormRequestCreation(Long petId, String name, String status) throws WebClientResponseException {
        Object postBody = null;
        // verify the required parameter 'petId' is set
        if (petId == null) {
            throw new WebClientResponseException("Missing the required parameter 'petId' when calling updatePetWithForm", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        pathParams.put("petId", petId);

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        if (name != null)
            formParams.add("name", name);
        if (status != null)
            formParams.add("status", status);

        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/x-www-form-urlencoded"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] { "petstore_auth" };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return apiClient.invokeAPI("/pet/{petId}", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Updates a pet in the store with form data
     * 
     * <p><b>200</b> - Successful operation
     * <p><b>405</b> - Invalid input
     * @param petId ID of pet that needs to be updated
     * @param name Updated name of the pet
     * @param status Updated status of the pet
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Void> updatePetWithForm(Long petId, String name, String status) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return updatePetWithFormRequestCreation(petId, name, status).bodyToMono(localVarReturnType);
    }

    public Mono<ResponseEntity<Void>> updatePetWithFormWithHttpInfo(Long petId, String name, String status) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return updatePetWithFormRequestCreation(petId, name, status).toEntity(localVarReturnType);
    }
    /**
     * uploads an image
     * 
     * <p><b>200</b> - successful operation
     * @param petId ID of pet to update
     * @param additionalMetadata Additional data to pass to server
     * @param file file to upload
     * @return ModelApiResponse
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec uploadFileRequestCreation(Long petId, String additionalMetadata, File file) throws WebClientResponseException {
        Object postBody = null;
        // verify the required parameter 'petId' is set
        if (petId == null) {
            throw new WebClientResponseException("Missing the required parameter 'petId' when calling uploadFile", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        pathParams.put("petId", petId);

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        if (additionalMetadata != null)
            formParams.add("additionalMetadata", additionalMetadata);
        if (file != null)
            formParams.add("file", new FileSystemResource(file));

        final String[] localVarAccepts = { 
            "application/json"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "multipart/form-data"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] { "petstore_auth" };

        ParameterizedTypeReference<ModelApiResponse> localVarReturnType = new ParameterizedTypeReference<ModelApiResponse>() {};
        return apiClient.invokeAPI("/pet/{petId}/uploadImage", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * uploads an image
     * 
     * <p><b>200</b> - successful operation
     * @param petId ID of pet to update
     * @param additionalMetadata Additional data to pass to server
     * @param file file to upload
     * @return ModelApiResponse
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ModelApiResponse> uploadFile(Long petId, String additionalMetadata, File file) throws WebClientResponseException {
        ParameterizedTypeReference<ModelApiResponse> localVarReturnType = new ParameterizedTypeReference<ModelApiResponse>() {};
        return uploadFileRequestCreation(petId, additionalMetadata, file).bodyToMono(localVarReturnType);
    }

    public Mono<ResponseEntity<ModelApiResponse>> uploadFileWithHttpInfo(Long petId, String additionalMetadata, File file) throws WebClientResponseException {
        ParameterizedTypeReference<ModelApiResponse> localVarReturnType = new ParameterizedTypeReference<ModelApiResponse>() {};
        return uploadFileRequestCreation(petId, additionalMetadata, file).toEntity(localVarReturnType);
    }
    /**
     * uploads an image (required)
     * 
     * <p><b>200</b> - successful operation
     * @param petId ID of pet to update
     * @param requiredFile file to upload
     * @param additionalMetadata Additional data to pass to server
     * @return ModelApiResponse
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec uploadFileWithRequiredFileRequestCreation(Long petId, File requiredFile, String additionalMetadata) throws WebClientResponseException {
        Object postBody = null;
        // verify the required parameter 'petId' is set
        if (petId == null) {
            throw new WebClientResponseException("Missing the required parameter 'petId' when calling uploadFileWithRequiredFile", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // verify the required parameter 'requiredFile' is set
        if (requiredFile == null) {
            throw new WebClientResponseException("Missing the required parameter 'requiredFile' when calling uploadFileWithRequiredFile", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        pathParams.put("petId", petId);

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        if (additionalMetadata != null)
            formParams.add("additionalMetadata", additionalMetadata);
        if (requiredFile != null)
            formParams.add("requiredFile", new FileSystemResource(requiredFile));

        final String[] localVarAccepts = { 
            "application/json"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "multipart/form-data"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] { "petstore_auth" };

        ParameterizedTypeReference<ModelApiResponse> localVarReturnType = new ParameterizedTypeReference<ModelApiResponse>() {};
        return apiClient.invokeAPI("/fake/{petId}/uploadImageWithRequiredFile", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * uploads an image (required)
     * 
     * <p><b>200</b> - successful operation
     * @param petId ID of pet to update
     * @param requiredFile file to upload
     * @param additionalMetadata Additional data to pass to server
     * @return ModelApiResponse
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ModelApiResponse> uploadFileWithRequiredFile(Long petId, File requiredFile, String additionalMetadata) throws WebClientResponseException {
        ParameterizedTypeReference<ModelApiResponse> localVarReturnType = new ParameterizedTypeReference<ModelApiResponse>() {};
        return uploadFileWithRequiredFileRequestCreation(petId, requiredFile, additionalMetadata).bodyToMono(localVarReturnType);
    }

    public Mono<ResponseEntity<ModelApiResponse>> uploadFileWithRequiredFileWithHttpInfo(Long petId, File requiredFile, String additionalMetadata) throws WebClientResponseException {
        ParameterizedTypeReference<ModelApiResponse> localVarReturnType = new ParameterizedTypeReference<ModelApiResponse>() {};
        return uploadFileWithRequiredFileRequestCreation(petId, requiredFile, additionalMetadata).toEntity(localVarReturnType);
    }
}
