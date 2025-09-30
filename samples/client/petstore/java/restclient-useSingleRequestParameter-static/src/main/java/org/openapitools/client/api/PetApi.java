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
import java.util.Objects;
import java.util.Arrays;
import java.util.stream.Collectors;

import org.springframework.core.io.FileSystemResource;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestClient.ResponseSpec;
import org.springframework.web.client.RestClientResponseException;

@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")
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
     * <p><b>200</b> - Successful operation
     * <p><b>405</b> - Invalid input
     * @param pet Pet object that needs to be added to the store
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec addPetRequestCreation(@jakarta.annotation.Nonnull Pet pet) throws RestClientResponseException {
        Object postBody = pet;
        // verify the required parameter 'pet' is set
        if (pet == null) {
            throw new RestClientResponseException("Missing the required parameter 'pet' when calling addPet", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json", "application/xml"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] { "petstore_auth" };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/pet", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Add a new pet to the store
     * 
     * <p><b>200</b> - Successful operation
     * <p><b>405</b> - Invalid input
     * @param pet Pet object that needs to be added to the store
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public void addPet(@jakarta.annotation.Nonnull Pet pet) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        addPetRequestCreation(pet).body(localVarReturnType);
    }

    /**
     * Add a new pet to the store
     * 
     * <p><b>200</b> - Successful operation
     * <p><b>405</b> - Invalid input
     * @param pet Pet object that needs to be added to the store
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> addPetWithHttpInfo(@jakarta.annotation.Nonnull Pet pet) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return addPetRequestCreation(pet).toEntity(localVarReturnType);
    }

    /**
     * Add a new pet to the store
     * 
     * <p><b>200</b> - Successful operation
     * <p><b>405</b> - Invalid input
     * @param pet Pet object that needs to be added to the store
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec addPetWithResponseSpec(@jakarta.annotation.Nonnull Pet pet) throws RestClientResponseException {
        return addPetRequestCreation(pet);
    }

    public static class DeletePetRequest {
        private @jakarta.annotation.Nonnull Long petId;
        private @jakarta.annotation.Nullable String apiKey;

        public DeletePetRequest() {}

        public DeletePetRequest(@jakarta.annotation.Nonnull Long petId, @jakarta.annotation.Nullable String apiKey) {
            this.petId = petId;
            this.apiKey = apiKey;
        }

        public @jakarta.annotation.Nonnull Long petId() {
            return this.petId;
        }
        public DeletePetRequest petId(@jakarta.annotation.Nonnull Long petId) {
            this.petId = petId;
            return this;
        }

        public @jakarta.annotation.Nullable String apiKey() {
            return this.apiKey;
        }
        public DeletePetRequest apiKey(@jakarta.annotation.Nullable String apiKey) {
            this.apiKey = apiKey;
            return this;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) {
                return true;
            }
            if (o == null || getClass() != o.getClass()) {
                return false;
            }
            DeletePetRequest request = (DeletePetRequest) o;
            return Objects.equals(this.petId, request.petId()) &&
                Objects.equals(this.apiKey, request.apiKey());
        }

        @Override
        public int hashCode() {
            return Objects.hash(petId, apiKey);
        }
    }

    /**
     * Deletes a pet
     * 
     * <p><b>200</b> - Successful operation
     * <p><b>400</b> - Invalid pet value
     * @param requestParameters The deletePet request parameters as object
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public void deletePet(DeletePetRequest requestParameters) throws RestClientResponseException {
        this.deletePet(requestParameters.petId(), requestParameters.apiKey());
    }

    /**
     * Deletes a pet
     * 
     * <p><b>200</b> - Successful operation
     * <p><b>400</b> - Invalid pet value
     * @param requestParameters The deletePet request parameters as object
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> deletePetWithHttpInfo(DeletePetRequest requestParameters) throws RestClientResponseException {
        return this.deletePetWithHttpInfo(requestParameters.petId(), requestParameters.apiKey());
    }

    /**
     * Deletes a pet
     * 
     * <p><b>200</b> - Successful operation
     * <p><b>400</b> - Invalid pet value
     * @param requestParameters The deletePet request parameters as object
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec deletePetWithResponseSpec(DeletePetRequest requestParameters) throws RestClientResponseException {
        return this.deletePetWithResponseSpec(requestParameters.petId(), requestParameters.apiKey());
    }

    /**
     * Deletes a pet
     * 
     * <p><b>200</b> - Successful operation
     * <p><b>400</b> - Invalid pet value
     * @param petId Pet id to delete
     * @param apiKey The apiKey parameter
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec deletePetRequestCreation(@jakarta.annotation.Nonnull Long petId, @jakarta.annotation.Nullable String apiKey) throws RestClientResponseException {
        Object postBody = null;
        // verify the required parameter 'petId' is set
        if (petId == null) {
            throw new RestClientResponseException("Missing the required parameter 'petId' when calling deletePet", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        pathParams.put("petId", petId);

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        if (apiKey != null)
        headerParams.add("api_key", apiClient.parameterToString(apiKey));
        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] { "petstore_auth" };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/pet/{petId}", HttpMethod.DELETE, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Deletes a pet
     * 
     * <p><b>200</b> - Successful operation
     * <p><b>400</b> - Invalid pet value
     * @param petId Pet id to delete
     * @param apiKey The apiKey parameter
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public void deletePet(@jakarta.annotation.Nonnull Long petId, @jakarta.annotation.Nullable String apiKey) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        deletePetRequestCreation(petId, apiKey).body(localVarReturnType);
    }

    /**
     * Deletes a pet
     * 
     * <p><b>200</b> - Successful operation
     * <p><b>400</b> - Invalid pet value
     * @param petId Pet id to delete
     * @param apiKey The apiKey parameter
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> deletePetWithHttpInfo(@jakarta.annotation.Nonnull Long petId, @jakarta.annotation.Nullable String apiKey) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return deletePetRequestCreation(petId, apiKey).toEntity(localVarReturnType);
    }

    /**
     * Deletes a pet
     * 
     * <p><b>200</b> - Successful operation
     * <p><b>400</b> - Invalid pet value
     * @param petId Pet id to delete
     * @param apiKey The apiKey parameter
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec deletePetWithResponseSpec(@jakarta.annotation.Nonnull Long petId, @jakarta.annotation.Nullable String apiKey) throws RestClientResponseException {
        return deletePetRequestCreation(petId, apiKey);
    }

    /**
     * Finds Pets by status
     * Multiple status values can be provided with comma separated strings
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid status value
     * @param status Status values that need to be considered for filter
     * @return List&lt;Pet&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec findPetsByStatusRequestCreation(@jakarta.annotation.Nonnull List<String> status) throws RestClientResponseException {
        Object postBody = null;
        // verify the required parameter 'status' is set
        if (status == null) {
            throw new RestClientResponseException("Missing the required parameter 'status' when calling findPetsByStatus", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        queryParams.putAll(apiClient.parameterToMultiValueMap(ApiClient.CollectionFormat.valueOf("csv".toUpperCase(Locale.ROOT)), "status", status));

        final String[] localVarAccepts = { 
            "application/xml", "application/json"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] { "petstore_auth" };

        ParameterizedTypeReference<List<Pet>> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/pet/findByStatus", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Finds Pets by status
     * Multiple status values can be provided with comma separated strings
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid status value
     * @param status Status values that need to be considered for filter
     * @return List&lt;Pet&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public List<Pet> findPetsByStatus(@jakarta.annotation.Nonnull List<String> status) throws RestClientResponseException {
        ParameterizedTypeReference<List<Pet>> localVarReturnType = new ParameterizedTypeReference<>() {};
        return findPetsByStatusRequestCreation(status).body(localVarReturnType);
    }

    /**
     * Finds Pets by status
     * Multiple status values can be provided with comma separated strings
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid status value
     * @param status Status values that need to be considered for filter
     * @return ResponseEntity&lt;List&lt;Pet&gt;&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<List<Pet>> findPetsByStatusWithHttpInfo(@jakarta.annotation.Nonnull List<String> status) throws RestClientResponseException {
        ParameterizedTypeReference<List<Pet>> localVarReturnType = new ParameterizedTypeReference<>() {};
        return findPetsByStatusRequestCreation(status).toEntity(localVarReturnType);
    }

    /**
     * Finds Pets by status
     * Multiple status values can be provided with comma separated strings
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid status value
     * @param status Status values that need to be considered for filter
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec findPetsByStatusWithResponseSpec(@jakarta.annotation.Nonnull List<String> status) throws RestClientResponseException {
        return findPetsByStatusRequestCreation(status);
    }

    /**
     * Finds Pets by tags
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid tag value
     * @param tags Tags to filter by
     * @return Set&lt;Pet&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     * @deprecated
     */
    @Deprecated
    private ResponseSpec findPetsByTagsRequestCreation(@jakarta.annotation.Nonnull Set<String> tags) throws RestClientResponseException {
        Object postBody = null;
        // verify the required parameter 'tags' is set
        if (tags == null) {
            throw new RestClientResponseException("Missing the required parameter 'tags' when calling findPetsByTags", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        queryParams.putAll(apiClient.parameterToMultiValueMap(ApiClient.CollectionFormat.valueOf("csv".toUpperCase(Locale.ROOT)), "tags", tags));

        final String[] localVarAccepts = { 
            "application/xml", "application/json"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] { "petstore_auth" };

        ParameterizedTypeReference<Set<Pet>> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/pet/findByTags", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Finds Pets by tags
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid tag value
     * @param tags Tags to filter by
     * @return Set&lt;Pet&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public Set<Pet> findPetsByTags(@jakarta.annotation.Nonnull Set<String> tags) throws RestClientResponseException {
        ParameterizedTypeReference<Set<Pet>> localVarReturnType = new ParameterizedTypeReference<>() {};
        return findPetsByTagsRequestCreation(tags).body(localVarReturnType);
    }

    /**
     * Finds Pets by tags
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid tag value
     * @param tags Tags to filter by
     * @return ResponseEntity&lt;Set&lt;Pet&gt;&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Set<Pet>> findPetsByTagsWithHttpInfo(@jakarta.annotation.Nonnull Set<String> tags) throws RestClientResponseException {
        ParameterizedTypeReference<Set<Pet>> localVarReturnType = new ParameterizedTypeReference<>() {};
        return findPetsByTagsRequestCreation(tags).toEntity(localVarReturnType);
    }

    /**
     * Finds Pets by tags
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid tag value
     * @param tags Tags to filter by
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec findPetsByTagsWithResponseSpec(@jakarta.annotation.Nonnull Set<String> tags) throws RestClientResponseException {
        return findPetsByTagsRequestCreation(tags);
    }

    /**
     * Find pet by ID
     * Returns a single pet
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid ID supplied
     * <p><b>404</b> - Pet not found
     * @param petId ID of pet to return
     * @return Pet
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec getPetByIdRequestCreation(@jakarta.annotation.Nonnull Long petId) throws RestClientResponseException {
        Object postBody = null;
        // verify the required parameter 'petId' is set
        if (petId == null) {
            throw new RestClientResponseException("Missing the required parameter 'petId' when calling getPetById", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        pathParams.put("petId", petId);

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = { 
            "application/xml", "application/json"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] { "api_key" };

        ParameterizedTypeReference<Pet> localVarReturnType = new ParameterizedTypeReference<>() {};
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
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public Pet getPetById(@jakarta.annotation.Nonnull Long petId) throws RestClientResponseException {
        ParameterizedTypeReference<Pet> localVarReturnType = new ParameterizedTypeReference<>() {};
        return getPetByIdRequestCreation(petId).body(localVarReturnType);
    }

    /**
     * Find pet by ID
     * Returns a single pet
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid ID supplied
     * <p><b>404</b> - Pet not found
     * @param petId ID of pet to return
     * @return ResponseEntity&lt;Pet&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Pet> getPetByIdWithHttpInfo(@jakarta.annotation.Nonnull Long petId) throws RestClientResponseException {
        ParameterizedTypeReference<Pet> localVarReturnType = new ParameterizedTypeReference<>() {};
        return getPetByIdRequestCreation(petId).toEntity(localVarReturnType);
    }

    /**
     * Find pet by ID
     * Returns a single pet
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid ID supplied
     * <p><b>404</b> - Pet not found
     * @param petId ID of pet to return
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec getPetByIdWithResponseSpec(@jakarta.annotation.Nonnull Long petId) throws RestClientResponseException {
        return getPetByIdRequestCreation(petId);
    }

    /**
     * Update an existing pet
     * 
     * <p><b>200</b> - Successful operation
     * <p><b>400</b> - Invalid ID supplied
     * <p><b>404</b> - Pet not found
     * <p><b>405</b> - Validation exception
     * @param pet Pet object that needs to be added to the store
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec updatePetRequestCreation(@jakarta.annotation.Nonnull Pet pet) throws RestClientResponseException {
        Object postBody = pet;
        // verify the required parameter 'pet' is set
        if (pet == null) {
            throw new RestClientResponseException("Missing the required parameter 'pet' when calling updatePet", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json", "application/xml"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] { "petstore_auth" };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
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
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public void updatePet(@jakarta.annotation.Nonnull Pet pet) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        updatePetRequestCreation(pet).body(localVarReturnType);
    }

    /**
     * Update an existing pet
     * 
     * <p><b>200</b> - Successful operation
     * <p><b>400</b> - Invalid ID supplied
     * <p><b>404</b> - Pet not found
     * <p><b>405</b> - Validation exception
     * @param pet Pet object that needs to be added to the store
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> updatePetWithHttpInfo(@jakarta.annotation.Nonnull Pet pet) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return updatePetRequestCreation(pet).toEntity(localVarReturnType);
    }

    /**
     * Update an existing pet
     * 
     * <p><b>200</b> - Successful operation
     * <p><b>400</b> - Invalid ID supplied
     * <p><b>404</b> - Pet not found
     * <p><b>405</b> - Validation exception
     * @param pet Pet object that needs to be added to the store
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec updatePetWithResponseSpec(@jakarta.annotation.Nonnull Pet pet) throws RestClientResponseException {
        return updatePetRequestCreation(pet);
    }

    public static class UpdatePetWithFormRequest {
        private @jakarta.annotation.Nonnull Long petId;
        private @jakarta.annotation.Nullable String name;
        private @jakarta.annotation.Nullable String status;

        public UpdatePetWithFormRequest() {}

        public UpdatePetWithFormRequest(@jakarta.annotation.Nonnull Long petId, @jakarta.annotation.Nullable String name, @jakarta.annotation.Nullable String status) {
            this.petId = petId;
            this.name = name;
            this.status = status;
        }

        public @jakarta.annotation.Nonnull Long petId() {
            return this.petId;
        }
        public UpdatePetWithFormRequest petId(@jakarta.annotation.Nonnull Long petId) {
            this.petId = petId;
            return this;
        }

        public @jakarta.annotation.Nullable String name() {
            return this.name;
        }
        public UpdatePetWithFormRequest name(@jakarta.annotation.Nullable String name) {
            this.name = name;
            return this;
        }

        public @jakarta.annotation.Nullable String status() {
            return this.status;
        }
        public UpdatePetWithFormRequest status(@jakarta.annotation.Nullable String status) {
            this.status = status;
            return this;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) {
                return true;
            }
            if (o == null || getClass() != o.getClass()) {
                return false;
            }
            UpdatePetWithFormRequest request = (UpdatePetWithFormRequest) o;
            return Objects.equals(this.petId, request.petId()) &&
                Objects.equals(this.name, request.name()) &&
                Objects.equals(this.status, request.status());
        }

        @Override
        public int hashCode() {
            return Objects.hash(petId, name, status);
        }
    }

    /**
     * Updates a pet in the store with form data
     * 
     * <p><b>200</b> - Successful operation
     * <p><b>405</b> - Invalid input
     * @param requestParameters The updatePetWithForm request parameters as object
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public void updatePetWithForm(UpdatePetWithFormRequest requestParameters) throws RestClientResponseException {
        this.updatePetWithForm(requestParameters.petId(), requestParameters.name(), requestParameters.status());
    }

    /**
     * Updates a pet in the store with form data
     * 
     * <p><b>200</b> - Successful operation
     * <p><b>405</b> - Invalid input
     * @param requestParameters The updatePetWithForm request parameters as object
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> updatePetWithFormWithHttpInfo(UpdatePetWithFormRequest requestParameters) throws RestClientResponseException {
        return this.updatePetWithFormWithHttpInfo(requestParameters.petId(), requestParameters.name(), requestParameters.status());
    }

    /**
     * Updates a pet in the store with form data
     * 
     * <p><b>200</b> - Successful operation
     * <p><b>405</b> - Invalid input
     * @param requestParameters The updatePetWithForm request parameters as object
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec updatePetWithFormWithResponseSpec(UpdatePetWithFormRequest requestParameters) throws RestClientResponseException {
        return this.updatePetWithFormWithResponseSpec(requestParameters.petId(), requestParameters.name(), requestParameters.status());
    }

    /**
     * Updates a pet in the store with form data
     * 
     * <p><b>200</b> - Successful operation
     * <p><b>405</b> - Invalid input
     * @param petId ID of pet that needs to be updated
     * @param name Updated name of the pet
     * @param status Updated status of the pet
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec updatePetWithFormRequestCreation(@jakarta.annotation.Nonnull Long petId, @jakarta.annotation.Nullable String name, @jakarta.annotation.Nullable String status) throws RestClientResponseException {
        Object postBody = null;
        // verify the required parameter 'petId' is set
        if (petId == null) {
            throw new RestClientResponseException("Missing the required parameter 'petId' when calling updatePetWithForm", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        pathParams.put("petId", petId);

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

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

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
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
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public void updatePetWithForm(@jakarta.annotation.Nonnull Long petId, @jakarta.annotation.Nullable String name, @jakarta.annotation.Nullable String status) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        updatePetWithFormRequestCreation(petId, name, status).body(localVarReturnType);
    }

    /**
     * Updates a pet in the store with form data
     * 
     * <p><b>200</b> - Successful operation
     * <p><b>405</b> - Invalid input
     * @param petId ID of pet that needs to be updated
     * @param name Updated name of the pet
     * @param status Updated status of the pet
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> updatePetWithFormWithHttpInfo(@jakarta.annotation.Nonnull Long petId, @jakarta.annotation.Nullable String name, @jakarta.annotation.Nullable String status) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return updatePetWithFormRequestCreation(petId, name, status).toEntity(localVarReturnType);
    }

    /**
     * Updates a pet in the store with form data
     * 
     * <p><b>200</b> - Successful operation
     * <p><b>405</b> - Invalid input
     * @param petId ID of pet that needs to be updated
     * @param name Updated name of the pet
     * @param status Updated status of the pet
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec updatePetWithFormWithResponseSpec(@jakarta.annotation.Nonnull Long petId, @jakarta.annotation.Nullable String name, @jakarta.annotation.Nullable String status) throws RestClientResponseException {
        return updatePetWithFormRequestCreation(petId, name, status);
    }

    public static class UploadFileRequest {
        private @jakarta.annotation.Nonnull Long petId;
        private @jakarta.annotation.Nullable String additionalMetadata;
        private @jakarta.annotation.Nullable File _file;

        public UploadFileRequest() {}

        public UploadFileRequest(@jakarta.annotation.Nonnull Long petId, @jakarta.annotation.Nullable String additionalMetadata, @jakarta.annotation.Nullable File _file) {
            this.petId = petId;
            this.additionalMetadata = additionalMetadata;
            this._file = _file;
        }

        public @jakarta.annotation.Nonnull Long petId() {
            return this.petId;
        }
        public UploadFileRequest petId(@jakarta.annotation.Nonnull Long petId) {
            this.petId = petId;
            return this;
        }

        public @jakarta.annotation.Nullable String additionalMetadata() {
            return this.additionalMetadata;
        }
        public UploadFileRequest additionalMetadata(@jakarta.annotation.Nullable String additionalMetadata) {
            this.additionalMetadata = additionalMetadata;
            return this;
        }

        public @jakarta.annotation.Nullable File _file() {
            return this._file;
        }
        public UploadFileRequest _file(@jakarta.annotation.Nullable File _file) {
            this._file = _file;
            return this;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) {
                return true;
            }
            if (o == null || getClass() != o.getClass()) {
                return false;
            }
            UploadFileRequest request = (UploadFileRequest) o;
            return Objects.equals(this.petId, request.petId()) &&
                Objects.equals(this.additionalMetadata, request.additionalMetadata()) &&
                Objects.equals(this._file, request._file());
        }

        @Override
        public int hashCode() {
            return Objects.hash(petId, additionalMetadata, _file);
        }
    }

    /**
     * uploads an image
     * 
     * <p><b>200</b> - successful operation
     * @param requestParameters The uploadFile request parameters as object
     * @return ModelApiResponse
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ModelApiResponse uploadFile(UploadFileRequest requestParameters) throws RestClientResponseException {
        return this.uploadFile(requestParameters.petId(), requestParameters.additionalMetadata(), requestParameters._file());
    }

    /**
     * uploads an image
     * 
     * <p><b>200</b> - successful operation
     * @param requestParameters The uploadFile request parameters as object
     * @return ResponseEntity&lt;ModelApiResponse&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<ModelApiResponse> uploadFileWithHttpInfo(UploadFileRequest requestParameters) throws RestClientResponseException {
        return this.uploadFileWithHttpInfo(requestParameters.petId(), requestParameters.additionalMetadata(), requestParameters._file());
    }

    /**
     * uploads an image
     * 
     * <p><b>200</b> - successful operation
     * @param requestParameters The uploadFile request parameters as object
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec uploadFileWithResponseSpec(UploadFileRequest requestParameters) throws RestClientResponseException {
        return this.uploadFileWithResponseSpec(requestParameters.petId(), requestParameters.additionalMetadata(), requestParameters._file());
    }

    /**
     * uploads an image
     * 
     * <p><b>200</b> - successful operation
     * @param petId ID of pet to update
     * @param additionalMetadata Additional data to pass to server
     * @param _file file to upload
     * @return ModelApiResponse
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec uploadFileRequestCreation(@jakarta.annotation.Nonnull Long petId, @jakarta.annotation.Nullable String additionalMetadata, @jakarta.annotation.Nullable File _file) throws RestClientResponseException {
        Object postBody = null;
        // verify the required parameter 'petId' is set
        if (petId == null) {
            throw new RestClientResponseException("Missing the required parameter 'petId' when calling uploadFile", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        pathParams.put("petId", petId);

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        if (additionalMetadata != null)
            formParams.add("additionalMetadata", additionalMetadata);
        if (_file != null)
            formParams.add("file", new FileSystemResource(_file));

        final String[] localVarAccepts = { 
            "application/json"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "multipart/form-data"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] { "petstore_auth" };

        ParameterizedTypeReference<ModelApiResponse> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/pet/{petId}/uploadImage", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * uploads an image
     * 
     * <p><b>200</b> - successful operation
     * @param petId ID of pet to update
     * @param additionalMetadata Additional data to pass to server
     * @param _file file to upload
     * @return ModelApiResponse
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ModelApiResponse uploadFile(@jakarta.annotation.Nonnull Long petId, @jakarta.annotation.Nullable String additionalMetadata, @jakarta.annotation.Nullable File _file) throws RestClientResponseException {
        ParameterizedTypeReference<ModelApiResponse> localVarReturnType = new ParameterizedTypeReference<>() {};
        return uploadFileRequestCreation(petId, additionalMetadata, _file).body(localVarReturnType);
    }

    /**
     * uploads an image
     * 
     * <p><b>200</b> - successful operation
     * @param petId ID of pet to update
     * @param additionalMetadata Additional data to pass to server
     * @param _file file to upload
     * @return ResponseEntity&lt;ModelApiResponse&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<ModelApiResponse> uploadFileWithHttpInfo(@jakarta.annotation.Nonnull Long petId, @jakarta.annotation.Nullable String additionalMetadata, @jakarta.annotation.Nullable File _file) throws RestClientResponseException {
        ParameterizedTypeReference<ModelApiResponse> localVarReturnType = new ParameterizedTypeReference<>() {};
        return uploadFileRequestCreation(petId, additionalMetadata, _file).toEntity(localVarReturnType);
    }

    /**
     * uploads an image
     * 
     * <p><b>200</b> - successful operation
     * @param petId ID of pet to update
     * @param additionalMetadata Additional data to pass to server
     * @param _file file to upload
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec uploadFileWithResponseSpec(@jakarta.annotation.Nonnull Long petId, @jakarta.annotation.Nullable String additionalMetadata, @jakarta.annotation.Nullable File _file) throws RestClientResponseException {
        return uploadFileRequestCreation(petId, additionalMetadata, _file);
    }

    public static class UploadFileWithRequiredFileRequest {
        private @jakarta.annotation.Nonnull Long petId;
        private @jakarta.annotation.Nonnull File requiredFile;
        private @jakarta.annotation.Nullable String additionalMetadata;

        public UploadFileWithRequiredFileRequest() {}

        public UploadFileWithRequiredFileRequest(@jakarta.annotation.Nonnull Long petId, @jakarta.annotation.Nonnull File requiredFile, @jakarta.annotation.Nullable String additionalMetadata) {
            this.petId = petId;
            this.requiredFile = requiredFile;
            this.additionalMetadata = additionalMetadata;
        }

        public @jakarta.annotation.Nonnull Long petId() {
            return this.petId;
        }
        public UploadFileWithRequiredFileRequest petId(@jakarta.annotation.Nonnull Long petId) {
            this.petId = petId;
            return this;
        }

        public @jakarta.annotation.Nonnull File requiredFile() {
            return this.requiredFile;
        }
        public UploadFileWithRequiredFileRequest requiredFile(@jakarta.annotation.Nonnull File requiredFile) {
            this.requiredFile = requiredFile;
            return this;
        }

        public @jakarta.annotation.Nullable String additionalMetadata() {
            return this.additionalMetadata;
        }
        public UploadFileWithRequiredFileRequest additionalMetadata(@jakarta.annotation.Nullable String additionalMetadata) {
            this.additionalMetadata = additionalMetadata;
            return this;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) {
                return true;
            }
            if (o == null || getClass() != o.getClass()) {
                return false;
            }
            UploadFileWithRequiredFileRequest request = (UploadFileWithRequiredFileRequest) o;
            return Objects.equals(this.petId, request.petId()) &&
                Objects.equals(this.requiredFile, request.requiredFile()) &&
                Objects.equals(this.additionalMetadata, request.additionalMetadata());
        }

        @Override
        public int hashCode() {
            return Objects.hash(petId, requiredFile, additionalMetadata);
        }
    }

    /**
     * uploads an image (required)
     * 
     * <p><b>200</b> - successful operation
     * @param requestParameters The uploadFileWithRequiredFile request parameters as object
     * @return ModelApiResponse
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ModelApiResponse uploadFileWithRequiredFile(UploadFileWithRequiredFileRequest requestParameters) throws RestClientResponseException {
        return this.uploadFileWithRequiredFile(requestParameters.petId(), requestParameters.requiredFile(), requestParameters.additionalMetadata());
    }

    /**
     * uploads an image (required)
     * 
     * <p><b>200</b> - successful operation
     * @param requestParameters The uploadFileWithRequiredFile request parameters as object
     * @return ResponseEntity&lt;ModelApiResponse&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<ModelApiResponse> uploadFileWithRequiredFileWithHttpInfo(UploadFileWithRequiredFileRequest requestParameters) throws RestClientResponseException {
        return this.uploadFileWithRequiredFileWithHttpInfo(requestParameters.petId(), requestParameters.requiredFile(), requestParameters.additionalMetadata());
    }

    /**
     * uploads an image (required)
     * 
     * <p><b>200</b> - successful operation
     * @param requestParameters The uploadFileWithRequiredFile request parameters as object
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec uploadFileWithRequiredFileWithResponseSpec(UploadFileWithRequiredFileRequest requestParameters) throws RestClientResponseException {
        return this.uploadFileWithRequiredFileWithResponseSpec(requestParameters.petId(), requestParameters.requiredFile(), requestParameters.additionalMetadata());
    }

    /**
     * uploads an image (required)
     * 
     * <p><b>200</b> - successful operation
     * @param petId ID of pet to update
     * @param requiredFile file to upload
     * @param additionalMetadata Additional data to pass to server
     * @return ModelApiResponse
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec uploadFileWithRequiredFileRequestCreation(@jakarta.annotation.Nonnull Long petId, @jakarta.annotation.Nonnull File requiredFile, @jakarta.annotation.Nullable String additionalMetadata) throws RestClientResponseException {
        Object postBody = null;
        // verify the required parameter 'petId' is set
        if (petId == null) {
            throw new RestClientResponseException("Missing the required parameter 'petId' when calling uploadFileWithRequiredFile", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // verify the required parameter 'requiredFile' is set
        if (requiredFile == null) {
            throw new RestClientResponseException("Missing the required parameter 'requiredFile' when calling uploadFileWithRequiredFile", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        pathParams.put("petId", petId);

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

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

        ParameterizedTypeReference<ModelApiResponse> localVarReturnType = new ParameterizedTypeReference<>() {};
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
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ModelApiResponse uploadFileWithRequiredFile(@jakarta.annotation.Nonnull Long petId, @jakarta.annotation.Nonnull File requiredFile, @jakarta.annotation.Nullable String additionalMetadata) throws RestClientResponseException {
        ParameterizedTypeReference<ModelApiResponse> localVarReturnType = new ParameterizedTypeReference<>() {};
        return uploadFileWithRequiredFileRequestCreation(petId, requiredFile, additionalMetadata).body(localVarReturnType);
    }

    /**
     * uploads an image (required)
     * 
     * <p><b>200</b> - successful operation
     * @param petId ID of pet to update
     * @param requiredFile file to upload
     * @param additionalMetadata Additional data to pass to server
     * @return ResponseEntity&lt;ModelApiResponse&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<ModelApiResponse> uploadFileWithRequiredFileWithHttpInfo(@jakarta.annotation.Nonnull Long petId, @jakarta.annotation.Nonnull File requiredFile, @jakarta.annotation.Nullable String additionalMetadata) throws RestClientResponseException {
        ParameterizedTypeReference<ModelApiResponse> localVarReturnType = new ParameterizedTypeReference<>() {};
        return uploadFileWithRequiredFileRequestCreation(petId, requiredFile, additionalMetadata).toEntity(localVarReturnType);
    }

    /**
     * uploads an image (required)
     * 
     * <p><b>200</b> - successful operation
     * @param petId ID of pet to update
     * @param requiredFile file to upload
     * @param additionalMetadata Additional data to pass to server
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec uploadFileWithRequiredFileWithResponseSpec(@jakarta.annotation.Nonnull Long petId, @jakarta.annotation.Nonnull File requiredFile, @jakarta.annotation.Nullable String additionalMetadata) throws RestClientResponseException {
        return uploadFileWithRequiredFileRequestCreation(petId, requiredFile, additionalMetadata);
    }
}
