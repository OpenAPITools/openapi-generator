package org.openapitools.client.api;

import org.openapitools.client.ApiClient;

import org.openapitools.client.model.Pet;

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

@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen", comments = "Generator version: 7.23.0-SNAPSHOT")
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
     * <p><b>200</b> - successful operation
     * <p><b>405</b> - Invalid input
     * @param pet Pet object that needs to be added to the store
     * @return Pet
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

        final String[] localVarAccepts = { 
            "application/xml", "application/json"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json", "application/xml"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] { "petstore_auth" };

        ParameterizedTypeReference<Pet> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/pet", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Add a new pet to the store
     * 
     * <p><b>200</b> - successful operation
     * <p><b>405</b> - Invalid input
     * @param pet Pet object that needs to be added to the store
     * @return Pet
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public Pet addPet(@jakarta.annotation.Nonnull Pet pet) throws RestClientResponseException {
        ParameterizedTypeReference<Pet> localVarReturnType = new ParameterizedTypeReference<>() {};
        return addPetRequestCreation(pet).body(localVarReturnType);
    }

    /**
     * Add a new pet to the store
     * 
     * <p><b>200</b> - successful operation
     * <p><b>405</b> - Invalid input
     * @param pet Pet object that needs to be added to the store
     * @return ResponseEntity&lt;Pet&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Pet> addPetWithHttpInfo(@jakarta.annotation.Nonnull Pet pet) throws RestClientResponseException {
        ParameterizedTypeReference<Pet> localVarReturnType = new ParameterizedTypeReference<>() {};
        return addPetRequestCreation(pet).toEntity(localVarReturnType);
    }

    /**
     * Add a new pet to the store
     * 
     * <p><b>200</b> - successful operation
     * <p><b>405</b> - Invalid input
     * @param pet Pet object that needs to be added to the store
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec addPetWithResponseSpec(@jakarta.annotation.Nonnull Pet pet) throws RestClientResponseException {
        return addPetRequestCreation(pet);
    }
}
