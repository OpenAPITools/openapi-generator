package org.openapitools.client.api;

import org.openapitools.client.ApiClient;

import org.openapitools.client.model.Pet;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.core.io.FileSystemResource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen")
@Component("org.openapitools.client.api.BodyApi")
public class BodyApi {
    private ApiClient apiClient;

    public BodyApi() {
        this(new ApiClient());
    }

    @Autowired
    public BodyApi(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    public ApiClient getApiClient() {
        return apiClient;
    }

    public void setApiClient(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    /**
     * Test body parameter(s)
     * Test body parameter(s)
     * <p><b>200</b> - Successful operation
     * @param pet Pet object that needs to be added to the store (optional)
     * @return Pet
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public Pet testEchoBodyPet(Pet pet) throws RestClientException {
        return testEchoBodyPetWithHttpInfo(pet).getBody();
    }

    /**
     * Test body parameter(s)
     * Test body parameter(s)
     * <p><b>200</b> - Successful operation
     * @param pet Pet object that needs to be added to the store (optional)
     * @return ResponseEntity&lt;Pet&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Pet> testEchoBodyPetWithHttpInfo(Pet pet) throws RestClientException {
        Object localVarPostBody = pet;
        

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders localVarHeaderParams = new HttpHeaders();
        final MultiValueMap<String, String> localVarCookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> localVarFormParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = { 
            "application/json"
         };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
         };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Pet> localReturnType = new ParameterizedTypeReference<Pet>() {};
        return apiClient.invokeAPI("/echo/body/Pet", HttpMethod.POST, Collections.<String, Object>emptyMap(), localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localReturnType);
    }
}
