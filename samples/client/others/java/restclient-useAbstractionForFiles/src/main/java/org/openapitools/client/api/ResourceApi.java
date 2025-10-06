package org.openapitools.client.api;

import org.openapitools.client.ApiClient;

import java.io.File;

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
public class ResourceApi {
    private ApiClient apiClient;

    public ResourceApi() {
        this(new ApiClient());
    }

    public ResourceApi(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    public ApiClient getApiClient() {
        return apiClient;
    }

    public void setApiClient(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    /**
     * 
     * Response file abstraction
     * <p><b>200</b> - Successful operation
     * @return File
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec resourceInResponseRequestCreation() throws RestClientResponseException {
        Object postBody = null;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = { 
            "application/octet-stream"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<org.springframework.core.io.Resource> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/resource", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * Response file abstraction
     * <p><b>200</b> - Successful operation
     * @return File
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public org.springframework.core.io.Resource resourceInResponse() throws RestClientResponseException {
        ParameterizedTypeReference<org.springframework.core.io.Resource> localVarReturnType = new ParameterizedTypeReference<>() {};
        return resourceInResponseRequestCreation().body(localVarReturnType);
    }

    /**
     * 
     * Response file abstraction
     * <p><b>200</b> - Successful operation
     * @return ResponseEntity&lt;File&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<org.springframework.core.io.Resource> resourceInResponseWithHttpInfo() throws RestClientResponseException {
        ParameterizedTypeReference<org.springframework.core.io.Resource> localVarReturnType = new ParameterizedTypeReference<>() {};
        return resourceInResponseRequestCreation().toEntity(localVarReturnType);
    }

    /**
     * 
     * Response file abstraction
     * <p><b>200</b> - Successful operation
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec resourceInResponseWithResponseSpec() throws RestClientResponseException {
        return resourceInResponseRequestCreation();
    }
}
