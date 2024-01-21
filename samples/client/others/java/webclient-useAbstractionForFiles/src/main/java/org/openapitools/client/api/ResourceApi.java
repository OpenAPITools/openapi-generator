package org.openapitools.client.api;

import org.openapitools.client.ApiClient;

import java.io.File;

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
public class ResourceApi {
    private ApiClient apiClient;

    public ResourceApi() {
        this(new ApiClient());
    }

    @Autowired
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
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec resourceInResponseRequestCreation() throws WebClientResponseException {
        Object postBody = null;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = { 
            "application/octet-stream"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<org.springframework.core.io.Resource> localVarReturnType = new ParameterizedTypeReference<org.springframework.core.io.Resource>() {};
        return apiClient.invokeAPI("/resource", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * Response file abstraction
     * <p><b>200</b> - Successful operation
     * @return File
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<org.springframework.core.io.Resource> resourceInResponse() throws WebClientResponseException {
        ParameterizedTypeReference<org.springframework.core.io.Resource> localVarReturnType = new ParameterizedTypeReference<org.springframework.core.io.Resource>() {};
        return resourceInResponseRequestCreation().bodyToMono(localVarReturnType);
    }

    /**
     * 
     * Response file abstraction
     * <p><b>200</b> - Successful operation
     * @return ResponseEntity&lt;File&gt;
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<org.springframework.core.io.Resource>> resourceInResponseWithHttpInfo() throws WebClientResponseException {
        ParameterizedTypeReference<org.springframework.core.io.Resource> localVarReturnType = new ParameterizedTypeReference<org.springframework.core.io.Resource>() {};
        return resourceInResponseRequestCreation().toEntity(localVarReturnType);
    }

    /**
     * 
     * Response file abstraction
     * <p><b>200</b> - Successful operation
     * @return ResponseSpec
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec resourceInResponseWithResponseSpec() throws WebClientResponseException {
        return resourceInResponseRequestCreation();
    }
}
