package org.openapitools.client.api;

import org.openapitools.client.ApiClient;

import java.io.File;

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

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen", comments = "Generator version: 7.5.0-SNAPSHOT")
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
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public org.springframework.core.io.Resource resourceInResponse() throws RestClientException {
        return resourceInResponseWithHttpInfo().getBody();
    }

    /**
     * 
     * Response file abstraction
     * <p><b>200</b> - Successful operation
     * @return ResponseEntity&lt;File&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<org.springframework.core.io.Resource> resourceInResponseWithHttpInfo() throws RestClientException {
        Object localVarPostBody = null;
        

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders localVarHeaderParams = new HttpHeaders();
        final MultiValueMap<String, String> localVarCookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> localVarFormParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = { 
            "application/octet-stream"
         };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = {  };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<org.springframework.core.io.Resource> localReturnType = new ParameterizedTypeReference<org.springframework.core.io.Resource>() {};
        return apiClient.invokeAPI("/resource", HttpMethod.GET, Collections.<String, Object>emptyMap(), localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localReturnType);
    }

    /**
     * Directly invoke the API for the given URL. Useful if the API returns direct links/URLs for subsequent requests.
     * @param url The URL for the request, either full URL or only the path.
     * @param method The HTTP method for the request.
     * @return ResponseEntity&lt;Void&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> invokeAPI(String url, HttpMethod method) throws RestClientException {
        return invokeAPI(url, method, null, new ParameterizedTypeReference<Void>() {});
    }

    /**
     * Directly invoke the API for the given URL. Useful if the API returns direct links/URLs for subsequent requests.
     * @param url The URL for the request, either full URL or only the path.
     * @param method The HTTP method for the request.
     * @param request The request object.
     * @return ResponseEntity&lt;Void&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> invokeAPI(String url, HttpMethod method, Object request) throws RestClientException {
        return invokeAPI(url, method, request, new ParameterizedTypeReference<Void>() {});
    }

    /**
     * Directly invoke the API for the given URL. Useful if the API returns direct links/URLs for subsequent requests.
     * @param url The URL for the request, either full URL or only the path.
     * @param method The HTTP method for the request.
     * @param returnType The return type.
     * @return ResponseEntity in the specified type.
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public <T> ResponseEntity<T> invokeAPI(String url, HttpMethod method, ParameterizedTypeReference<T> returnType) throws RestClientException {
        return invokeAPI(url, method, null, returnType);
    }

    /**
     * Directly invoke the API for the given URL. Useful if the API returns direct links/URLs for subsequent requests.
     * @param url The URL for the request, either full URL or only the path.
     * @param method The HTTP method for the request.
     * @param request The request object.
     * @param returnType The return type.
     * @return ResponseEntity in the specified type.
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public <T> ResponseEntity<T> invokeAPI(String url, HttpMethod method, Object request, ParameterizedTypeReference<T> returnType) throws RestClientException {
        String localVarPath = url.replace(apiClient.getBasePath(), "");
        Object localVarPostBody = request;

        final Map<String, Object> uriVariables = new HashMap<String, Object>();
        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders localVarHeaderParams = new HttpHeaders();
        final MultiValueMap<String, String> localVarCookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> localVarFormParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = { 
            "application/octet-stream"
         };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = {  };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        return apiClient.invokeAPI(localVarPath, method, uriVariables, localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, returnType);
    }
}
