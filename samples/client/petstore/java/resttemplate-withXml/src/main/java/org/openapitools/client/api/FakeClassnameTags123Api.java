package org.openapitools.client.api;

import org.openapitools.client.ApiClient;

import org.openapitools.client.model.Client;

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
public class FakeClassnameTags123Api {
    private ApiClient apiClient;

    public FakeClassnameTags123Api() {
        this(new ApiClient());
    }

    public FakeClassnameTags123Api(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    public ApiClient getApiClient() {
        return apiClient;
    }

    public void setApiClient(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    /**
     * To test class name in snake case
     * To test class name in snake case
     * <p><b>200</b> - successful operation
     * @param client client model (required)
     * @return Client
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public Client testClassname(Client client) throws RestClientException {
        return testClassnameWithHttpInfo(client).getBody();
    }

    /**
     * To test class name in snake case
     * To test class name in snake case
     * <p><b>200</b> - successful operation
     * @param client client model (required)
     * @return ResponseEntity&lt;Client&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Client> testClassnameWithHttpInfo(Client client) throws RestClientException {
        Object localVarPostBody = client;
        
        // verify the required parameter 'client' is set
        if (client == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'client' when calling testClassname");
        }
        

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

        String[] localVarAuthNames = new String[] { "api_key_query" };

        ParameterizedTypeReference<Client> localReturnType = new ParameterizedTypeReference<Client>() {};
        return apiClient.invokeAPI("/fake_classname_test", HttpMethod.PATCH, Collections.<String, Object>emptyMap(), localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localReturnType);
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
            "application/json"
         };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
         };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] { "api_key_query" };

        return apiClient.invokeAPI(localVarPath, method, uriVariables, localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, returnType);
    }
}
