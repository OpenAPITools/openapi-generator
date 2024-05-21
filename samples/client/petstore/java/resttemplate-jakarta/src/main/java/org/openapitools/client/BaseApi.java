package org.openapitools.client;

import org.springframework.web.client.RestClientException;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;

@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen", comments = "Generator version: 7.7.0-SNAPSHOT")
public abstract class BaseApi {

    protected ApiClient apiClient;

    public BaseApi() {
        this(new ApiClient());
    }

    public BaseApi(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    public ApiClient getApiClient() {
        return apiClient;
    }

    public void setApiClient(ApiClient apiClient) {
        this.apiClient = apiClient;
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
    public abstract <T> ResponseEntity<T> invokeAPI(String url, HttpMethod method, Object request, ParameterizedTypeReference<T> returnType) throws RestClientException;
}
