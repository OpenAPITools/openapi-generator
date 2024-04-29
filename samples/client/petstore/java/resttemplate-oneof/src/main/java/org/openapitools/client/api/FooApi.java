package org.openapitools.client.api;

import org.openapitools.client.ApiClient;

import org.openapitools.client.model.Foo;
import org.openapitools.client.model.FooRefOrValue;

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
@Component("org.openapitools.client.api.FooApi")
public class FooApi {
    private ApiClient apiClient;

    public FooApi() {
        this(new ApiClient());
    }

    @Autowired
    public FooApi(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    public ApiClient getApiClient() {
        return apiClient;
    }

    public void setApiClient(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    /**
     * Create a Foo
     * 
     * <p><b>201</b> - Error
     * @param foo The Foo to be created (optional)
     * @return FooRefOrValue
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public FooRefOrValue createFoo(Foo foo) throws RestClientException {
        return createFooWithHttpInfo(foo).getBody();
    }

    /**
     * Create a Foo
     * 
     * <p><b>201</b> - Error
     * @param foo The Foo to be created (optional)
     * @return ResponseEntity&lt;FooRefOrValue&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<FooRefOrValue> createFooWithHttpInfo(Foo foo) throws RestClientException {
        Object localVarPostBody = foo;
        

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders localVarHeaderParams = new HttpHeaders();
        final MultiValueMap<String, String> localVarCookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> localVarFormParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = { 
            "application/json"
         };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json;charset=utf-8"
         };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<FooRefOrValue> localReturnType = new ParameterizedTypeReference<FooRefOrValue>() {};
        return apiClient.invokeAPI("/foo", HttpMethod.POST, Collections.<String, Object>emptyMap(), localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localReturnType);
    }
    /**
     * GET all Foos
     * 
     * <p><b>200</b> - Success
     * @return List&lt;FooRefOrValue&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public List<FooRefOrValue> getAllFoos() throws RestClientException {
        return getAllFoosWithHttpInfo().getBody();
    }

    /**
     * GET all Foos
     * 
     * <p><b>200</b> - Success
     * @return ResponseEntity&lt;List&lt;FooRefOrValue&gt;&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<List<FooRefOrValue>> getAllFoosWithHttpInfo() throws RestClientException {
        Object localVarPostBody = null;
        

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders localVarHeaderParams = new HttpHeaders();
        final MultiValueMap<String, String> localVarCookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> localVarFormParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = { 
            "application/json;charset=utf-8"
         };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = {  };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<List<FooRefOrValue>> localReturnType = new ParameterizedTypeReference<List<FooRefOrValue>>() {};
        return apiClient.invokeAPI("/foo", HttpMethod.GET, Collections.<String, Object>emptyMap(), localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localReturnType);
    }
}
