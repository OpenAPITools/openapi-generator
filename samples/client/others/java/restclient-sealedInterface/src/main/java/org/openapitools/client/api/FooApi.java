package org.openapitools.client.api;

import org.openapitools.client.ApiClient;

import org.openapitools.client.model.Foo;
import org.openapitools.client.model.FooRefOrValue;

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

@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen", comments = "Generator version: 7.22.0-SNAPSHOT")
public class FooApi {
    private ApiClient apiClient;

    public FooApi() {
        this(new ApiClient());
    }

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
     * @param foo The Foo to be created
     * @return FooRefOrValue
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec createFooRequestCreation(@jakarta.annotation.Nullable Foo foo) throws RestClientResponseException {
        Object postBody = foo;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = { 
            "application/json"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json;charset=utf-8"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<FooRefOrValue> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/foo", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Create a Foo
     * 
     * <p><b>201</b> - Error
     * @param foo The Foo to be created
     * @return FooRefOrValue
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public FooRefOrValue createFoo(@jakarta.annotation.Nullable Foo foo) throws RestClientResponseException {
        ParameterizedTypeReference<FooRefOrValue> localVarReturnType = new ParameterizedTypeReference<>() {};
        return createFooRequestCreation(foo).body(localVarReturnType);
    }

    /**
     * Create a Foo
     * 
     * <p><b>201</b> - Error
     * @param foo The Foo to be created
     * @return ResponseEntity&lt;FooRefOrValue&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<FooRefOrValue> createFooWithHttpInfo(@jakarta.annotation.Nullable Foo foo) throws RestClientResponseException {
        ParameterizedTypeReference<FooRefOrValue> localVarReturnType = new ParameterizedTypeReference<>() {};
        return createFooRequestCreation(foo).toEntity(localVarReturnType);
    }

    /**
     * Create a Foo
     * 
     * <p><b>201</b> - Error
     * @param foo The Foo to be created
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec createFooWithResponseSpec(@jakarta.annotation.Nullable Foo foo) throws RestClientResponseException {
        return createFooRequestCreation(foo);
    }

    /**
     * GET all Foos
     * 
     * <p><b>200</b> - Success
     * @return List&lt;FooRefOrValue&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec getAllFoosRequestCreation() throws RestClientResponseException {
        Object postBody = null;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = { 
            "application/json;charset=utf-8"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<List<FooRefOrValue>> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/foo", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * GET all Foos
     * 
     * <p><b>200</b> - Success
     * @return List&lt;FooRefOrValue&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public List<FooRefOrValue> getAllFoos() throws RestClientResponseException {
        ParameterizedTypeReference<List<FooRefOrValue>> localVarReturnType = new ParameterizedTypeReference<>() {};
        return getAllFoosRequestCreation().body(localVarReturnType);
    }

    /**
     * GET all Foos
     * 
     * <p><b>200</b> - Success
     * @return ResponseEntity&lt;List&lt;FooRefOrValue&gt;&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<List<FooRefOrValue>> getAllFoosWithHttpInfo() throws RestClientResponseException {
        ParameterizedTypeReference<List<FooRefOrValue>> localVarReturnType = new ParameterizedTypeReference<>() {};
        return getAllFoosRequestCreation().toEntity(localVarReturnType);
    }

    /**
     * GET all Foos
     * 
     * <p><b>200</b> - Success
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec getAllFoosWithResponseSpec() throws RestClientResponseException {
        return getAllFoosRequestCreation();
    }
}
