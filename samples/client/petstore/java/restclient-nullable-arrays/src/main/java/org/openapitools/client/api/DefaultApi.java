package org.openapitools.client.api;

import org.openapitools.client.ApiClient;

import org.openapitools.client.model.ByteArrayObject;

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
public class DefaultApi {
    private ApiClient apiClient;

    public DefaultApi() {
        this(new ApiClient());
    }

    public DefaultApi(ApiClient apiClient) {
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
     * 
     * <p><b>200</b> - 
     * @return List&lt;ByteArrayObject&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec nullableArrayTestGetRequestCreation() throws RestClientResponseException {
        Object postBody = null;
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
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<List<ByteArrayObject>> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/nullable-array-test", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - 
     * @return List&lt;ByteArrayObject&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public List<ByteArrayObject> nullableArrayTestGet() throws RestClientResponseException {
        ParameterizedTypeReference<List<ByteArrayObject>> localVarReturnType = new ParameterizedTypeReference<>() {};
        return nullableArrayTestGetRequestCreation().body(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - 
     * @return ResponseEntity&lt;List&lt;ByteArrayObject&gt;&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<List<ByteArrayObject>> nullableArrayTestGetWithHttpInfo() throws RestClientResponseException {
        ParameterizedTypeReference<List<ByteArrayObject>> localVarReturnType = new ParameterizedTypeReference<>() {};
        return nullableArrayTestGetRequestCreation().toEntity(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - 
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec nullableArrayTestGetWithResponseSpec() throws RestClientResponseException {
        return nullableArrayTestGetRequestCreation();
    }
}
