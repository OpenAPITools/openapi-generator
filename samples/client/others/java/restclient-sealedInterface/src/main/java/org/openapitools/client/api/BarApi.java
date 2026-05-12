package org.openapitools.client.api;

import org.openapitools.client.ApiClient;

import org.openapitools.client.model.Bar;
import org.openapitools.client.model.BarCreate;

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
public class BarApi {
    private ApiClient apiClient;

    public BarApi() {
        this(new ApiClient());
    }

    public BarApi(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    public ApiClient getApiClient() {
        return apiClient;
    }

    public void setApiClient(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    /**
     * Create a Bar
     * 
     * <p><b>200</b> - Bar created
     * @param barCreate The barCreate parameter
     * @return Bar
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec createBarRequestCreation(@jakarta.annotation.Nonnull BarCreate barCreate) throws RestClientResponseException {
        Object postBody = barCreate;
        // verify the required parameter 'barCreate' is set
        if (barCreate == null) {
            throw new RestClientResponseException("Missing the required parameter 'barCreate' when calling createBar", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
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
            "application/json"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Bar> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/bar", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Create a Bar
     * 
     * <p><b>200</b> - Bar created
     * @param barCreate The barCreate parameter
     * @return Bar
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public Bar createBar(@jakarta.annotation.Nonnull BarCreate barCreate) throws RestClientResponseException {
        ParameterizedTypeReference<Bar> localVarReturnType = new ParameterizedTypeReference<>() {};
        return createBarRequestCreation(barCreate).body(localVarReturnType);
    }

    /**
     * Create a Bar
     * 
     * <p><b>200</b> - Bar created
     * @param barCreate The barCreate parameter
     * @return ResponseEntity&lt;Bar&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Bar> createBarWithHttpInfo(@jakarta.annotation.Nonnull BarCreate barCreate) throws RestClientResponseException {
        ParameterizedTypeReference<Bar> localVarReturnType = new ParameterizedTypeReference<>() {};
        return createBarRequestCreation(barCreate).toEntity(localVarReturnType);
    }

    /**
     * Create a Bar
     * 
     * <p><b>200</b> - Bar created
     * @param barCreate The barCreate parameter
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec createBarWithResponseSpec(@jakarta.annotation.Nonnull BarCreate barCreate) throws RestClientResponseException {
        return createBarRequestCreation(barCreate);
    }
}
