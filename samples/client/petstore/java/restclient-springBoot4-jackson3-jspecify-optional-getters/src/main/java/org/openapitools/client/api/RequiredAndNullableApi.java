package org.openapitools.client.api;

import org.openapitools.client.ApiClient;

import org.openapitools.client.model.RequiredAndNullable;

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

@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
public class RequiredAndNullableApi {
    private ApiClient apiClient;

    public RequiredAndNullableApi() {
        this(new ApiClient());
    }

    public RequiredAndNullableApi(ApiClient apiClient) {
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
     * <p><b>0</b> - response
     * @param requiredAndNullable bodyWithRequiredAndNullableAttributes
     * @return RequiredAndNullable
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec requiredAndNullablePostRequestCreation(RequiredAndNullable requiredAndNullable) throws RestClientResponseException {
        Object postBody = requiredAndNullable;
        // verify the required parameter 'requiredAndNullable' is set
        if (requiredAndNullable == null) {
            throw new RestClientResponseException("Missing the required parameter 'requiredAndNullable' when calling requiredAndNullablePost", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<>();
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

        ParameterizedTypeReference<RequiredAndNullable> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/requiredAndNullable", HttpMethod.POST, pathParams, localVarQueryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>0</b> - response
     * @param requiredAndNullable bodyWithRequiredAndNullableAttributes
     * @return RequiredAndNullable
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public RequiredAndNullable requiredAndNullablePost(RequiredAndNullable requiredAndNullable) throws RestClientResponseException {
        ParameterizedTypeReference<RequiredAndNullable> localVarReturnType = new ParameterizedTypeReference<>() {};
        return requiredAndNullablePostRequestCreation(requiredAndNullable).body(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>0</b> - response
     * @param requiredAndNullable bodyWithRequiredAndNullableAttributes
     * @return ResponseEntity&lt;RequiredAndNullable&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<RequiredAndNullable> requiredAndNullablePostWithHttpInfo(RequiredAndNullable requiredAndNullable) throws RestClientResponseException {
        ParameterizedTypeReference<RequiredAndNullable> localVarReturnType = new ParameterizedTypeReference<>() {};
        return requiredAndNullablePostRequestCreation(requiredAndNullable).toEntity(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>0</b> - response
     * @param requiredAndNullable bodyWithRequiredAndNullableAttributes
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec requiredAndNullablePostWithResponseSpec(RequiredAndNullable requiredAndNullable) throws RestClientResponseException {
        return requiredAndNullablePostRequestCreation(requiredAndNullable);
    }
}
