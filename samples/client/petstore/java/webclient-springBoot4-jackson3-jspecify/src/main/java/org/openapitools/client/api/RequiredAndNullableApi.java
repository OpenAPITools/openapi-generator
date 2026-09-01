package org.openapitools.client.api;

import org.openapitools.client.ApiClient;

import org.jspecify.annotations.Nullable;
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
import org.springframework.web.reactive.function.client.WebClient.ResponseSpec;
import org.springframework.web.reactive.function.client.WebClientResponseException;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Flux;

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
     * <p><b>0</b> - OK
     * @param filter The filter parameter
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec requiredAndNullableGetRequestCreation(@Nullable String filter) throws WebClientResponseException {
        Object postBody = null;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        localVarQueryParams.putAll(apiClient.parameterToMultiValueMap(null, "filter", filter));

        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return apiClient.invokeAPI("/requiredAndNullable", HttpMethod.GET, pathParams, localVarQueryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>0</b> - OK
     * @param filter The filter parameter
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Void> requiredAndNullableGet(@Nullable String filter) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return requiredAndNullableGetRequestCreation(filter).bodyToMono(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>0</b> - OK
     * @param filter The filter parameter
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<Void>> requiredAndNullableGetWithHttpInfo(@Nullable String filter) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return requiredAndNullableGetRequestCreation(filter).toEntity(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>0</b> - OK
     * @param filter The filter parameter
     * @return ResponseSpec
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec requiredAndNullableGetWithResponseSpec(@Nullable String filter) throws WebClientResponseException {
        return requiredAndNullableGetRequestCreation(filter);
    }

    /**
     * 
     * 
     * <p><b>0</b> - response
     * @param requiredAndNullable bodyWithRequiredAndNullableAttributes
     * @return RequiredAndNullable
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec requiredAndNullablePostRequestCreation(RequiredAndNullable requiredAndNullable) throws WebClientResponseException {
        Object postBody = requiredAndNullable;
        // verify the required parameter 'requiredAndNullable' is set
        if (requiredAndNullable == null) {
            throw new WebClientResponseException("Missing the required parameter 'requiredAndNullable' when calling requiredAndNullablePost", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = { 
            "application/json"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<RequiredAndNullable> localVarReturnType = new ParameterizedTypeReference<RequiredAndNullable>() {};
        return apiClient.invokeAPI("/requiredAndNullable", HttpMethod.POST, pathParams, localVarQueryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>0</b> - response
     * @param requiredAndNullable bodyWithRequiredAndNullableAttributes
     * @return RequiredAndNullable
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<RequiredAndNullable> requiredAndNullablePost(RequiredAndNullable requiredAndNullable) throws WebClientResponseException {
        ParameterizedTypeReference<RequiredAndNullable> localVarReturnType = new ParameterizedTypeReference<RequiredAndNullable>() {};
        return requiredAndNullablePostRequestCreation(requiredAndNullable).bodyToMono(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>0</b> - response
     * @param requiredAndNullable bodyWithRequiredAndNullableAttributes
     * @return ResponseEntity&lt;RequiredAndNullable&gt;
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<RequiredAndNullable>> requiredAndNullablePostWithHttpInfo(RequiredAndNullable requiredAndNullable) throws WebClientResponseException {
        ParameterizedTypeReference<RequiredAndNullable> localVarReturnType = new ParameterizedTypeReference<RequiredAndNullable>() {};
        return requiredAndNullablePostRequestCreation(requiredAndNullable).toEntity(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>0</b> - response
     * @param requiredAndNullable bodyWithRequiredAndNullableAttributes
     * @return ResponseSpec
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec requiredAndNullablePostWithResponseSpec(RequiredAndNullable requiredAndNullable) throws WebClientResponseException {
        return requiredAndNullablePostRequestCreation(requiredAndNullable);
    }
}
