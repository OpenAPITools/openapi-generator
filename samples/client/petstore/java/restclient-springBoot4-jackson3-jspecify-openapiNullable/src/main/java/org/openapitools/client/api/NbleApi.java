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
import org.springframework.web.client.RestClient.ResponseSpec;
import org.springframework.web.client.RestClientResponseException;

@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
public class NbleApi {
    private ApiClient apiClient;

    public NbleApi() {
        this(new ApiClient());
    }

    public NbleApi(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    public ApiClient getApiClient() {
        return apiClient;
    }

    public void setApiClient(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    public record NbleGetRequest(@Nullable String mandatory, @Nullable String optional){}

    /**
     * 
     * 
     * <p><b>0</b> - OK
     * @param requestParameters The nbleGet request parameters as object
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public void nbleGet(NbleGetRequest requestParameters) throws RestClientResponseException {
        this.nbleGet(requestParameters.mandatory(), requestParameters.optional());
    }

    /**
     * 
     * 
     * <p><b>0</b> - OK
     * @param requestParameters The nbleGet request parameters as object
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> nbleGetWithHttpInfo(NbleGetRequest requestParameters) throws RestClientResponseException {
        return this.nbleGetWithHttpInfo(requestParameters.mandatory(), requestParameters.optional());
    }

    /**
     * 
     * 
     * <p><b>0</b> - OK
     * @param requestParameters The nbleGet request parameters as object
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec nbleGetWithResponseSpec(NbleGetRequest requestParameters) throws RestClientResponseException {
        return this.nbleGetWithResponseSpec(requestParameters.mandatory(), requestParameters.optional());
    }

    /**
     * 
     * 
     * <p><b>0</b> - OK
     * @param mandatory The mandatory parameter
     * @param optional The optional parameter
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec nbleGetRequestCreation(@Nullable String mandatory, @Nullable String optional) throws RestClientResponseException {
        Object postBody = null;
        // verify the required parameter 'mandatory' is set
        if (mandatory == null) {
            throw new RestClientResponseException("Missing the required parameter 'mandatory' when calling nbleGet", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        localVarQueryParams.putAll(apiClient.parameterToMultiValueMap(null, "optional", optional));
        localVarQueryParams.putAll(apiClient.parameterToMultiValueMap(null, "mandatory", mandatory));

        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/nble", HttpMethod.GET, pathParams, localVarQueryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>0</b> - OK
     * @param mandatory The mandatory parameter
     * @param optional The optional parameter
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public void nbleGet(@Nullable String mandatory, @Nullable String optional) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        nbleGetRequestCreation(mandatory, optional).body(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>0</b> - OK
     * @param mandatory The mandatory parameter
     * @param optional The optional parameter
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> nbleGetWithHttpInfo(@Nullable String mandatory, @Nullable String optional) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return nbleGetRequestCreation(mandatory, optional).toEntity(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>0</b> - OK
     * @param mandatory The mandatory parameter
     * @param optional The optional parameter
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec nbleGetWithResponseSpec(@Nullable String mandatory, @Nullable String optional) throws RestClientResponseException {
        return nbleGetRequestCreation(mandatory, optional);
    }

    /**
     * 
     * 
     * <p><b>0</b> - response
     * @param requiredAndNullable bodyWithRequiredAndNullableAttributes
     * @return RequiredAndNullable
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec nblePostRequestCreation(RequiredAndNullable requiredAndNullable) throws RestClientResponseException {
        Object postBody = requiredAndNullable;
        // verify the required parameter 'requiredAndNullable' is set
        if (requiredAndNullable == null) {
            throw new RestClientResponseException("Missing the required parameter 'requiredAndNullable' when calling nblePost", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
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
        return apiClient.invokeAPI("/nble", HttpMethod.POST, pathParams, localVarQueryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>0</b> - response
     * @param requiredAndNullable bodyWithRequiredAndNullableAttributes
     * @return RequiredAndNullable
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public RequiredAndNullable nblePost(RequiredAndNullable requiredAndNullable) throws RestClientResponseException {
        ParameterizedTypeReference<RequiredAndNullable> localVarReturnType = new ParameterizedTypeReference<>() {};
        return nblePostRequestCreation(requiredAndNullable).body(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>0</b> - response
     * @param requiredAndNullable bodyWithRequiredAndNullableAttributes
     * @return ResponseEntity&lt;RequiredAndNullable&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<RequiredAndNullable> nblePostWithHttpInfo(RequiredAndNullable requiredAndNullable) throws RestClientResponseException {
        ParameterizedTypeReference<RequiredAndNullable> localVarReturnType = new ParameterizedTypeReference<>() {};
        return nblePostRequestCreation(requiredAndNullable).toEntity(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>0</b> - response
     * @param requiredAndNullable bodyWithRequiredAndNullableAttributes
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec nblePostWithResponseSpec(RequiredAndNullable requiredAndNullable) throws RestClientResponseException {
        return nblePostRequestCreation(requiredAndNullable);
    }
}
