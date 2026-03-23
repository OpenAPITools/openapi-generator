package org.openapitools.client.api;

import org.openapitools.client.ApiClient;

import org.openapitools.client.model.FakeWebhooksSourcesDeletedPostRequest;

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

@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen", comments = "Generator version: 7.21.0-SNAPSHOT")
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
     * <p><b>200</b> - successful operation
     * <p><b>405</b> - Invalid input
     * @param fakeWebhooksSourcesDeletedPostRequest The fakeWebhooksSourcesDeletedPostRequest parameter
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec fakeWebhooksSourcesDeletedPostRequestCreation(@jakarta.annotation.Nullable FakeWebhooksSourcesDeletedPostRequest fakeWebhooksSourcesDeletedPostRequest) throws RestClientResponseException {
        Object postBody = fakeWebhooksSourcesDeletedPostRequest;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/fake/webhooks/sources/deleted", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - successful operation
     * <p><b>405</b> - Invalid input
     * @param fakeWebhooksSourcesDeletedPostRequest The fakeWebhooksSourcesDeletedPostRequest parameter
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public void fakeWebhooksSourcesDeletedPost(@jakarta.annotation.Nullable FakeWebhooksSourcesDeletedPostRequest fakeWebhooksSourcesDeletedPostRequest) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        fakeWebhooksSourcesDeletedPostRequestCreation(fakeWebhooksSourcesDeletedPostRequest).body(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - successful operation
     * <p><b>405</b> - Invalid input
     * @param fakeWebhooksSourcesDeletedPostRequest The fakeWebhooksSourcesDeletedPostRequest parameter
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> fakeWebhooksSourcesDeletedPostWithHttpInfo(@jakarta.annotation.Nullable FakeWebhooksSourcesDeletedPostRequest fakeWebhooksSourcesDeletedPostRequest) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return fakeWebhooksSourcesDeletedPostRequestCreation(fakeWebhooksSourcesDeletedPostRequest).toEntity(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - successful operation
     * <p><b>405</b> - Invalid input
     * @param fakeWebhooksSourcesDeletedPostRequest The fakeWebhooksSourcesDeletedPostRequest parameter
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec fakeWebhooksSourcesDeletedPostWithResponseSpec(@jakarta.annotation.Nullable FakeWebhooksSourcesDeletedPostRequest fakeWebhooksSourcesDeletedPostRequest) throws RestClientResponseException {
        return fakeWebhooksSourcesDeletedPostRequestCreation(fakeWebhooksSourcesDeletedPostRequest);
    }
}
