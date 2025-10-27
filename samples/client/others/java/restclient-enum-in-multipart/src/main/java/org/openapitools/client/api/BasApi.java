package org.openapitools.client.api;

import org.openapitools.client.ApiClient;

import org.openapitools.client.model.DataChannel;
import org.openapitools.client.model.DataDirection;
import java.io.File;
import org.openapitools.client.model.InlineObject;

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
public class BasApi {
    private ApiClient apiClient;

    public BasApi() {
        this(new ApiClient());
    }

    public BasApi(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    public ApiClient getApiClient() {
        return apiClient;
    }

    public void setApiClient(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    /**
     * Creates a new message
     * Creates a new message
     * <p><b>201</b> - The message was created.
     * @param fileContent The message payload
     * @param idempotencyKey The idempotencyKey parameter
     * @param dataDirection The dataDirection parameter
     * @param dataChannel The dataChannel parameter
     * @return InlineObject
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec createMessageRequestCreation(@jakarta.annotation.Nonnull File fileContent, @jakarta.annotation.Nonnull String idempotencyKey, @jakarta.annotation.Nonnull DataDirection dataDirection, @jakarta.annotation.Nonnull DataChannel dataChannel) throws RestClientResponseException {
        Object postBody = null;
        // verify the required parameter 'fileContent' is set
        if (fileContent == null) {
            throw new RestClientResponseException("Missing the required parameter 'fileContent' when calling createMessage", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // verify the required parameter 'idempotencyKey' is set
        if (idempotencyKey == null) {
            throw new RestClientResponseException("Missing the required parameter 'idempotencyKey' when calling createMessage", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // verify the required parameter 'dataDirection' is set
        if (dataDirection == null) {
            throw new RestClientResponseException("Missing the required parameter 'dataDirection' when calling createMessage", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // verify the required parameter 'dataChannel' is set
        if (dataChannel == null) {
            throw new RestClientResponseException("Missing the required parameter 'dataChannel' when calling createMessage", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        if (fileContent != null)
            formParams.add("fileContent", new FileSystemResource(fileContent));
        if (idempotencyKey != null)
            formParams.add("idempotencyKey", idempotencyKey);
        if (dataDirection != null)
            formParams.add("dataDirection", dataDirection);
        if (dataChannel != null)
            formParams.add("dataChannel", dataChannel);

        final String[] localVarAccepts = { 
            "application/json"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "multipart/form-data"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] { "basicAuth" };

        ParameterizedTypeReference<InlineObject> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/messages", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Creates a new message
     * Creates a new message
     * <p><b>201</b> - The message was created.
     * @param fileContent The message payload
     * @param idempotencyKey The idempotencyKey parameter
     * @param dataDirection The dataDirection parameter
     * @param dataChannel The dataChannel parameter
     * @return InlineObject
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public InlineObject createMessage(@jakarta.annotation.Nonnull File fileContent, @jakarta.annotation.Nonnull String idempotencyKey, @jakarta.annotation.Nonnull DataDirection dataDirection, @jakarta.annotation.Nonnull DataChannel dataChannel) throws RestClientResponseException {
        ParameterizedTypeReference<InlineObject> localVarReturnType = new ParameterizedTypeReference<>() {};
        return createMessageRequestCreation(fileContent, idempotencyKey, dataDirection, dataChannel).body(localVarReturnType);
    }

    /**
     * Creates a new message
     * Creates a new message
     * <p><b>201</b> - The message was created.
     * @param fileContent The message payload
     * @param idempotencyKey The idempotencyKey parameter
     * @param dataDirection The dataDirection parameter
     * @param dataChannel The dataChannel parameter
     * @return ResponseEntity&lt;InlineObject&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<InlineObject> createMessageWithHttpInfo(@jakarta.annotation.Nonnull File fileContent, @jakarta.annotation.Nonnull String idempotencyKey, @jakarta.annotation.Nonnull DataDirection dataDirection, @jakarta.annotation.Nonnull DataChannel dataChannel) throws RestClientResponseException {
        ParameterizedTypeReference<InlineObject> localVarReturnType = new ParameterizedTypeReference<>() {};
        return createMessageRequestCreation(fileContent, idempotencyKey, dataDirection, dataChannel).toEntity(localVarReturnType);
    }

    /**
     * Creates a new message
     * Creates a new message
     * <p><b>201</b> - The message was created.
     * @param fileContent The message payload
     * @param idempotencyKey The idempotencyKey parameter
     * @param dataDirection The dataDirection parameter
     * @param dataChannel The dataChannel parameter
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec createMessageWithResponseSpec(@jakarta.annotation.Nonnull File fileContent, @jakarta.annotation.Nonnull String idempotencyKey, @jakarta.annotation.Nonnull DataDirection dataDirection, @jakarta.annotation.Nonnull DataChannel dataChannel) throws RestClientResponseException {
        return createMessageRequestCreation(fileContent, idempotencyKey, dataDirection, dataChannel);
    }
}
