package org.openapitools.client.api;

import org.openapitools.client.ApiClient;

import java.io.File;
import org.openapitools.client.model.StructuredType;

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

@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen", comments = "Generator version: 7.19.0-SNAPSHOT")
public class FileApi {
    private ApiClient apiClient;

    public FileApi() {
        this(new ApiClient());
    }

    public FileApi(ApiClient apiClient) {
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
     * <p><b>201</b> - File created successfully
     * @param documentBytes The documentBytes parameter
     * @param documentType The documentType parameter
     * @param properties The properties parameter
     * @param structured The structured parameter
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec createFileRequestCreation(@jakarta.annotation.Nonnull File documentBytes, @jakarta.annotation.Nonnull String documentType, @jakarta.annotation.Nonnull Map<String, String> properties, @jakarta.annotation.Nullable StructuredType structured) throws RestClientResponseException {
        Object postBody = null;
        // verify the required parameter 'documentBytes' is set
        if (documentBytes == null) {
            throw new RestClientResponseException("Missing the required parameter 'documentBytes' when calling createFile", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // verify the required parameter 'documentType' is set
        if (documentType == null) {
            throw new RestClientResponseException("Missing the required parameter 'documentType' when calling createFile", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // verify the required parameter 'properties' is set
        if (properties == null) {
            throw new RestClientResponseException("Missing the required parameter 'properties' when calling createFile", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        if (documentBytes != null)
            formParams.add("documentBytes", new FileSystemResource(documentBytes));
        if (documentType != null)
            formParams.add("documentType", documentType);
        if (structured != null)
            formParams.add("structured", structured);
        if (properties != null)
            formParams.add("properties", properties);

        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "multipart/form-data"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/api/v1/file", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>201</b> - File created successfully
     * @param documentBytes The documentBytes parameter
     * @param documentType The documentType parameter
     * @param properties The properties parameter
     * @param structured The structured parameter
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public void createFile(@jakarta.annotation.Nonnull File documentBytes, @jakarta.annotation.Nonnull String documentType, @jakarta.annotation.Nonnull Map<String, String> properties, @jakarta.annotation.Nullable StructuredType structured) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        createFileRequestCreation(documentBytes, documentType, properties, structured).body(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>201</b> - File created successfully
     * @param documentBytes The documentBytes parameter
     * @param documentType The documentType parameter
     * @param properties The properties parameter
     * @param structured The structured parameter
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> createFileWithHttpInfo(@jakarta.annotation.Nonnull File documentBytes, @jakarta.annotation.Nonnull String documentType, @jakarta.annotation.Nonnull Map<String, String> properties, @jakarta.annotation.Nullable StructuredType structured) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return createFileRequestCreation(documentBytes, documentType, properties, structured).toEntity(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>201</b> - File created successfully
     * @param documentBytes The documentBytes parameter
     * @param documentType The documentType parameter
     * @param properties The properties parameter
     * @param structured The structured parameter
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec createFileWithResponseSpec(@jakarta.annotation.Nonnull File documentBytes, @jakarta.annotation.Nonnull String documentType, @jakarta.annotation.Nonnull Map<String, String> properties, @jakarta.annotation.Nullable StructuredType structured) throws RestClientResponseException {
        return createFileRequestCreation(documentBytes, documentType, properties, structured);
    }
}
