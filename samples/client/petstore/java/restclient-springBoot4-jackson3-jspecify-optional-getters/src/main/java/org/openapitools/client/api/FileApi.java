package org.openapitools.client.api;

import org.openapitools.client.ApiClient;

import org.openapitools.client.model.FileContent;

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
     * <p><b>200</b> - ok
     * @param id The id parameter
     * @return FileContent
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec fileIdGetRequestCreation(String id) throws RestClientResponseException {
        Object postBody = null;
        // verify the required parameter 'id' is set
        if (id == null) {
            throw new RestClientResponseException("Missing the required parameter 'id' when calling fileIdGet", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        pathParams.put("id", id);

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<>();
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

        ParameterizedTypeReference<FileContent> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/file/{id}", HttpMethod.GET, pathParams, localVarQueryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - ok
     * @param id The id parameter
     * @return FileContent
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public FileContent fileIdGet(String id) throws RestClientResponseException {
        ParameterizedTypeReference<FileContent> localVarReturnType = new ParameterizedTypeReference<>() {};
        return fileIdGetRequestCreation(id).body(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - ok
     * @param id The id parameter
     * @return ResponseEntity&lt;FileContent&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<FileContent> fileIdGetWithHttpInfo(String id) throws RestClientResponseException {
        ParameterizedTypeReference<FileContent> localVarReturnType = new ParameterizedTypeReference<>() {};
        return fileIdGetRequestCreation(id).toEntity(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - ok
     * @param id The id parameter
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec fileIdGetWithResponseSpec(String id) throws RestClientResponseException {
        return fileIdGetRequestCreation(id);
    }
}
