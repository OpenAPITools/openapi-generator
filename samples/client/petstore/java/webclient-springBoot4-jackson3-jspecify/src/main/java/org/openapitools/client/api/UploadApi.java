package org.openapitools.client.api;

import org.openapitools.client.ApiClient;

import java.io.File;
import org.jspecify.annotations.Nullable;

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
public class UploadApi {
    private ApiClient apiClient;

    public UploadApi() {
        this(new ApiClient());
    }

    public UploadApi(ApiClient apiClient) {
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
     * <p><b>0</b> - ok
     * @param _file The _file parameter
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec uploadFilesPostRequestCreation(java.util.@Nullable Collection<org.springframework.core.io.Resource> _file) throws WebClientResponseException {
        Object postBody = null;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        if (_file != null)
            formParams.addAll("file", _file.stream().collect(Collectors.toList()));

        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "multipart/form-data"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return apiClient.invokeAPI("/uploadFiles", HttpMethod.POST, pathParams, localVarQueryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>0</b> - ok
     * @param _file The _file parameter
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Void> uploadFilesPost(java.util.@Nullable Collection<org.springframework.core.io.Resource> _file) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return uploadFilesPostRequestCreation(_file).bodyToMono(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>0</b> - ok
     * @param _file The _file parameter
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<Void>> uploadFilesPostWithHttpInfo(java.util.@Nullable Collection<org.springframework.core.io.Resource> _file) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return uploadFilesPostRequestCreation(_file).toEntity(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>0</b> - ok
     * @param _file The _file parameter
     * @return ResponseSpec
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec uploadFilesPostWithResponseSpec(java.util.@Nullable Collection<org.springframework.core.io.Resource> _file) throws WebClientResponseException {
        return uploadFilesPostRequestCreation(_file);
    }

    /**
     * 
     * 
     * <p><b>0</b> - ok
     * @param _file The _file parameter
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec uploadPostRequestCreation(org.springframework.core.io.@Nullable Resource _file) throws WebClientResponseException {
        Object postBody = null;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        if (_file != null)
            formParams.add("file", _file);

        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "multipart/form-data"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return apiClient.invokeAPI("/upload", HttpMethod.POST, pathParams, localVarQueryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>0</b> - ok
     * @param _file The _file parameter
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Void> uploadPost(org.springframework.core.io.@Nullable Resource _file) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return uploadPostRequestCreation(_file).bodyToMono(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>0</b> - ok
     * @param _file The _file parameter
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<Void>> uploadPostWithHttpInfo(org.springframework.core.io.@Nullable Resource _file) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return uploadPostRequestCreation(_file).toEntity(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>0</b> - ok
     * @param _file The _file parameter
     * @return ResponseSpec
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec uploadPostWithResponseSpec(org.springframework.core.io.@Nullable Resource _file) throws WebClientResponseException {
        return uploadPostRequestCreation(_file);
    }
}
