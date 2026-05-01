package org.openapitools.client.api;

import org.openapitools.client.ApiClient;

import java.io.File;
import org.openapitools.client.model.Foo;
import org.jspecify.annotations.Nullable;
import java.time.OffsetDateTime;
import org.openapitools.client.model.UploadPostDefaultResponse;

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

@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen", comments = "Generator version: 7.23.0-SNAPSHOT")
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
     * <p><b>200</b> - ok
     * @param id The id parameter
     * @return File
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec downloadIdGetRequestCreation(String id) throws WebClientResponseException {
        Object postBody = null;
        // verify the required parameter 'id' is set
        if (id == null) {
            throw new WebClientResponseException("Missing the required parameter 'id' when calling downloadIdGet", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        pathParams.put("id", id);

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = { 
            "application/octet-stream"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<File> localVarReturnType = new ParameterizedTypeReference<File>() {};
        return apiClient.invokeAPI("/download/{id}", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - ok
     * @param id The id parameter
     * @return File
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<File> downloadIdGet(String id) throws WebClientResponseException {
        ParameterizedTypeReference<File> localVarReturnType = new ParameterizedTypeReference<File>() {};
        return downloadIdGetRequestCreation(id).bodyToMono(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - ok
     * @param id The id parameter
     * @return ResponseEntity&lt;File&gt;
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<File>> downloadIdGetWithHttpInfo(String id) throws WebClientResponseException {
        ParameterizedTypeReference<File> localVarReturnType = new ParameterizedTypeReference<File>() {};
        return downloadIdGetRequestCreation(id).toEntity(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - ok
     * @param id The id parameter
     * @return ResponseSpec
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec downloadIdGetWithResponseSpec(String id) throws WebClientResponseException {
        return downloadIdGetRequestCreation(id);
    }

    /**
     * 
     * 
     * <p><b>0</b> - response
     * @param dtParam The dtParam parameter
     * @param dtQuery The dtQuery parameter
     * @param dtCookie The dtCookie parameter
     * @return Foo
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec fooDtParamGetRequestCreation(java.time.@Nullable Instant dtParam, java.time.@Nullable Instant dtQuery, java.time.@Nullable Instant dtCookie) throws WebClientResponseException {
        Object postBody = null;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        pathParams.put("dtParam", dtParam);

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "dtQuery", dtQuery));

        cookieParams.putAll(apiClient.parameterToMultiValueMap(null, "dtCookie", dtCookie));

        final String[] localVarAccepts = { 
            "application/json"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Foo> localVarReturnType = new ParameterizedTypeReference<Foo>() {};
        return apiClient.invokeAPI("/foo/{dtParam}", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>0</b> - response
     * @param dtParam The dtParam parameter
     * @param dtQuery The dtQuery parameter
     * @param dtCookie The dtCookie parameter
     * @return Foo
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Foo> fooDtParamGet(java.time.@Nullable Instant dtParam, java.time.@Nullable Instant dtQuery, java.time.@Nullable Instant dtCookie) throws WebClientResponseException {
        ParameterizedTypeReference<Foo> localVarReturnType = new ParameterizedTypeReference<Foo>() {};
        return fooDtParamGetRequestCreation(dtParam, dtQuery, dtCookie).bodyToMono(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>0</b> - response
     * @param dtParam The dtParam parameter
     * @param dtQuery The dtQuery parameter
     * @param dtCookie The dtCookie parameter
     * @return ResponseEntity&lt;Foo&gt;
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<Foo>> fooDtParamGetWithHttpInfo(java.time.@Nullable Instant dtParam, java.time.@Nullable Instant dtQuery, java.time.@Nullable Instant dtCookie) throws WebClientResponseException {
        ParameterizedTypeReference<Foo> localVarReturnType = new ParameterizedTypeReference<Foo>() {};
        return fooDtParamGetRequestCreation(dtParam, dtQuery, dtCookie).toEntity(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>0</b> - response
     * @param dtParam The dtParam parameter
     * @param dtQuery The dtQuery parameter
     * @param dtCookie The dtCookie parameter
     * @return ResponseSpec
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec fooDtParamGetWithResponseSpec(java.time.@Nullable Instant dtParam, java.time.@Nullable Instant dtQuery, java.time.@Nullable Instant dtCookie) throws WebClientResponseException {
        return fooDtParamGetRequestCreation(dtParam, dtQuery, dtCookie);
    }

    /**
     * 
     * 
     * <p><b>0</b> - ok
     * @param _file The _file parameter
     * @param metadata The metadata parameter
     * @return UploadPostDefaultResponse
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec uploadPostRequestCreation(File _file, @Nullable File metadata) throws WebClientResponseException {
        Object postBody = null;
        // verify the required parameter '_file' is set
        if (_file == null) {
            throw new WebClientResponseException("Missing the required parameter '_file' when calling uploadPost", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        if (_file != null)
            formParams.add("file", new FileSystemResource(_file));
        if (metadata != null)
            formParams.add("metadata", new FileSystemResource(metadata));

        final String[] localVarAccepts = { 
            "application/json"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "multipart/form-data"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<UploadPostDefaultResponse> localVarReturnType = new ParameterizedTypeReference<UploadPostDefaultResponse>() {};
        return apiClient.invokeAPI("/upload", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>0</b> - ok
     * @param _file The _file parameter
     * @param metadata The metadata parameter
     * @return UploadPostDefaultResponse
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<UploadPostDefaultResponse> uploadPost(File _file, @Nullable File metadata) throws WebClientResponseException {
        ParameterizedTypeReference<UploadPostDefaultResponse> localVarReturnType = new ParameterizedTypeReference<UploadPostDefaultResponse>() {};
        return uploadPostRequestCreation(_file, metadata).bodyToMono(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>0</b> - ok
     * @param _file The _file parameter
     * @param metadata The metadata parameter
     * @return ResponseEntity&lt;UploadPostDefaultResponse&gt;
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<UploadPostDefaultResponse>> uploadPostWithHttpInfo(File _file, @Nullable File metadata) throws WebClientResponseException {
        ParameterizedTypeReference<UploadPostDefaultResponse> localVarReturnType = new ParameterizedTypeReference<UploadPostDefaultResponse>() {};
        return uploadPostRequestCreation(_file, metadata).toEntity(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>0</b> - ok
     * @param _file The _file parameter
     * @param metadata The metadata parameter
     * @return ResponseSpec
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec uploadPostWithResponseSpec(File _file, @Nullable File metadata) throws WebClientResponseException {
        return uploadPostRequestCreation(_file, metadata);
    }
}
