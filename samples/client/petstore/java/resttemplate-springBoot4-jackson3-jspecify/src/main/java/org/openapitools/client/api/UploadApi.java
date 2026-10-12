package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import org.openapitools.client.BaseApi;

import java.io.File;
import org.jspecify.annotations.Nullable;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.stream.Collectors;

import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.core.io.FileSystemResource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;

@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
public class UploadApi extends BaseApi {

    public UploadApi() {
        super(new ApiClient());
    }

    public UploadApi(ApiClient apiClient) {
        super(apiClient);
    }

    /**
     * 
     * 
     * <p><b>0</b> - ok
     * @param _file  (optional)
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public void uploadFilesPost(@Nullable List<File> _file) throws RestClientException {
        uploadFilesPostWithHttpInfo(_file);
    }

    /**
     * 
     * 
     * <p><b>0</b> - ok
     * @param _file  (optional)
     * @return ResponseEntity&lt;Void&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> uploadFilesPostWithHttpInfo(@Nullable List<File> _file) throws RestClientException {
        

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<>();
        final HttpHeaders localVarHeaderParams = new HttpHeaders();
        final MultiValueMap<String, String> localVarCookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> localVarFormParams = new LinkedMultiValueMap<>();

        if (_file != null) {
            localVarFormParams.addAll("file", _file.stream().map(FileSystemResource::new).collect(Collectors.toList()));
        }

        final String[] localVarAccepts = {  };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "multipart/form-data"
         };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Void> localReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/uploadFiles", HttpMethod.POST, Collections.<String, Object>emptyMap(), localVarQueryParams, null, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localReturnType);
    }
    /**
     * 
     * 
     * <p><b>0</b> - ok
     * @param _file  (optional)
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public void uploadPost(@Nullable File _file) throws RestClientException {
        uploadPostWithHttpInfo(_file);
    }

    /**
     * 
     * 
     * <p><b>0</b> - ok
     * @param _file  (optional)
     * @return ResponseEntity&lt;Void&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> uploadPostWithHttpInfo(@Nullable File _file) throws RestClientException {
        

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<>();
        final HttpHeaders localVarHeaderParams = new HttpHeaders();
        final MultiValueMap<String, String> localVarCookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> localVarFormParams = new LinkedMultiValueMap<>();

        if (_file != null) {
            localVarFormParams.add("file", new FileSystemResource(_file));
        }

        final String[] localVarAccepts = {  };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "multipart/form-data"
         };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Void> localReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/upload", HttpMethod.POST, Collections.<String, Object>emptyMap(), localVarQueryParams, null, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localReturnType);
    }

    @Override
    public <T> ResponseEntity<T> invokeAPI(String url, HttpMethod method, Object request, ParameterizedTypeReference<T> returnType) throws RestClientException {
        String localVarPath = url.replace(apiClient.getBasePath(), "");

        final Map<String, Object> uriVariables = new HashMap<>();
        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<>();
        final HttpHeaders localVarHeaderParams = new HttpHeaders();
        final MultiValueMap<String, String> localVarCookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> localVarFormParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = {  };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "multipart/form-data"
         };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        return apiClient.invokeAPI(localVarPath, method, uriVariables, localVarQueryParams, request, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, returnType);
    }
}
