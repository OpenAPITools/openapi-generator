package org.openapitools.client.api;

import org.openapitools.client.ApiClient;

import org.openapitools.client.model.StringEnumRef;

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
public class PathApi {
    private ApiClient apiClient;

    public PathApi() {
        this(new ApiClient());
    }

    public PathApi(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    public ApiClient getApiClient() {
        return apiClient;
    }

    public void setApiClient(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    /**
     * Test path parameter(s)
     * Test path parameter(s)
     * <p><b>200</b> - Successful operation
     * @param pathString The pathString parameter
     * @param pathInteger The pathInteger parameter
     * @param enumNonrefStringPath The enumNonrefStringPath parameter
     * @param enumRefStringPath The enumRefStringPath parameter
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPathRequestCreation(@jakarta.annotation.Nonnull String pathString, @jakarta.annotation.Nonnull Integer pathInteger, @jakarta.annotation.Nonnull String enumNonrefStringPath, @jakarta.annotation.Nonnull StringEnumRef enumRefStringPath) throws RestClientResponseException {
        Object postBody = null;
        // verify the required parameter 'pathString' is set
        if (pathString == null) {
            throw new RestClientResponseException("Missing the required parameter 'pathString' when calling testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // verify the required parameter 'pathInteger' is set
        if (pathInteger == null) {
            throw new RestClientResponseException("Missing the required parameter 'pathInteger' when calling testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // verify the required parameter 'enumNonrefStringPath' is set
        if (enumNonrefStringPath == null) {
            throw new RestClientResponseException("Missing the required parameter 'enumNonrefStringPath' when calling testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // verify the required parameter 'enumRefStringPath' is set
        if (enumRefStringPath == null) {
            throw new RestClientResponseException("Missing the required parameter 'enumRefStringPath' when calling testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        pathParams.put("path_string", pathString);
        pathParams.put("path_integer", pathInteger);
        pathParams.put("enum_nonref_string_path", enumNonrefStringPath);
        pathParams.put("enum_ref_string_path", enumRefStringPath);

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = { 
            "text/plain"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/path/string/{path_string}/integer/{path_integer}/{enum_nonref_string_path}/{enum_ref_string_path}", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Test path parameter(s)
     * Test path parameter(s)
     * <p><b>200</b> - Successful operation
     * @param pathString The pathString parameter
     * @param pathInteger The pathInteger parameter
     * @param enumNonrefStringPath The enumNonrefStringPath parameter
     * @param enumRefStringPath The enumRefStringPath parameter
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public String testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath(@jakarta.annotation.Nonnull String pathString, @jakarta.annotation.Nonnull Integer pathInteger, @jakarta.annotation.Nonnull String enumNonrefStringPath, @jakarta.annotation.Nonnull StringEnumRef enumRefStringPath) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPathRequestCreation(pathString, pathInteger, enumNonrefStringPath, enumRefStringPath).body(localVarReturnType);
    }

    /**
     * Test path parameter(s)
     * Test path parameter(s)
     * <p><b>200</b> - Successful operation
     * @param pathString The pathString parameter
     * @param pathInteger The pathInteger parameter
     * @param enumNonrefStringPath The enumNonrefStringPath parameter
     * @param enumRefStringPath The enumRefStringPath parameter
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPathWithHttpInfo(@jakarta.annotation.Nonnull String pathString, @jakarta.annotation.Nonnull Integer pathInteger, @jakarta.annotation.Nonnull String enumNonrefStringPath, @jakarta.annotation.Nonnull StringEnumRef enumRefStringPath) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPathRequestCreation(pathString, pathInteger, enumNonrefStringPath, enumRefStringPath).toEntity(localVarReturnType);
    }

    /**
     * Test path parameter(s)
     * Test path parameter(s)
     * <p><b>200</b> - Successful operation
     * @param pathString The pathString parameter
     * @param pathInteger The pathInteger parameter
     * @param enumNonrefStringPath The enumNonrefStringPath parameter
     * @param enumRefStringPath The enumRefStringPath parameter
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPathWithResponseSpec(@jakarta.annotation.Nonnull String pathString, @jakarta.annotation.Nonnull Integer pathInteger, @jakarta.annotation.Nonnull String enumNonrefStringPath, @jakarta.annotation.Nonnull StringEnumRef enumRefStringPath) throws RestClientResponseException {
        return testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPathRequestCreation(pathString, pathInteger, enumNonrefStringPath, enumRefStringPath);
    }
}
