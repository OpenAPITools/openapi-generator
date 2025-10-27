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
public class HeaderApi {
    private ApiClient apiClient;

    public HeaderApi() {
        this(new ApiClient());
    }

    public HeaderApi(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    public ApiClient getApiClient() {
        return apiClient;
    }

    public void setApiClient(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    /**
     * Test header parameter(s)
     * Test header parameter(s)
     * <p><b>200</b> - Successful operation
     * @param integerHeader The integerHeader parameter
     * @param booleanHeader The booleanHeader parameter
     * @param stringHeader The stringHeader parameter
     * @param enumNonrefStringHeader The enumNonrefStringHeader parameter
     * @param enumRefStringHeader The enumRefStringHeader parameter
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testHeaderIntegerBooleanStringEnumsRequestCreation(@jakarta.annotation.Nullable Integer integerHeader, @jakarta.annotation.Nullable Boolean booleanHeader, @jakarta.annotation.Nullable String stringHeader, @jakarta.annotation.Nullable String enumNonrefStringHeader, @jakarta.annotation.Nullable StringEnumRef enumRefStringHeader) throws RestClientResponseException {
        Object postBody = null;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        if (integerHeader != null)
        headerParams.add("integer_header", apiClient.parameterToString(integerHeader));
        if (booleanHeader != null)
        headerParams.add("boolean_header", apiClient.parameterToString(booleanHeader));
        if (stringHeader != null)
        headerParams.add("string_header", apiClient.parameterToString(stringHeader));
        if (enumNonrefStringHeader != null)
        headerParams.add("enum_nonref_string_header", apiClient.parameterToString(enumNonrefStringHeader));
        if (enumRefStringHeader != null)
        headerParams.add("enum_ref_string_header", apiClient.parameterToString(enumRefStringHeader));
        final String[] localVarAccepts = { 
            "text/plain"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/header/integer/boolean/string/enums", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Test header parameter(s)
     * Test header parameter(s)
     * <p><b>200</b> - Successful operation
     * @param integerHeader The integerHeader parameter
     * @param booleanHeader The booleanHeader parameter
     * @param stringHeader The stringHeader parameter
     * @param enumNonrefStringHeader The enumNonrefStringHeader parameter
     * @param enumRefStringHeader The enumRefStringHeader parameter
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public String testHeaderIntegerBooleanStringEnums(@jakarta.annotation.Nullable Integer integerHeader, @jakarta.annotation.Nullable Boolean booleanHeader, @jakarta.annotation.Nullable String stringHeader, @jakarta.annotation.Nullable String enumNonrefStringHeader, @jakarta.annotation.Nullable StringEnumRef enumRefStringHeader) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testHeaderIntegerBooleanStringEnumsRequestCreation(integerHeader, booleanHeader, stringHeader, enumNonrefStringHeader, enumRefStringHeader).body(localVarReturnType);
    }

    /**
     * Test header parameter(s)
     * Test header parameter(s)
     * <p><b>200</b> - Successful operation
     * @param integerHeader The integerHeader parameter
     * @param booleanHeader The booleanHeader parameter
     * @param stringHeader The stringHeader parameter
     * @param enumNonrefStringHeader The enumNonrefStringHeader parameter
     * @param enumRefStringHeader The enumRefStringHeader parameter
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> testHeaderIntegerBooleanStringEnumsWithHttpInfo(@jakarta.annotation.Nullable Integer integerHeader, @jakarta.annotation.Nullable Boolean booleanHeader, @jakarta.annotation.Nullable String stringHeader, @jakarta.annotation.Nullable String enumNonrefStringHeader, @jakarta.annotation.Nullable StringEnumRef enumRefStringHeader) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testHeaderIntegerBooleanStringEnumsRequestCreation(integerHeader, booleanHeader, stringHeader, enumNonrefStringHeader, enumRefStringHeader).toEntity(localVarReturnType);
    }

    /**
     * Test header parameter(s)
     * Test header parameter(s)
     * <p><b>200</b> - Successful operation
     * @param integerHeader The integerHeader parameter
     * @param booleanHeader The booleanHeader parameter
     * @param stringHeader The stringHeader parameter
     * @param enumNonrefStringHeader The enumNonrefStringHeader parameter
     * @param enumRefStringHeader The enumRefStringHeader parameter
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testHeaderIntegerBooleanStringEnumsWithResponseSpec(@jakarta.annotation.Nullable Integer integerHeader, @jakarta.annotation.Nullable Boolean booleanHeader, @jakarta.annotation.Nullable String stringHeader, @jakarta.annotation.Nullable String enumNonrefStringHeader, @jakarta.annotation.Nullable StringEnumRef enumRefStringHeader) throws RestClientResponseException {
        return testHeaderIntegerBooleanStringEnumsRequestCreation(integerHeader, booleanHeader, stringHeader, enumNonrefStringHeader, enumRefStringHeader);
    }
}
