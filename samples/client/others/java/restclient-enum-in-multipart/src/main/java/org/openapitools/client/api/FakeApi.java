package org.openapitools.client.api;

import org.openapitools.client.ApiClient;

import org.openapitools.client.model.CodesEnum;
import org.openapitools.client.model.RefRefToPathLevelParameterOneofRefToOneofParameter;
import org.openapitools.client.model.RefToRefParameterAnyofRefToAnyofParameter;
import java.util.UUID;

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
public class FakeApi {
    private ApiClient apiClient;

    public FakeApi() {
        this(new ApiClient());
    }

    public FakeApi(ApiClient apiClient) {
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
     * <p><b>200</b> - 
     * @return Object
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec fakeInlineSchemaAnyofPath1GetRequestCreation() throws RestClientResponseException {
        Object postBody = null;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
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

        ParameterizedTypeReference<Object> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/fake/inline/schema/anyof/path1", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - 
     * @return Object
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public Object fakeInlineSchemaAnyofPath1Get() throws RestClientResponseException {
        ParameterizedTypeReference<Object> localVarReturnType = new ParameterizedTypeReference<>() {};
        return fakeInlineSchemaAnyofPath1GetRequestCreation().body(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - 
     * @return ResponseEntity&lt;Object&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Object> fakeInlineSchemaAnyofPath1GetWithHttpInfo() throws RestClientResponseException {
        ParameterizedTypeReference<Object> localVarReturnType = new ParameterizedTypeReference<>() {};
        return fakeInlineSchemaAnyofPath1GetRequestCreation().toEntity(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - 
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec fakeInlineSchemaAnyofPath1GetWithResponseSpec() throws RestClientResponseException {
        return fakeInlineSchemaAnyofPath1GetRequestCreation();
    }

    /**
     * 
     * 
     * <p><b>200</b> - 
     * @return Object
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec fakeInlineSchemaAnyofPath2GetRequestCreation() throws RestClientResponseException {
        Object postBody = null;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
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

        ParameterizedTypeReference<Object> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/fake/inline/schema/anyof/path2", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - 
     * @return Object
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public Object fakeInlineSchemaAnyofPath2Get() throws RestClientResponseException {
        ParameterizedTypeReference<Object> localVarReturnType = new ParameterizedTypeReference<>() {};
        return fakeInlineSchemaAnyofPath2GetRequestCreation().body(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - 
     * @return ResponseEntity&lt;Object&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Object> fakeInlineSchemaAnyofPath2GetWithHttpInfo() throws RestClientResponseException {
        ParameterizedTypeReference<Object> localVarReturnType = new ParameterizedTypeReference<>() {};
        return fakeInlineSchemaAnyofPath2GetRequestCreation().toEntity(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - 
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec fakeInlineSchemaAnyofPath2GetWithResponseSpec() throws RestClientResponseException {
        return fakeInlineSchemaAnyofPath2GetRequestCreation();
    }

    /**
     * 
     * 
     * <p><b>200</b> - 
     * @return List&lt;Object&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec fakeInlineSchemaAnyofPath3GetRequestCreation() throws RestClientResponseException {
        Object postBody = null;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
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

        ParameterizedTypeReference<List<Object>> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/fake/inline/schema/anyof/path3", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - 
     * @return List&lt;Object&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public List<Object> fakeInlineSchemaAnyofPath3Get() throws RestClientResponseException {
        ParameterizedTypeReference<List<Object>> localVarReturnType = new ParameterizedTypeReference<>() {};
        return fakeInlineSchemaAnyofPath3GetRequestCreation().body(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - 
     * @return ResponseEntity&lt;List&lt;Object&gt;&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<List<Object>> fakeInlineSchemaAnyofPath3GetWithHttpInfo() throws RestClientResponseException {
        ParameterizedTypeReference<List<Object>> localVarReturnType = new ParameterizedTypeReference<>() {};
        return fakeInlineSchemaAnyofPath3GetRequestCreation().toEntity(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - 
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec fakeInlineSchemaAnyofPath3GetWithResponseSpec() throws RestClientResponseException {
        return fakeInlineSchemaAnyofPath3GetRequestCreation();
    }

    /**
     * op1
     * 
     * <p><b>201</b> - Successful Response
     * <p><b>422</b> - Validation Error
     * @return Object
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec op1RequestCreation() throws RestClientResponseException {
        Object postBody = null;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
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

        ParameterizedTypeReference<Object> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/fake/api/changeowner", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * op1
     * 
     * <p><b>201</b> - Successful Response
     * <p><b>422</b> - Validation Error
     * @return Object
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public Object op1() throws RestClientResponseException {
        ParameterizedTypeReference<Object> localVarReturnType = new ParameterizedTypeReference<>() {};
        return op1RequestCreation().body(localVarReturnType);
    }

    /**
     * op1
     * 
     * <p><b>201</b> - Successful Response
     * <p><b>422</b> - Validation Error
     * @return ResponseEntity&lt;Object&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Object> op1WithHttpInfo() throws RestClientResponseException {
        ParameterizedTypeReference<Object> localVarReturnType = new ParameterizedTypeReference<>() {};
        return op1RequestCreation().toEntity(localVarReturnType);
    }

    /**
     * op1
     * 
     * <p><b>201</b> - Successful Response
     * <p><b>422</b> - Validation Error
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec op1WithResponseSpec() throws RestClientResponseException {
        return op1RequestCreation();
    }

    /**
     * op2
     * 
     * <p><b>201</b> - Successful Response
     * <p><b>422</b> - Validation Error
     * @return Object
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec op2RequestCreation() throws RestClientResponseException {
        Object postBody = null;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
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

        ParameterizedTypeReference<Object> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/fake/api/changename", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * op2
     * 
     * <p><b>201</b> - Successful Response
     * <p><b>422</b> - Validation Error
     * @return Object
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public Object op2() throws RestClientResponseException {
        ParameterizedTypeReference<Object> localVarReturnType = new ParameterizedTypeReference<>() {};
        return op2RequestCreation().body(localVarReturnType);
    }

    /**
     * op2
     * 
     * <p><b>201</b> - Successful Response
     * <p><b>422</b> - Validation Error
     * @return ResponseEntity&lt;Object&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Object> op2WithHttpInfo() throws RestClientResponseException {
        ParameterizedTypeReference<Object> localVarReturnType = new ParameterizedTypeReference<>() {};
        return op2RequestCreation().toEntity(localVarReturnType);
    }

    /**
     * op2
     * 
     * <p><b>201</b> - Successful Response
     * <p><b>422</b> - Validation Error
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec op2WithResponseSpec() throws RestClientResponseException {
        return op2RequestCreation();
    }

    /**
     * op3
     * 
     * <p><b>200</b> - Successful Response
     * @param queryEnum query enum test
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec op3RequestCreation(@jakarta.annotation.Nonnull List<CodesEnum> queryEnum) throws RestClientResponseException {
        Object postBody = null;
        // verify the required parameter 'queryEnum' is set
        if (queryEnum == null) {
            throw new RestClientResponseException("Missing the required parameter 'queryEnum' when calling op3", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        queryParams.putAll(apiClient.parameterToMultiValueMap(ApiClient.CollectionFormat.valueOf("multi".toUpperCase(Locale.ROOT)), "query_enum", queryEnum));

        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/fake/api/query/enum", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * op3
     * 
     * <p><b>200</b> - Successful Response
     * @param queryEnum query enum test
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public void op3(@jakarta.annotation.Nonnull List<CodesEnum> queryEnum) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        op3RequestCreation(queryEnum).body(localVarReturnType);
    }

    /**
     * op3
     * 
     * <p><b>200</b> - Successful Response
     * @param queryEnum query enum test
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> op3WithHttpInfo(@jakarta.annotation.Nonnull List<CodesEnum> queryEnum) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return op3RequestCreation(queryEnum).toEntity(localVarReturnType);
    }

    /**
     * op3
     * 
     * <p><b>200</b> - Successful Response
     * @param queryEnum query enum test
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec op3WithResponseSpec(@jakarta.annotation.Nonnull List<CodesEnum> queryEnum) throws RestClientResponseException {
        return op3RequestCreation(queryEnum);
    }

    /**
     * 
     * 
     * <p><b>200</b> - required to pass validation
     * @param refToUuid to test ref to parameter (uuid)
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec refToRefParameterRequestCreation(@jakarta.annotation.Nonnull UUID refToUuid) throws RestClientResponseException {
        Object postBody = null;
        // verify the required parameter 'refToUuid' is set
        if (refToUuid == null) {
            throw new RestClientResponseException("Missing the required parameter 'refToUuid' when calling refToRefParameter", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        if (refToUuid != null)
        headerParams.add("ref_to_uuid", apiClient.parameterToString(refToUuid));
        final String[] localVarAccepts = { 
            "text/plain"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/ref/ref_to_parameter", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - required to pass validation
     * @param refToUuid to test ref to parameter (uuid)
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public String refToRefParameter(@jakarta.annotation.Nonnull UUID refToUuid) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return refToRefParameterRequestCreation(refToUuid).body(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - required to pass validation
     * @param refToUuid to test ref to parameter (uuid)
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> refToRefParameterWithHttpInfo(@jakarta.annotation.Nonnull UUID refToUuid) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return refToRefParameterRequestCreation(refToUuid).toEntity(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - required to pass validation
     * @param refToUuid to test ref to parameter (uuid)
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec refToRefParameterWithResponseSpec(@jakarta.annotation.Nonnull UUID refToUuid) throws RestClientResponseException {
        return refToRefParameterRequestCreation(refToUuid);
    }

    /**
     * 
     * to test $ref to operation level parameters
     * <p><b>200</b> - Successful Response
     * @param refToAnyof to test ref to parameter (anyof)
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec refToRefParameterAnyofRequestCreation(@jakarta.annotation.Nonnull RefToRefParameterAnyofRefToAnyofParameter refToAnyof) throws RestClientResponseException {
        Object postBody = null;
        // verify the required parameter 'refToAnyof' is set
        if (refToAnyof == null) {
            throw new RestClientResponseException("Missing the required parameter 'refToAnyof' when calling refToRefParameterAnyof", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        if (refToAnyof != null)
        headerParams.add("ref_to_anyof", apiClient.parameterToString(refToAnyof));
        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/ref/ref_to_operation_level_parameter_oneof", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * to test $ref to operation level parameters
     * <p><b>200</b> - Successful Response
     * @param refToAnyof to test ref to parameter (anyof)
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public void refToRefParameterAnyof(@jakarta.annotation.Nonnull RefToRefParameterAnyofRefToAnyofParameter refToAnyof) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        refToRefParameterAnyofRequestCreation(refToAnyof).body(localVarReturnType);
    }

    /**
     * 
     * to test $ref to operation level parameters
     * <p><b>200</b> - Successful Response
     * @param refToAnyof to test ref to parameter (anyof)
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> refToRefParameterAnyofWithHttpInfo(@jakarta.annotation.Nonnull RefToRefParameterAnyofRefToAnyofParameter refToAnyof) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return refToRefParameterAnyofRequestCreation(refToAnyof).toEntity(localVarReturnType);
    }

    /**
     * 
     * to test $ref to operation level parameters
     * <p><b>200</b> - Successful Response
     * @param refToAnyof to test ref to parameter (anyof)
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec refToRefParameterAnyofWithResponseSpec(@jakarta.annotation.Nonnull RefToRefParameterAnyofRefToAnyofParameter refToAnyof) throws RestClientResponseException {
        return refToRefParameterAnyofRequestCreation(refToAnyof);
    }

    /**
     * 
     * to test $ref to path level parameters
     * <p><b>200</b> - Successful Response
     * @param refToOneof to test ref to parameter (oneof)
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec refToRefParameterOneofRequestCreation(@jakarta.annotation.Nonnull RefRefToPathLevelParameterOneofRefToOneofParameter refToOneof) throws RestClientResponseException {
        Object postBody = null;
        // verify the required parameter 'refToOneof' is set
        if (refToOneof == null) {
            throw new RestClientResponseException("Missing the required parameter 'refToOneof' when calling refToRefParameterOneof", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        if (refToOneof != null)
        headerParams.add("ref_to_oneof", apiClient.parameterToString(refToOneof));
        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/ref/ref_to_path_level_parameter_oneof", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * to test $ref to path level parameters
     * <p><b>200</b> - Successful Response
     * @param refToOneof to test ref to parameter (oneof)
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public void refToRefParameterOneof(@jakarta.annotation.Nonnull RefRefToPathLevelParameterOneofRefToOneofParameter refToOneof) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        refToRefParameterOneofRequestCreation(refToOneof).body(localVarReturnType);
    }

    /**
     * 
     * to test $ref to path level parameters
     * <p><b>200</b> - Successful Response
     * @param refToOneof to test ref to parameter (oneof)
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> refToRefParameterOneofWithHttpInfo(@jakarta.annotation.Nonnull RefRefToPathLevelParameterOneofRefToOneofParameter refToOneof) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return refToRefParameterOneofRequestCreation(refToOneof).toEntity(localVarReturnType);
    }

    /**
     * 
     * to test $ref to path level parameters
     * <p><b>200</b> - Successful Response
     * @param refToOneof to test ref to parameter (oneof)
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec refToRefParameterOneofWithResponseSpec(@jakarta.annotation.Nonnull RefRefToPathLevelParameterOneofRefToOneofParameter refToOneof) throws RestClientResponseException {
        return refToRefParameterOneofRequestCreation(refToOneof);
    }

    /**
     * 
     * 
     * <p><b>200</b> - required to pass validation
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec responseNoRefRequestCreation() throws RestClientResponseException {
        Object postBody = null;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

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
        return apiClient.invokeAPI("/no_ref", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - required to pass validation
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public String responseNoRef() throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return responseNoRefRequestCreation().body(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - required to pass validation
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> responseNoRefWithHttpInfo() throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return responseNoRefRequestCreation().toEntity(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - required to pass validation
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec responseNoRefWithResponseSpec() throws RestClientResponseException {
        return responseNoRefRequestCreation();
    }

    /**
     * 
     * 
     * <p><b>200</b> - required to pass validation
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec responseRefToNoRefRequestCreation() throws RestClientResponseException {
        Object postBody = null;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

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
        return apiClient.invokeAPI("/ref/no_ref", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - required to pass validation
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public String responseRefToNoRef() throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return responseRefToNoRefRequestCreation().body(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - required to pass validation
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> responseRefToNoRefWithHttpInfo() throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return responseRefToNoRefRequestCreation().toEntity(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - required to pass validation
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec responseRefToNoRefWithResponseSpec() throws RestClientResponseException {
        return responseRefToNoRefRequestCreation();
    }

    /**
     * 
     * 
     * <p><b>200</b> - required to pass validation
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec responseRefToRefRequestCreation() throws RestClientResponseException {
        Object postBody = null;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

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
        return apiClient.invokeAPI("/ref/ref", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - required to pass validation
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public String responseRefToRef() throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return responseRefToRefRequestCreation().body(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - required to pass validation
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> responseRefToRefWithHttpInfo() throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return responseRefToRefRequestCreation().toEntity(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - required to pass validation
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec responseRefToRefWithResponseSpec() throws RestClientResponseException {
        return responseRefToRefRequestCreation();
    }
}
