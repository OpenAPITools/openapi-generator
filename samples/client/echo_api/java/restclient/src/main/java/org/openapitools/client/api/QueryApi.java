package org.openapitools.client.api;

import org.openapitools.client.ApiClient;

import org.openapitools.client.model.DataQuery;
import java.time.LocalDate;
import java.time.Instant;
import org.openapitools.client.model.Pet;
import org.openapitools.client.model.StringEnumRef;
import org.openapitools.client.model.TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter;
import org.openapitools.client.model.TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter;

import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.web.client.RestClient.ResponseSpec;
import org.springframework.web.client.RestClientResponseException;
import org.springframework.core.io.FileSystemResource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;

@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen", comments = "Generator version: 7.12.0-SNAPSHOT")
public class QueryApi {
    private ApiClient apiClient;

    public QueryApi() {
        this(new ApiClient());
    }

    @Autowired
    public QueryApi(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    public ApiClient getApiClient() {
        return apiClient;
    }

    public void setApiClient(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param enumNonrefStringQuery The enumNonrefStringQuery parameter
     * @param enumRefStringQuery The enumRefStringQuery parameter
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testEnumRefStringRequestCreation(String enumNonrefStringQuery, StringEnumRef enumRefStringQuery) throws RestClientResponseException {
        Object postBody = null;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "enum_nonref_string_query", enumNonrefStringQuery));
        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "enum_ref_string_query", enumRefStringQuery));
        
        final String[] localVarAccepts = { 
            "text/plain"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/query/enum_ref_string", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param enumNonrefStringQuery The enumNonrefStringQuery parameter
     * @param enumRefStringQuery The enumRefStringQuery parameter
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public String testEnumRefString(String enumNonrefStringQuery, StringEnumRef enumRefStringQuery) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testEnumRefStringRequestCreation(enumNonrefStringQuery, enumRefStringQuery).body(localVarReturnType);
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param enumNonrefStringQuery The enumNonrefStringQuery parameter
     * @param enumRefStringQuery The enumRefStringQuery parameter
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> testEnumRefStringWithHttpInfo(String enumNonrefStringQuery, StringEnumRef enumRefStringQuery) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testEnumRefStringRequestCreation(enumNonrefStringQuery, enumRefStringQuery).toEntity(localVarReturnType);
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param enumNonrefStringQuery The enumNonrefStringQuery parameter
     * @param enumRefStringQuery The enumRefStringQuery parameter
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testEnumRefStringWithResponseSpec(String enumNonrefStringQuery, StringEnumRef enumRefStringQuery) throws RestClientResponseException {
        return testEnumRefStringRequestCreation(enumNonrefStringQuery, enumRefStringQuery);
    }
    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param datetimeQuery The datetimeQuery parameter
     * @param dateQuery The dateQuery parameter
     * @param stringQuery The stringQuery parameter
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testQueryDatetimeDateStringRequestCreation(Instant datetimeQuery, LocalDate dateQuery, String stringQuery) throws RestClientResponseException {
        Object postBody = null;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "datetime_query", datetimeQuery));
        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "date_query", dateQuery));
        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "string_query", stringQuery));
        
        final String[] localVarAccepts = { 
            "text/plain"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/query/datetime/date/string", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param datetimeQuery The datetimeQuery parameter
     * @param dateQuery The dateQuery parameter
     * @param stringQuery The stringQuery parameter
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public String testQueryDatetimeDateString(Instant datetimeQuery, LocalDate dateQuery, String stringQuery) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testQueryDatetimeDateStringRequestCreation(datetimeQuery, dateQuery, stringQuery).body(localVarReturnType);
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param datetimeQuery The datetimeQuery parameter
     * @param dateQuery The dateQuery parameter
     * @param stringQuery The stringQuery parameter
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> testQueryDatetimeDateStringWithHttpInfo(Instant datetimeQuery, LocalDate dateQuery, String stringQuery) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testQueryDatetimeDateStringRequestCreation(datetimeQuery, dateQuery, stringQuery).toEntity(localVarReturnType);
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param datetimeQuery The datetimeQuery parameter
     * @param dateQuery The dateQuery parameter
     * @param stringQuery The stringQuery parameter
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testQueryDatetimeDateStringWithResponseSpec(Instant datetimeQuery, LocalDate dateQuery, String stringQuery) throws RestClientResponseException {
        return testQueryDatetimeDateStringRequestCreation(datetimeQuery, dateQuery, stringQuery);
    }
    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param integerQuery The integerQuery parameter
     * @param booleanQuery The booleanQuery parameter
     * @param stringQuery The stringQuery parameter
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testQueryIntegerBooleanStringRequestCreation(Integer integerQuery, Boolean booleanQuery, String stringQuery) throws RestClientResponseException {
        Object postBody = null;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "integer_query", integerQuery));
        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "boolean_query", booleanQuery));
        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "string_query", stringQuery));
        
        final String[] localVarAccepts = { 
            "text/plain"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/query/integer/boolean/string", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param integerQuery The integerQuery parameter
     * @param booleanQuery The booleanQuery parameter
     * @param stringQuery The stringQuery parameter
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public String testQueryIntegerBooleanString(Integer integerQuery, Boolean booleanQuery, String stringQuery) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testQueryIntegerBooleanStringRequestCreation(integerQuery, booleanQuery, stringQuery).body(localVarReturnType);
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param integerQuery The integerQuery parameter
     * @param booleanQuery The booleanQuery parameter
     * @param stringQuery The stringQuery parameter
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> testQueryIntegerBooleanStringWithHttpInfo(Integer integerQuery, Boolean booleanQuery, String stringQuery) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testQueryIntegerBooleanStringRequestCreation(integerQuery, booleanQuery, stringQuery).toEntity(localVarReturnType);
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param integerQuery The integerQuery parameter
     * @param booleanQuery The booleanQuery parameter
     * @param stringQuery The stringQuery parameter
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testQueryIntegerBooleanStringWithResponseSpec(Integer integerQuery, Boolean booleanQuery, String stringQuery) throws RestClientResponseException {
        return testQueryIntegerBooleanStringRequestCreation(integerQuery, booleanQuery, stringQuery);
    }
    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param queryObject The queryObject parameter
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testQueryStyleDeepObjectExplodeTrueObjectRequestCreation(Pet queryObject) throws RestClientResponseException {
        Object postBody = null;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "id", queryObject.getId()));
        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "name", queryObject.getName()));
        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "category", queryObject.getCategory()));
        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "photoUrls", queryObject.getPhotoUrls()));
        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "tags", queryObject.getTags()));
        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "status", queryObject.getStatus()));
        
        final String[] localVarAccepts = { 
            "text/plain"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/query/style_deepObject/explode_true/object", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param queryObject The queryObject parameter
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public String testQueryStyleDeepObjectExplodeTrueObject(Pet queryObject) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testQueryStyleDeepObjectExplodeTrueObjectRequestCreation(queryObject).body(localVarReturnType);
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param queryObject The queryObject parameter
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> testQueryStyleDeepObjectExplodeTrueObjectWithHttpInfo(Pet queryObject) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testQueryStyleDeepObjectExplodeTrueObjectRequestCreation(queryObject).toEntity(localVarReturnType);
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param queryObject The queryObject parameter
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testQueryStyleDeepObjectExplodeTrueObjectWithResponseSpec(Pet queryObject) throws RestClientResponseException {
        return testQueryStyleDeepObjectExplodeTrueObjectRequestCreation(queryObject);
    }
    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param queryObject The queryObject parameter
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testQueryStyleDeepObjectExplodeTrueObjectAllOfRequestCreation(TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter queryObject) throws RestClientResponseException {
        Object postBody = null;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "query_object", queryObject));
        
        final String[] localVarAccepts = { 
            "text/plain"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/query/style_deepObject/explode_true/object/allOf", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param queryObject The queryObject parameter
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public String testQueryStyleDeepObjectExplodeTrueObjectAllOf(TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter queryObject) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testQueryStyleDeepObjectExplodeTrueObjectAllOfRequestCreation(queryObject).body(localVarReturnType);
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param queryObject The queryObject parameter
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> testQueryStyleDeepObjectExplodeTrueObjectAllOfWithHttpInfo(TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter queryObject) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testQueryStyleDeepObjectExplodeTrueObjectAllOfRequestCreation(queryObject).toEntity(localVarReturnType);
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param queryObject The queryObject parameter
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testQueryStyleDeepObjectExplodeTrueObjectAllOfWithResponseSpec(TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter queryObject) throws RestClientResponseException {
        return testQueryStyleDeepObjectExplodeTrueObjectAllOfRequestCreation(queryObject);
    }
    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param queryObject The queryObject parameter
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testQueryStyleFormExplodeFalseArrayIntegerRequestCreation(List<Integer> queryObject) throws RestClientResponseException {
        Object postBody = null;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        queryParams.putAll(apiClient.parameterToMultiValueMap(ApiClient.CollectionFormat.valueOf("csv".toUpperCase(Locale.ROOT)), "query_object", queryObject));
        
        final String[] localVarAccepts = { 
            "text/plain"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/query/style_form/explode_false/array_integer", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param queryObject The queryObject parameter
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public String testQueryStyleFormExplodeFalseArrayInteger(List<Integer> queryObject) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testQueryStyleFormExplodeFalseArrayIntegerRequestCreation(queryObject).body(localVarReturnType);
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param queryObject The queryObject parameter
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> testQueryStyleFormExplodeFalseArrayIntegerWithHttpInfo(List<Integer> queryObject) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testQueryStyleFormExplodeFalseArrayIntegerRequestCreation(queryObject).toEntity(localVarReturnType);
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param queryObject The queryObject parameter
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testQueryStyleFormExplodeFalseArrayIntegerWithResponseSpec(List<Integer> queryObject) throws RestClientResponseException {
        return testQueryStyleFormExplodeFalseArrayIntegerRequestCreation(queryObject);
    }
    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param queryObject The queryObject parameter
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testQueryStyleFormExplodeFalseArrayStringRequestCreation(List<String> queryObject) throws RestClientResponseException {
        Object postBody = null;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        queryParams.putAll(apiClient.parameterToMultiValueMap(ApiClient.CollectionFormat.valueOf("csv".toUpperCase(Locale.ROOT)), "query_object", queryObject));
        
        final String[] localVarAccepts = { 
            "text/plain"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/query/style_form/explode_false/array_string", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param queryObject The queryObject parameter
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public String testQueryStyleFormExplodeFalseArrayString(List<String> queryObject) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testQueryStyleFormExplodeFalseArrayStringRequestCreation(queryObject).body(localVarReturnType);
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param queryObject The queryObject parameter
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> testQueryStyleFormExplodeFalseArrayStringWithHttpInfo(List<String> queryObject) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testQueryStyleFormExplodeFalseArrayStringRequestCreation(queryObject).toEntity(localVarReturnType);
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param queryObject The queryObject parameter
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testQueryStyleFormExplodeFalseArrayStringWithResponseSpec(List<String> queryObject) throws RestClientResponseException {
        return testQueryStyleFormExplodeFalseArrayStringRequestCreation(queryObject);
    }
    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param queryObject The queryObject parameter
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testQueryStyleFormExplodeTrueArrayStringRequestCreation(TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter queryObject) throws RestClientResponseException {
        Object postBody = null;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "values", queryObject.getValues()));
        
        final String[] localVarAccepts = { 
            "text/plain"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/query/style_form/explode_true/array_string", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param queryObject The queryObject parameter
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public String testQueryStyleFormExplodeTrueArrayString(TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter queryObject) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testQueryStyleFormExplodeTrueArrayStringRequestCreation(queryObject).body(localVarReturnType);
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param queryObject The queryObject parameter
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> testQueryStyleFormExplodeTrueArrayStringWithHttpInfo(TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter queryObject) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testQueryStyleFormExplodeTrueArrayStringRequestCreation(queryObject).toEntity(localVarReturnType);
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param queryObject The queryObject parameter
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testQueryStyleFormExplodeTrueArrayStringWithResponseSpec(TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter queryObject) throws RestClientResponseException {
        return testQueryStyleFormExplodeTrueArrayStringRequestCreation(queryObject);
    }
    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param queryObject The queryObject parameter
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testQueryStyleFormExplodeTrueObjectRequestCreation(Pet queryObject) throws RestClientResponseException {
        Object postBody = null;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "id", queryObject.getId()));
        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "name", queryObject.getName()));
        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "category", queryObject.getCategory()));
        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "photoUrls", queryObject.getPhotoUrls()));
        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "tags", queryObject.getTags()));
        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "status", queryObject.getStatus()));
        
        final String[] localVarAccepts = { 
            "text/plain"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/query/style_form/explode_true/object", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param queryObject The queryObject parameter
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public String testQueryStyleFormExplodeTrueObject(Pet queryObject) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testQueryStyleFormExplodeTrueObjectRequestCreation(queryObject).body(localVarReturnType);
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param queryObject The queryObject parameter
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> testQueryStyleFormExplodeTrueObjectWithHttpInfo(Pet queryObject) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testQueryStyleFormExplodeTrueObjectRequestCreation(queryObject).toEntity(localVarReturnType);
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param queryObject The queryObject parameter
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testQueryStyleFormExplodeTrueObjectWithResponseSpec(Pet queryObject) throws RestClientResponseException {
        return testQueryStyleFormExplodeTrueObjectRequestCreation(queryObject);
    }
    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param queryObject The queryObject parameter
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testQueryStyleFormExplodeTrueObjectAllOfRequestCreation(DataQuery queryObject) throws RestClientResponseException {
        Object postBody = null;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "query_object", queryObject));
        
        final String[] localVarAccepts = { 
            "text/plain"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/query/style_form/explode_true/object/allOf", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param queryObject The queryObject parameter
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public String testQueryStyleFormExplodeTrueObjectAllOf(DataQuery queryObject) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testQueryStyleFormExplodeTrueObjectAllOfRequestCreation(queryObject).body(localVarReturnType);
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param queryObject The queryObject parameter
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> testQueryStyleFormExplodeTrueObjectAllOfWithHttpInfo(DataQuery queryObject) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testQueryStyleFormExplodeTrueObjectAllOfRequestCreation(queryObject).toEntity(localVarReturnType);
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param queryObject The queryObject parameter
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testQueryStyleFormExplodeTrueObjectAllOfWithResponseSpec(DataQuery queryObject) throws RestClientResponseException {
        return testQueryStyleFormExplodeTrueObjectAllOfRequestCreation(queryObject);
    }
}
