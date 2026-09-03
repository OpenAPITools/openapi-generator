package org.openapitools.client.api;

import org.openapitools.client.ApiClient;

import org.openapitools.client.model.Foo;
import org.jspecify.annotations.Nullable;
import java.time.OffsetDateTime;

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
public class FooApi {
    private ApiClient apiClient;

    public FooApi() {
        this(new ApiClient());
    }

    public FooApi(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    public ApiClient getApiClient() {
        return apiClient;
    }

    public void setApiClient(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    public class FooDtParamGetRequest {
        private java.time.@Nullable Instant dtParam;
        private java.time.@Nullable Instant dtQuery;
        private java.time.@Nullable Instant dtCookie;
        private @Nullable String color;

        public FooDtParamGetRequest() {}

        public FooDtParamGetRequest(java.time.@Nullable Instant dtParam, java.time.@Nullable Instant dtQuery, java.time.@Nullable Instant dtCookie, @Nullable String color) {
            this.dtParam = dtParam;
            this.dtQuery = dtQuery;
            this.dtCookie = dtCookie;
            this.color = color;
        }

        public java.time.@Nullable Instant dtParam() {
            return this.dtParam;
        }
        public FooDtParamGetRequest dtParam(java.time.@Nullable Instant dtParam) {
            this.dtParam = dtParam;
            return this;
        }

        public java.time.@Nullable Instant dtQuery() {
            return this.dtQuery;
        }
        public FooDtParamGetRequest dtQuery(java.time.@Nullable Instant dtQuery) {
            this.dtQuery = dtQuery;
            return this;
        }

        public java.time.@Nullable Instant dtCookie() {
            return this.dtCookie;
        }
        public FooDtParamGetRequest dtCookie(java.time.@Nullable Instant dtCookie) {
            this.dtCookie = dtCookie;
            return this;
        }

        public @Nullable String color() {
            return this.color;
        }
        public FooDtParamGetRequest color(@Nullable String color) {
            this.color = color;
            return this;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) {
                return true;
            }
            if (o == null || getClass() != o.getClass()) {
                return false;
            }
            FooDtParamGetRequest request = (FooDtParamGetRequest) o;
            return Objects.equals(this.dtParam, request.dtParam()) &&
                Objects.equals(this.dtQuery, request.dtQuery()) &&
                Objects.equals(this.dtCookie, request.dtCookie()) &&
                Objects.equals(this.color, request.color());
        }

        @Override
        public int hashCode() {
            return Objects.hash(dtParam, dtQuery, dtCookie, color);
        }

        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder();
            sb.append("class FooDtParamGetRequest {\n");
            sb.append("    dtParam: ").append(toIndentedString(dtParam)).append("\n");
            sb.append("    dtQuery: ").append(toIndentedString(dtQuery)).append("\n");
            sb.append("    dtCookie: ").append(toIndentedString(dtCookie)).append("\n");
            sb.append("    color: ").append(toIndentedString(color)).append("\n");
            sb.append("}");
            return sb.toString();
        }

        /**
        * Convert the given object to string with each line indented by 4 spaces
        * (except the first line).
        */
        private String toIndentedString(Object o) {
            return o == null ? "null" : o.toString().replace("\n", "\n    ");
        }
    }

    /**
     * 
     * 
     * <p><b>0</b> - response
     * @param requestParameters The fooDtParamGet request parameters as object
     * @return Foo
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Foo> fooDtParamGet(FooDtParamGetRequest requestParameters) throws WebClientResponseException {
        return this.fooDtParamGet(requestParameters.dtParam(), requestParameters.dtQuery(), requestParameters.dtCookie(), requestParameters.color());
    }

    /**
     * 
     * 
     * <p><b>0</b> - response
     * @param requestParameters The fooDtParamGet request parameters as object
     * @return ResponseEntity&lt;Foo&gt;
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<Foo>> fooDtParamGetWithHttpInfo(FooDtParamGetRequest requestParameters) throws WebClientResponseException {
        return this.fooDtParamGetWithHttpInfo(requestParameters.dtParam(), requestParameters.dtQuery(), requestParameters.dtCookie(), requestParameters.color());
    }

    /**
     * 
     * 
     * <p><b>0</b> - response
     * @param requestParameters The fooDtParamGet request parameters as object
     * @return ResponseSpec
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec fooDtParamGetWithResponseSpec(FooDtParamGetRequest requestParameters) throws WebClientResponseException {
        return this.fooDtParamGetWithResponseSpec(requestParameters.dtParam(), requestParameters.dtQuery(), requestParameters.dtCookie(), requestParameters.color());
    }


    /**
     * 
     * 
     * <p><b>0</b> - response
     * @param dtParam The dtParam parameter
     * @param dtQuery The dtQuery parameter
     * @param dtCookie The dtCookie parameter
     * @param color The color parameter
     * @return Foo
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec fooDtParamGetRequestCreation(java.time.@Nullable Instant dtParam, java.time.@Nullable Instant dtQuery, java.time.@Nullable Instant dtCookie, @Nullable String color) throws WebClientResponseException {
        Object postBody = null;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        pathParams.put("dtParam", dtParam);

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        localVarQueryParams.putAll(apiClient.parameterToMultiValueMap(null, "dtQuery", dtQuery));
        localVarQueryParams.putAll(apiClient.parameterToMultiValueMap(null, "color", color));

        cookieParams.putAll(apiClient.parameterToMultiValueMap(null, "dtCookie", dtCookie));

        final String[] localVarAccepts = { 
            "application/json"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Foo> localVarReturnType = new ParameterizedTypeReference<Foo>() {};
        return apiClient.invokeAPI("/foo/{dtParam}", HttpMethod.GET, pathParams, localVarQueryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>0</b> - response
     * @param dtParam The dtParam parameter
     * @param dtQuery The dtQuery parameter
     * @param dtCookie The dtCookie parameter
     * @param color The color parameter
     * @return Foo
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Foo> fooDtParamGet(java.time.@Nullable Instant dtParam, java.time.@Nullable Instant dtQuery, java.time.@Nullable Instant dtCookie, @Nullable String color) throws WebClientResponseException {
        ParameterizedTypeReference<Foo> localVarReturnType = new ParameterizedTypeReference<Foo>() {};
        return fooDtParamGetRequestCreation(dtParam, dtQuery, dtCookie, color).bodyToMono(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>0</b> - response
     * @param dtParam The dtParam parameter
     * @param dtQuery The dtQuery parameter
     * @param dtCookie The dtCookie parameter
     * @param color The color parameter
     * @return ResponseEntity&lt;Foo&gt;
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<Foo>> fooDtParamGetWithHttpInfo(java.time.@Nullable Instant dtParam, java.time.@Nullable Instant dtQuery, java.time.@Nullable Instant dtCookie, @Nullable String color) throws WebClientResponseException {
        ParameterizedTypeReference<Foo> localVarReturnType = new ParameterizedTypeReference<Foo>() {};
        return fooDtParamGetRequestCreation(dtParam, dtQuery, dtCookie, color).toEntity(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>0</b> - response
     * @param dtParam The dtParam parameter
     * @param dtQuery The dtQuery parameter
     * @param dtCookie The dtCookie parameter
     * @param color The color parameter
     * @return ResponseSpec
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec fooDtParamGetWithResponseSpec(java.time.@Nullable Instant dtParam, java.time.@Nullable Instant dtQuery, java.time.@Nullable Instant dtCookie, @Nullable String color) throws WebClientResponseException {
        return fooDtParamGetRequestCreation(dtParam, dtQuery, dtCookie, color);
    }
}
