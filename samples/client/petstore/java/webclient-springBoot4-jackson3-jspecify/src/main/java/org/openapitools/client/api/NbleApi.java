package org.openapitools.client.api;

import org.openapitools.client.ApiClient;

import org.jspecify.annotations.Nullable;
import org.openapitools.client.model.RequiredAndNullable;

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
public class NbleApi {
    private ApiClient apiClient;

    public NbleApi() {
        this(new ApiClient());
    }

    public NbleApi(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    public ApiClient getApiClient() {
        return apiClient;
    }

    public void setApiClient(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    public class NbleGetRequest {
        private @Nullable String mandatory;
        private @Nullable String optional;

        public NbleGetRequest() {}

        public NbleGetRequest(@Nullable String mandatory, @Nullable String optional) {
            this.mandatory = mandatory;
            this.optional = optional;
        }

        public @Nullable String mandatory() {
            return this.mandatory;
        }
        public NbleGetRequest mandatory(@Nullable String mandatory) {
            this.mandatory = mandatory;
            return this;
        }

        public @Nullable String optional() {
            return this.optional;
        }
        public NbleGetRequest optional(@Nullable String optional) {
            this.optional = optional;
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
            NbleGetRequest request = (NbleGetRequest) o;
            return Objects.equals(this.mandatory, request.mandatory()) &&
                Objects.equals(this.optional, request.optional());
        }

        @Override
        public int hashCode() {
            return Objects.hash(mandatory, optional);
        }

        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder();
            sb.append("class NbleGetRequest {\n");
            sb.append("    mandatory: ").append(toIndentedString(mandatory)).append("\n");
            sb.append("    optional: ").append(toIndentedString(optional)).append("\n");
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
     * <p><b>0</b> - OK
     * @param requestParameters The nbleGet request parameters as object
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Void> nbleGet(NbleGetRequest requestParameters) throws WebClientResponseException {
        return this.nbleGet(requestParameters.mandatory(), requestParameters.optional());
    }

    /**
     * 
     * 
     * <p><b>0</b> - OK
     * @param requestParameters The nbleGet request parameters as object
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<Void>> nbleGetWithHttpInfo(NbleGetRequest requestParameters) throws WebClientResponseException {
        return this.nbleGetWithHttpInfo(requestParameters.mandatory(), requestParameters.optional());
    }

    /**
     * 
     * 
     * <p><b>0</b> - OK
     * @param requestParameters The nbleGet request parameters as object
     * @return ResponseSpec
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec nbleGetWithResponseSpec(NbleGetRequest requestParameters) throws WebClientResponseException {
        return this.nbleGetWithResponseSpec(requestParameters.mandatory(), requestParameters.optional());
    }


    /**
     * 
     * 
     * <p><b>0</b> - OK
     * @param mandatory The mandatory parameter
     * @param optional The optional parameter
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec nbleGetRequestCreation(@Nullable String mandatory, @Nullable String optional) throws WebClientResponseException {
        Object postBody = null;
        // verify the required parameter 'mandatory' is set
        if (mandatory == null) {
            throw new WebClientResponseException("Missing the required parameter 'mandatory' when calling nbleGet", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        localVarQueryParams.putAll(apiClient.parameterToMultiValueMap(null, "optional", optional));
        localVarQueryParams.putAll(apiClient.parameterToMultiValueMap(null, "mandatory", mandatory));

        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return apiClient.invokeAPI("/nble", HttpMethod.GET, pathParams, localVarQueryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>0</b> - OK
     * @param mandatory The mandatory parameter
     * @param optional The optional parameter
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Void> nbleGet(@Nullable String mandatory, @Nullable String optional) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return nbleGetRequestCreation(mandatory, optional).bodyToMono(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>0</b> - OK
     * @param mandatory The mandatory parameter
     * @param optional The optional parameter
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<Void>> nbleGetWithHttpInfo(@Nullable String mandatory, @Nullable String optional) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return nbleGetRequestCreation(mandatory, optional).toEntity(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>0</b> - OK
     * @param mandatory The mandatory parameter
     * @param optional The optional parameter
     * @return ResponseSpec
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec nbleGetWithResponseSpec(@Nullable String mandatory, @Nullable String optional) throws WebClientResponseException {
        return nbleGetRequestCreation(mandatory, optional);
    }

    /**
     * 
     * 
     * <p><b>0</b> - response
     * @param requiredAndNullable bodyWithRequiredAndNullableAttributes
     * @return RequiredAndNullable
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec nblePostRequestCreation(RequiredAndNullable requiredAndNullable) throws WebClientResponseException {
        Object postBody = requiredAndNullable;
        // verify the required parameter 'requiredAndNullable' is set
        if (requiredAndNullable == null) {
            throw new WebClientResponseException("Missing the required parameter 'requiredAndNullable' when calling nblePost", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = { 
            "application/json"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<RequiredAndNullable> localVarReturnType = new ParameterizedTypeReference<RequiredAndNullable>() {};
        return apiClient.invokeAPI("/nble", HttpMethod.POST, pathParams, localVarQueryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>0</b> - response
     * @param requiredAndNullable bodyWithRequiredAndNullableAttributes
     * @return RequiredAndNullable
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<RequiredAndNullable> nblePost(RequiredAndNullable requiredAndNullable) throws WebClientResponseException {
        ParameterizedTypeReference<RequiredAndNullable> localVarReturnType = new ParameterizedTypeReference<RequiredAndNullable>() {};
        return nblePostRequestCreation(requiredAndNullable).bodyToMono(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>0</b> - response
     * @param requiredAndNullable bodyWithRequiredAndNullableAttributes
     * @return ResponseEntity&lt;RequiredAndNullable&gt;
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<RequiredAndNullable>> nblePostWithHttpInfo(RequiredAndNullable requiredAndNullable) throws WebClientResponseException {
        ParameterizedTypeReference<RequiredAndNullable> localVarReturnType = new ParameterizedTypeReference<RequiredAndNullable>() {};
        return nblePostRequestCreation(requiredAndNullable).toEntity(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>0</b> - response
     * @param requiredAndNullable bodyWithRequiredAndNullableAttributes
     * @return ResponseSpec
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec nblePostWithResponseSpec(RequiredAndNullable requiredAndNullable) throws WebClientResponseException {
        return nblePostRequestCreation(requiredAndNullable);
    }
}
