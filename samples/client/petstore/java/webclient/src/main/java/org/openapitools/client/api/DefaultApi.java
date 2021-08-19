package org.openapitools.client.api;

import org.openapitools.client.ApiClient;

import org.openapitools.client.model.InlineResponseDefault;

import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.web.reactive.function.client.WebClient.ResponseSpec;
import org.springframework.web.reactive.function.client.WebClientResponseException;
import org.springframework.core.io.FileSystemResource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Flux;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen")
public class DefaultApi {
    private ApiClient apiClient;

    public DefaultApi() {
        this(new ApiClient());
    }

    @Autowired
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
     * <p><b>0</b> - response
     * @return InlineResponseDefault
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec fooGetRequestCreation() throws WebClientResponseException {
        Object postBody = null;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = { 
            "application/json"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<InlineResponseDefault> localVarReturnType = new ParameterizedTypeReference<InlineResponseDefault>() {};
        return apiClient.invokeAPI("/foo", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>0</b> - response
     * @return InlineResponseDefault
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<InlineResponseDefault> fooGet() throws WebClientResponseException {
        ParameterizedTypeReference<InlineResponseDefault> localVarReturnType = new ParameterizedTypeReference<InlineResponseDefault>() {};
        return fooGetRequestCreation().bodyToMono(localVarReturnType);
    }

    public Mono<ResponseEntity<InlineResponseDefault>> fooGetWithHttpInfo() throws WebClientResponseException {
        ParameterizedTypeReference<InlineResponseDefault> localVarReturnType = new ParameterizedTypeReference<InlineResponseDefault>() {};
        return fooGetRequestCreation().toEntity(localVarReturnType);
    }
}
