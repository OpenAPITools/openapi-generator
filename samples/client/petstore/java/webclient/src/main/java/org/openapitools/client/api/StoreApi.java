package org.openapitools.client.api;

import org.openapitools.client.ApiClient;

import org.openapitools.client.model.Order;

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
public class StoreApi {
    private ApiClient apiClient;

    public StoreApi() {
        this(new ApiClient());
    }

    @Autowired
    public StoreApi(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    public ApiClient getApiClient() {
        return apiClient;
    }

    public void setApiClient(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    /**
     * Delete purchase order by ID
     * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
     * <p><b>400</b> - Invalid ID supplied
     * <p><b>404</b> - Order not found
     * @param orderId ID of the order that needs to be deleted
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec deleteOrderRequestCreation(String orderId) throws WebClientResponseException {
        Object postBody = null;
        // verify the required parameter 'orderId' is set
        if (orderId == null) {
            throw new WebClientResponseException("Missing the required parameter 'orderId' when calling deleteOrder", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        pathParams.put("order_id", orderId);

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return apiClient.invokeAPI("/store/order/{order_id}", HttpMethod.DELETE, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Delete purchase order by ID
     * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
     * <p><b>400</b> - Invalid ID supplied
     * <p><b>404</b> - Order not found
     * @param orderId ID of the order that needs to be deleted
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Void> deleteOrder(String orderId) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return deleteOrderRequestCreation(orderId).bodyToMono(localVarReturnType);
    }

    public Mono<ResponseEntity<Void>> deleteOrderWithHttpInfo(String orderId) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return deleteOrderRequestCreation(orderId).toEntity(localVarReturnType);
    }
    /**
     * Returns pet inventories by status
     * Returns a map of status codes to quantities
     * <p><b>200</b> - successful operation
     * @return Map&lt;String, Integer&gt;
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec getInventoryRequestCreation() throws WebClientResponseException {
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

        String[] localVarAuthNames = new String[] { "api_key" };

        ParameterizedTypeReference<Map<String, Integer>> localVarReturnType = new ParameterizedTypeReference<Map<String, Integer>>() {};
        return apiClient.invokeAPI("/store/inventory", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Returns pet inventories by status
     * Returns a map of status codes to quantities
     * <p><b>200</b> - successful operation
     * @return Map&lt;String, Integer&gt;
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Map<String, Integer>> getInventory() throws WebClientResponseException {
        ParameterizedTypeReference<Map<String, Integer>> localVarReturnType = new ParameterizedTypeReference<Map<String, Integer>>() {};
        return getInventoryRequestCreation().bodyToMono(localVarReturnType);
    }

    public Mono<ResponseEntity<Map<String, Integer>>> getInventoryWithHttpInfo() throws WebClientResponseException {
        ParameterizedTypeReference<Map<String, Integer>> localVarReturnType = new ParameterizedTypeReference<Map<String, Integer>>() {};
        return getInventoryRequestCreation().toEntity(localVarReturnType);
    }
    /**
     * Find purchase order by ID
     * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid ID supplied
     * <p><b>404</b> - Order not found
     * @param orderId ID of pet that needs to be fetched
     * @return Order
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec getOrderByIdRequestCreation(Long orderId) throws WebClientResponseException {
        Object postBody = null;
        // verify the required parameter 'orderId' is set
        if (orderId == null) {
            throw new WebClientResponseException("Missing the required parameter 'orderId' when calling getOrderById", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        pathParams.put("order_id", orderId);

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = { 
            "application/xml", "application/json"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Order> localVarReturnType = new ParameterizedTypeReference<Order>() {};
        return apiClient.invokeAPI("/store/order/{order_id}", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Find purchase order by ID
     * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid ID supplied
     * <p><b>404</b> - Order not found
     * @param orderId ID of pet that needs to be fetched
     * @return Order
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Order> getOrderById(Long orderId) throws WebClientResponseException {
        ParameterizedTypeReference<Order> localVarReturnType = new ParameterizedTypeReference<Order>() {};
        return getOrderByIdRequestCreation(orderId).bodyToMono(localVarReturnType);
    }

    public Mono<ResponseEntity<Order>> getOrderByIdWithHttpInfo(Long orderId) throws WebClientResponseException {
        ParameterizedTypeReference<Order> localVarReturnType = new ParameterizedTypeReference<Order>() {};
        return getOrderByIdRequestCreation(orderId).toEntity(localVarReturnType);
    }
    /**
     * Place an order for a pet
     * 
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid Order
     * @param order order placed for purchasing the pet
     * @return Order
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec placeOrderRequestCreation(Order order) throws WebClientResponseException {
        Object postBody = order;
        // verify the required parameter 'order' is set
        if (order == null) {
            throw new WebClientResponseException("Missing the required parameter 'order' when calling placeOrder", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = { 
            "application/xml", "application/json"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Order> localVarReturnType = new ParameterizedTypeReference<Order>() {};
        return apiClient.invokeAPI("/store/order", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Place an order for a pet
     * 
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid Order
     * @param order order placed for purchasing the pet
     * @return Order
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Order> placeOrder(Order order) throws WebClientResponseException {
        ParameterizedTypeReference<Order> localVarReturnType = new ParameterizedTypeReference<Order>() {};
        return placeOrderRequestCreation(order).bodyToMono(localVarReturnType);
    }

    public Mono<ResponseEntity<Order>> placeOrderWithHttpInfo(Order order) throws WebClientResponseException {
        ParameterizedTypeReference<Order> localVarReturnType = new ParameterizedTypeReference<Order>() {};
        return placeOrderRequestCreation(order).toEntity(localVarReturnType);
    }
}
