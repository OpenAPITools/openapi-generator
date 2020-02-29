package org.openapitools.client.apis;

import java.lang.System;

@kotlin.Metadata(mv = {1, 1, 16}, bv = {1, 0, 3}, k = 1, d1 = {"\u00000\n\u0002\u0018\u0002\n\u0002\u0018\u0002\n\u0000\n\u0002\u0010\u000e\n\u0002\b\u0002\n\u0002\u0010\u0002\n\u0002\b\u0002\n\u0002\u0010$\n\u0002\u0010\b\n\u0000\n\u0002\u0018\u0002\n\u0002\u0010\t\n\u0002\b\u0003\u0018\u00002\u00020\u0001B\u000f\u0012\b\b\u0002\u0010\u0002\u001a\u00020\u0003\u00a2\u0006\u0002\u0010\u0004J\u000e\u0010\u0005\u001a\u00020\u00062\u0006\u0010\u0007\u001a\u00020\u0003J\u0012\u0010\b\u001a\u000e\u0012\u0004\u0012\u00020\u0003\u0012\u0004\u0012\u00020\n0\tJ\u000e\u0010\u000b\u001a\u00020\f2\u0006\u0010\u0007\u001a\u00020\rJ\u000e\u0010\u000e\u001a\u00020\f2\u0006\u0010\u000f\u001a\u00020\f\u00a8\u0006\u0010"}, d2 = {"Lorg/openapitools/client/apis/StoreApi;", "Lorg/openapitools/client/infrastructure/ApiClient;", "basePath", "", "(Ljava/lang/String;)V", "deleteOrder", "", "orderId", "getInventory", "", "", "getOrderById", "Lorg/openapitools/client/models/Order;", "", "placeOrder", "body", "kotlin-petstore-moshi-codegen"})
public final class StoreApi extends org.openapitools.client.infrastructure.ApiClient {
    
    /**
     * Delete purchase order by ID
     * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
     * @param orderId ID of the order that needs to be deleted 
     * @return void
     * @throws UnsupportedOperationException If the API returns an informational or redirection response
     * @throws ClientException If the API returns a client error response
     * @throws ServerException If the API returns a server error response
     */
    public final void deleteOrder(@org.jetbrains.annotations.NotNull()
    java.lang.String orderId) throws java.lang.UnsupportedOperationException, org.openapitools.client.infrastructure.ClientException, org.openapitools.client.infrastructure.ServerException {
    }
    
    /**
     * Returns pet inventories by status
     * Returns a map of status codes to quantities
     * @return kotlin.collections.Map<kotlin.String, kotlin.Int>
     * @throws UnsupportedOperationException If the API returns an informational or redirection response
     * @throws ClientException If the API returns a client error response
     * @throws ServerException If the API returns a server error response
     */
    @org.jetbrains.annotations.NotNull()
    @kotlin.Suppress(names = {"UNCHECKED_CAST"})
    public final java.util.Map<java.lang.String, java.lang.Integer> getInventory() {
        return null;
    }
    
    /**
     * Find purchase order by ID
     * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
     * @param orderId ID of pet that needs to be fetched 
     * @return Order
     * @throws UnsupportedOperationException If the API returns an informational or redirection response
     * @throws ClientException If the API returns a client error response
     * @throws ServerException If the API returns a server error response
     */
    @org.jetbrains.annotations.NotNull()
    @kotlin.Suppress(names = {"UNCHECKED_CAST"})
    public final org.openapitools.client.models.Order getOrderById(long orderId) throws java.lang.UnsupportedOperationException, org.openapitools.client.infrastructure.ClientException, org.openapitools.client.infrastructure.ServerException {
        return null;
    }
    
    /**
     * Place an order for a pet
     *
     * @param body order placed for purchasing the pet 
     * @return Order
     * @throws UnsupportedOperationException If the API returns an informational or redirection response
     * @throws ClientException If the API returns a client error response
     * @throws ServerException If the API returns a server error response
     */
    @org.jetbrains.annotations.NotNull()
    @kotlin.Suppress(names = {"UNCHECKED_CAST"})
    public final org.openapitools.client.models.Order placeOrder(@org.jetbrains.annotations.NotNull()
    org.openapitools.client.models.Order body) throws java.lang.UnsupportedOperationException, org.openapitools.client.infrastructure.ClientException, org.openapitools.client.infrastructure.ServerException {
        return null;
    }
    
    public StoreApi(@org.jetbrains.annotations.NotNull()
    java.lang.String basePath) {
        super(null);
    }
    
    public StoreApi() {
        super(null);
    }
}