package org.openapitools.api.impl;

import org.openapitools.api.*;
import java.util.Map;
import org.openapitools.model.Order;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;
import javax.ws.rs.*;
import javax.ws.rs.core.Response;
import org.apache.cxf.jaxrs.model.wadl.Description;
import org.apache.cxf.jaxrs.model.wadl.DocTarget;

import org.apache.cxf.jaxrs.ext.multipart.*;

import io.swagger.annotations.Api;

/**
 * OpenAPI Petstore
 *
 * <p>This spec is mainly for testing Petstore server and contains fake endpoints, models. Please do not use this for any other purpose. Special characters: \" \\
 *
 */
public class StoreApiServiceImpl implements StoreApi {
    /**
     * Delete purchase order by ID
     *
     * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
     *
     */
    public void deleteOrder(String orderId) {
        // TODO: Implement...

        
    }

    /**
     * Returns pet inventories by status
     *
     * Returns a map of status codes to quantities
     *
     */
    public Map<String, Integer> getInventory() {
        // TODO: Implement...

        return null;
    }

    /**
     * Find purchase order by ID
     *
     * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
     *
     */
    public Order getOrderById(Long orderId) {
        // TODO: Implement...

        return null;
    }

    /**
     * Place an order for a pet
     *
     */
    public Order placeOrder(Order body) {
        // TODO: Implement...

        return null;
    }

}
