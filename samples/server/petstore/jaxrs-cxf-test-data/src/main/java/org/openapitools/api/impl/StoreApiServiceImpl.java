package org.openapitools.api.impl;

import org.openapitools.api.*;
import java.util.List;
import java.util.Map;
import org.openapitools.model.Order;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;
import java.io.File;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.openapitools.codegen.utils.JsonCache;
import org.openapitools.codegen.utils.JsonCache.CacheException;
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
    private JsonCache cache;

    {
        try {
            File cacheFile = new File(System.getProperty("jaxrs.test.server.json",
                    "/Users/williamcheng/Code/openapi-generator/samples/server/petstore/jaxrs-cxf-test-data/src/main/resources/test-data.json"));
            cache = JsonCache.Factory.instance.get("test-data").load(cacheFile).child("/org.openapitools.api/StoreApi");
        } catch (CacheException e) {
            e.printStackTrace();
        }
    }

    /**
     * Delete purchase order by ID
     *
     * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
     *
     */
    @Override
    public void deleteOrder(String orderId) {

    }

    /**
     * Returns pet inventories by status
     *
     * Returns a map of status codes to quantities
     *
     */
    @Override
    public Map<String, Integer> getInventory() {
        try {
            Map<String, Integer> response = cache.getObject("/getInventory/response", Map.class);
            return response;
        } catch (CacheException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Find purchase order by ID
     *
     * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
     *
     */
    @Override
    public Order getOrderById(Long orderId) {
        try {
            Order response = cache.getObject("/getOrderById/response", Order.class);
            return response;
        } catch (CacheException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Place an order for a pet
     *
     */
    @Override
    public Order placeOrder(Order body) {
        try {
            Order response = cache.getObject("/placeOrder/response", Order.class);
            return response;
        } catch (CacheException e) {
            throw new RuntimeException(e);
        }
    }

}
