package org.openapitools.api.impl;

import org.openapitools.api.*;
import org.openapitools.model.Client;
import java.util.List;
import java.util.Map;

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
public class FakeClassnameTags123ApiServiceImpl implements FakeClassnameTags123Api {
    private JsonCache cache;

    {
        try {
            File cacheFile = new File(System.getProperty("jaxrs.test.server.json",
                    "/Users/joschi/src/openapi-generator/samples/server/petstore/jaxrs-cxf-test-data/src/main/resources/test-data.json"));
            cache = JsonCache.Factory.instance.get("test-data").load(cacheFile).child("/org.openapitools.api/FakeClassnameTags123Api");
        } catch (CacheException e) {
            e.printStackTrace();
        }
    }

    /**
     * To test class name in snake case
     *
     * To test class name in snake case
     *
     */
    @Override
    public Client testClassname(Client body) {
        try {
            Client response = cache.getObject("/testClassname/response", Client.class);
            return response;
        } catch (CacheException e) {
            throw new RuntimeException(e);
        }
    }

}
