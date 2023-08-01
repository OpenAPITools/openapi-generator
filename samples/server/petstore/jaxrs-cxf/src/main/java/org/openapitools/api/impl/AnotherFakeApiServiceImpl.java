package org.openapitools.api.impl;

import org.openapitools.api.*;
import org.openapitools.model.Client;

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
public class AnotherFakeApiServiceImpl implements AnotherFakeApi {
    /**
     * To test special tags
     *
     * To test special tags and operation ID starting with number
     *
     */
    public Client call123testSpecialTags(Client body) {
        // TODO: Implement...

        return null;
    }

}
