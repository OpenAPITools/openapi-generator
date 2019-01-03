package org.openapitools.api;

import org.openapitools.model.XmlItem;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;
import javax.ws.rs.*;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.MediaType;
import org.apache.cxf.jaxrs.ext.multipart.*;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponses;
import io.swagger.annotations.ApiResponse;
import io.swagger.jaxrs.PATCH;
import javax.validation.constraints.*;
import javax.validation.Valid;

/**
 * OpenAPI Petstore
 *
 * <p>This spec is mainly for testing Petstore server and contains fake endpoints, models. Please do not use this for any other purpose. Special characters: \" \\
 *
 */
@Path("/")
@Api(value = "/", description = "")
public interface DefaultApi  {

    /**
     * creates an XmlItem
     *
     * this route creates an XmlItem
     *
     */
    @POST
    @Path("/fake/create_xml_item")
    @Consumes({ "application/xml", "application/xml; charset=utf-8", "application/xml; charset=utf-16", "text/xml", "text/xml; charset=utf-8", "text/xml; charset=utf-16" })
    @ApiOperation(value = "creates an XmlItem", tags={  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation") })
    public void createXmlItem(@Valid XmlItem xmlItem);
}

