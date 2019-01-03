package org.openapitools.api;

import org.openapitools.model.*;
import org.openapitools.api.DefaultApiService;
import org.openapitools.api.factories.DefaultApiServiceFactory;

import io.swagger.annotations.ApiParam;
import io.swagger.jaxrs.*;

import org.openapitools.model.XmlItem;

import java.util.Map;
import java.util.List;
import org.openapitools.api.NotFoundException;

import java.io.InputStream;

import com.sun.jersey.core.header.FormDataContentDisposition;
import com.sun.jersey.multipart.FormDataParam;

import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.*;
import javax.validation.constraints.*;
import javax.validation.Valid;

@Path("/Default")


@io.swagger.annotations.Api(description = "the Default API")

public class DefaultApi  {
   private final DefaultApiService delegate = DefaultApiServiceFactory.getDefaultApi();

    @POST
    @Path("/fake/create_xml_item")
    @Consumes({ "application/xml", "application/xml; charset=utf-8", "application/xml; charset=utf-16", "text/xml", "text/xml; charset=utf-8", "text/xml; charset=utf-16" })
    
    @io.swagger.annotations.ApiOperation(value = "creates an XmlItem", notes = "this route creates an XmlItem", response = Void.class, tags={  })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = Void.class) })
    public Response createXmlItem(
        @ApiParam(value = "XmlItem Body" ,required=true) @Valid XmlItem xmlItem,
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.createXmlItem(xmlItem,securityContext);
    }
}
