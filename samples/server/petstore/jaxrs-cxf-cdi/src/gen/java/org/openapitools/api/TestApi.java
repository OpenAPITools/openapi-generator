package org.openapitools.api;

import org.openapitools.api.TestApiService;

import jakarta.ws.rs.*;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.core.SecurityContext;
import jakarta.enterprise.context.RequestScoped;
import jakarta.inject.Inject;

import io.swagger.annotations.*;
import java.io.InputStream;

import org.apache.cxf.jaxrs.ext.PATCH;
import org.apache.cxf.jaxrs.ext.multipart.Attachment;
import org.apache.cxf.jaxrs.ext.multipart.Multipart;

import java.util.Map;
import java.util.List;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;
@Path("/test/upload")
@RequestScoped

@Api(description = "the test API")


@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSCXFCDIServerCodegen", comments = "Generator version: 7.5.0-SNAPSHOT")

public class TestApi  {

  @Context SecurityContext securityContext;

  @Inject TestApiService delegate;


    @POST
    
    @Consumes({ "application/octet-stream" })
    
    @ApiOperation(value = "test upload", notes = "upload test", response = Void.class, tags={  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "whatever", response = Void.class) })
    public Response testUpload(@ApiParam(value = "" ) java.io.InputStream body) {
        return delegate.testUpload(body, securityContext);
    }
}
