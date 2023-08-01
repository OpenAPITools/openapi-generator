package org.openapitools.api;

import org.openapitools.api.TestApiService;

import javax.ws.rs.*;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.enterprise.context.RequestScoped;
import javax.inject.Inject;

import io.swagger.annotations.*;
import java.io.InputStream;

import org.apache.cxf.jaxrs.ext.PATCH;
import org.apache.cxf.jaxrs.ext.multipart.Attachment;
import org.apache.cxf.jaxrs.ext.multipart.Multipart;

import java.util.Map;
import java.util.List;
import javax.validation.constraints.*;
@Path("/test/upload")
@RequestScoped

@Api(description = "the test API")


@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSCXFCDIServerCodegen")

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
