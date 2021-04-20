package org.openapitools.api;

import java.math.BigDecimal;
import org.openapitools.model.TestResponse;
import org.openapitools.api.TestHeadersApiService;

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
@Path("/test-headers")
@RequestScoped

@Api(description = "the test-headers API")


@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSCXFCDIServerCodegen")

public class TestHeadersApi  {

  @Context SecurityContext securityContext;

  @Inject TestHeadersApiService delegate;


    @GET
    
    
    @Produces({ "application/json" })
    @ApiOperation(value = "test headers", notes = "desc", response = TestResponse.class, tags={ "verify-default-value" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "default response", response = TestResponse.class) })
    public Response headersTest( @ApiParam(value = "" , defaultValue="11.2")@HeaderParam("headerNumber") BigDecimal headerNumber,  @ApiParam(value = "" , defaultValue="qwerty")@HeaderParam("headerString") String headerString,  @ApiParam(value = "" , defaultValue="qwerty")@HeaderParam("headerStringWrapped") String headerStringWrapped,  @ApiParam(value = "" , defaultValue="qwerty\"with quotes\" test")@HeaderParam("headerStringQuotes") String headerStringQuotes,  @ApiParam(value = "" , defaultValue="qwerty\"with quotes\" test")@HeaderParam("headerStringQuotesWrapped") String headerStringQuotesWrapped,  @ApiParam(value = "" , defaultValue="true")@HeaderParam("headerBoolean") Boolean headerBoolean) {
        return delegate.headersTest(headerNumber, headerString, headerStringWrapped, headerStringQuotes, headerStringQuotesWrapped, headerBoolean, securityContext);
    }
}
