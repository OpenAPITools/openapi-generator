package org.openapitools.api;

import org.openapitools.model.*;
import org.openapitools.api.TestHeadersApiService;

import io.swagger.annotations.ApiParam;
import io.swagger.jaxrs.*;

import java.math.BigDecimal;
import org.openapitools.model.TestResponse;

import java.util.Map;
import java.util.List;
import org.openapitools.api.NotFoundException;

import java.io.InputStream;

import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.*;
import javax.inject.Inject;

import javax.validation.constraints.*;
import javax.validation.Valid;

@Path("/test-headers")


@io.swagger.annotations.Api(description = "the test-headers API")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaResteasyServerCodegen")
public class TestHeadersApi  {

    @Inject TestHeadersApiService service;

    @GET
    
    
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "test headers", notes = "desc", response = TestResponse.class, tags={ "verify-default-value", })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "default response", response = TestResponse.class) })
    public Response headersTest( @ApiParam(value = "" , defaultValue="11.2") @HeaderParam("headerNumber") BigDecimal headerNumber, @ApiParam(value = "" , defaultValue="qwerty") @HeaderParam("headerString") String headerString, @ApiParam(value = "" , defaultValue="qwerty") @HeaderParam("headerStringWrapped") String headerStringWrapped, @ApiParam(value = "" , defaultValue="qwerty\"with quotes\" test") @HeaderParam("headerStringQuotes") String headerStringQuotes, @ApiParam(value = "" , defaultValue="qwerty\"with quotes\" test") @HeaderParam("headerStringQuotesWrapped") String headerStringQuotesWrapped, @ApiParam(value = "" , defaultValue="true") @HeaderParam("headerBoolean") Boolean headerBoolean,@Context SecurityContext securityContext)
    throws NotFoundException {
        return service.headersTest(headerNumber,headerString,headerStringWrapped,headerStringQuotes,headerStringQuotesWrapped,headerBoolean,securityContext);
    }
}
