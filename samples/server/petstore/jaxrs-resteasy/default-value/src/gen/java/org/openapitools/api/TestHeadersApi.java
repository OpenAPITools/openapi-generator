package org.openapitools.api;

import org.openapitools.model.*;
import org.openapitools.api.TestHeadersApiService;



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


@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaResteasyServerCodegen")
public class TestHeadersApi  {

    @Inject TestHeadersApiService service;

    @GET
    
    
    @Produces({ "application/json" })
    public Response headersTest(   @HeaderParam("headerNumber") BigDecimal headerNumber,   @HeaderParam("headerString") String headerString,   @HeaderParam("headerStringWrapped") String headerStringWrapped,   @HeaderParam("headerStringQuotes") String headerStringQuotes,   @HeaderParam("headerStringQuotesWrapped") String headerStringQuotesWrapped,   @HeaderParam("headerBoolean") Boolean headerBoolean,@Context SecurityContext securityContext)
    throws NotFoundException {
        return service.headersTest(headerNumber,headerString,headerStringWrapped,headerStringQuotes,headerStringQuotesWrapped,headerBoolean,securityContext);
    }
}
