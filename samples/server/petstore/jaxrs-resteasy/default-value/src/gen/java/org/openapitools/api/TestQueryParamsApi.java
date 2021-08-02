package org.openapitools.api;

import org.openapitools.model.*;
import org.openapitools.api.TestQueryParamsApiService;



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

@Path("/test-query-params")


@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaResteasyServerCodegen")
public class TestQueryParamsApi  {

    @Inject TestQueryParamsApiService service;

    @GET
    
    
    @Produces({ "application/json" })
    public Response queryParamsTest(  @DefaultValue("11.2") @QueryParam("queryNumber") BigDecimal queryNumber,  @DefaultValue("qwerty") @QueryParam("queryString") String queryString,  @DefaultValue("qwerty") @QueryParam("queryStringWrapped") String queryStringWrapped,  @DefaultValue("qwerty\"with quotes\" test") @QueryParam("queryStringQuotes") String queryStringQuotes,  @DefaultValue("qwerty\"with quotes\" test") @QueryParam("queryStringQuotesWrapped") String queryStringQuotesWrapped,  @DefaultValue("true") @QueryParam("queryBoolean") Boolean queryBoolean,@Context SecurityContext securityContext)
    throws NotFoundException {
        return service.queryParamsTest(queryNumber,queryString,queryStringWrapped,queryStringQuotes,queryStringQuotesWrapped,queryBoolean,securityContext);
    }
}
