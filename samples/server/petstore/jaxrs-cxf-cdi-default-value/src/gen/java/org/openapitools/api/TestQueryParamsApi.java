package org.openapitools.api;

import java.math.BigDecimal;
import org.openapitools.model.TestResponse;
import org.openapitools.api.TestQueryParamsApiService;

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
@Path("/test-query-params")
@RequestScoped

@Api(description = "the test-query-params API")


@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSCXFCDIServerCodegen")

public class TestQueryParamsApi  {

  @Context SecurityContext securityContext;

  @Inject TestQueryParamsApiService delegate;


    @GET
    
    
    @Produces({ "application/json" })
    @ApiOperation(value = "test query params", notes = "desc", response = TestResponse.class, tags={ "verify-default-value" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "default response", response = TestResponse.class) })
    public Response queryParamsTest(@ApiParam(value = "", defaultValue="11.2") @DefaultValue("11.2")  @QueryParam("queryNumber") BigDecimal queryNumber, @ApiParam(value = "", defaultValue="qwerty") @DefaultValue("qwerty")  @QueryParam("queryString") String queryString, @ApiParam(value = "", defaultValue="qwerty") @DefaultValue("qwerty")  @QueryParam("queryStringWrapped") String queryStringWrapped, @ApiParam(value = "", defaultValue="qwerty\"with quotes\" test") @DefaultValue("qwerty\"with quotes\" test")  @QueryParam("queryStringQuotes") String queryStringQuotes, @ApiParam(value = "", defaultValue="qwerty\"with quotes\" test") @DefaultValue("qwerty\"with quotes\" test")  @QueryParam("queryStringQuotesWrapped") String queryStringQuotesWrapped, @ApiParam(value = "", defaultValue="true") @DefaultValue("true")  @QueryParam("queryBoolean") Boolean queryBoolean) {
        return delegate.queryParamsTest(queryNumber, queryString, queryStringWrapped, queryStringQuotes, queryStringQuotesWrapped, queryBoolean, securityContext);
    }
}
