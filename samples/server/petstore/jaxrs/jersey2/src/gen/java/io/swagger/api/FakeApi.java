package io.swagger.api;

import io.swagger.model.*;
import io.swagger.api.FakeApiService;
import io.swagger.api.factories.FakeApiServiceFactory;

import io.swagger.annotations.ApiParam;
import io.swagger.jaxrs.*;

import io.swagger.model.Client;
import java.util.Date;
import java.math.BigDecimal;

import java.util.List;
import io.swagger.api.NotFoundException;

import java.io.InputStream;

import org.glassfish.jersey.media.multipart.FormDataContentDisposition;
import org.glassfish.jersey.media.multipart.FormDataParam;

import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.*;

@Path("/fake")


@io.swagger.annotations.Api(description = "the fake API")

public class FakeApi  {
   private final FakeApiService delegate = FakeApiServiceFactory.getFakeApi();

    @PATCH
    
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "To test \"client\" model", notes = "", response = Client.class, tags={ "fake", })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = Client.class) })
    public Response testClientModel(@ApiParam(value = "client model" ,required=true) Client body
,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.testClientModel(body,securityContext);
    }
    @POST
    
    @Consumes({ "application/xml; charset=utf-8", "application/json; charset=utf-8" })
    @Produces({ "application/xml; charset=utf-8", "application/json; charset=utf-8" })
    @io.swagger.annotations.ApiOperation(value = "Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 ", notes = "Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 ", response = void.class, tags={ "fake", })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid username supplied", response = void.class),
        
        @io.swagger.annotations.ApiResponse(code = 404, message = "User not found", response = void.class) })
    public Response testEndpointParameters(@ApiParam(value = "None", required=true)  @FormParam("number")  BigDecimal number
,@ApiParam(value = "None", required=true)  @FormParam("double")  Double _double
,@ApiParam(value = "None", required=true)  @FormParam("string")  String string
,@ApiParam(value = "None", required=true)  @FormParam("byte")  byte[] _byte
,@ApiParam(value = "None")  @FormParam("integer")  Integer integer
,@ApiParam(value = "None")  @FormParam("int32")  Integer int32
,@ApiParam(value = "None")  @FormParam("int64")  Long int64
,@ApiParam(value = "None")  @FormParam("float")  Float _float
,@ApiParam(value = "None")  @FormParam("binary")  byte[] binary
,@ApiParam(value = "None")  @FormParam("date")  Date date
,@ApiParam(value = "None")  @FormParam("dateTime")  Date dateTime
,@ApiParam(value = "None")  @FormParam("password")  String password
,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.testEndpointParameters(number,_double,string,_byte,integer,int32,int64,_float,binary,date,dateTime,password,securityContext);
    }
    @GET
    
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "To test enum query parameters", notes = "", response = void.class, tags={ "fake", })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid request", response = void.class),
        
        @io.swagger.annotations.ApiResponse(code = 404, message = "Not found", response = void.class) })
    public Response testEnumQueryParameters(@ApiParam(value = "Query parameter enum test (string)", allowableValues="_abc, -efg, (xyz)", defaultValue="-efg")  @DefaultValue("-efg") @FormParam("enum_query_string")  String enumQueryString
,@ApiParam(value = "Query parameter enum test (double)") @QueryParam("enum_query_integer") BigDecimal enumQueryInteger
,@ApiParam(value = "Query parameter enum test (double)")  @FormParam("enum_query_double")  Double enumQueryDouble
,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.testEnumQueryParameters(enumQueryString,enumQueryInteger,enumQueryDouble,securityContext);
    }
}
