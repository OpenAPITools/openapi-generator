package io.swagger.api;

import java.math.BigDecimal;
import io.swagger.model.Client;
import org.joda.time.LocalDate;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import io.swagger.annotations.*;

import java.util.List;
import javax.validation.constraints.*;

@Path("/fake")

@Api(description = "the fake API")




public class FakeApi  {

    @PATCH
    
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @ApiOperation(value = "To test \"client\" model", notes = "To test \"client\" model", response = Client.class, tags={ "fake",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Client.class) })
    public Response testClientModel(Client body) {
    	return Response.ok().entity("magic!").build();
    }

    @POST
    
    @Consumes({ "application/xml; charset&#x3D;utf-8", "application/json; charset&#x3D;utf-8" })
    @Produces({ "application/xml; charset&#x3D;utf-8", "application/json; charset&#x3D;utf-8" })
    @ApiOperation(value = "Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 ", notes = "Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 ", response = void.class, authorizations = {
        @Authorization(value = "http_basic_test")
    }, tags={ "fake",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid username supplied", response = void.class),
        @ApiResponse(code = 404, message = "User not found", response = void.class) })
    public Response testEndpointParameters(@FormParam(value = "number")  BigDecimal number,@FormParam(value = "_double")  Double _double,@FormParam(value = "patternWithoutDelimiter")  String patternWithoutDelimiter,@FormParam(value = "_byte")  byte[] _byte,@FormParam(value = "integer")  Integer integer,@FormParam(value = "int32")  Integer int32,@FormParam(value = "int64")  Long int64,@FormParam(value = "_float")  Float _float,@FormParam(value = "string")  String string,@FormParam(value = "binary")  byte[] binary,@FormParam(value = "date")  LocalDate date,@FormParam(value = "dateTime")  javax.xml.datatype.XMLGregorianCalendar dateTime,@FormParam(value = "password")  String password,@FormParam(value = "paramCallback")  String paramCallback) {
    	return Response.ok().entity("magic!").build();
    }

    @GET
    
    @Consumes({ "*/*" })
    @Produces({ "*/*" })
    @ApiOperation(value = "To test enum parameters", notes = "To test enum parameters", response = void.class, tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid request", response = void.class),
        @ApiResponse(code = 404, message = "Not found", response = void.class) })
    public Response testEnumParameters(@FormParam(value = "enumFormStringArray")  List<String> enumFormStringArray,@FormParam(value = "enumFormString")  String enumFormString,@HeaderParam("enum_header_string_array") List<String> enumHeaderStringArray,@HeaderParam("enum_header_string") String enumHeaderString,@QueryParam("enum_query_string_array")  List<String> enumQueryStringArray,@QueryParam("enum_query_string")  String enumQueryString,@QueryParam("enum_query_integer")  Integer enumQueryInteger,@FormParam(value = "enumQueryDouble")  Double enumQueryDouble) {
    	return Response.ok().entity("magic!").build();
    }
}

