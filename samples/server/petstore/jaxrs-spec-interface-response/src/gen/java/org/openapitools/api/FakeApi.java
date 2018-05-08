package org.openapitools.api;

import java.math.BigDecimal;
import org.openapitools.model.Client;
import java.util.Date;
import java.io.File;
import org.joda.time.LocalDate;
import java.util.Map;
import org.openapitools.model.OuterComposite;
import org.openapitools.model.User;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import io.swagger.annotations.*;

import java.util.Map;
import java.util.List;
import javax.validation.constraints.*;
import javax.validation.Valid;

@Path("/fake")
@Api(description = "the fake API")
public interface FakeApi {

    @POST
    @Path("/outer/boolean")
    @Produces({ "*/*" })
    @ApiOperation(value = "", notes = "Test serialization of outer boolean types", tags={ "fake",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Output boolean", response = Boolean.class) })
    Response fakeOuterBooleanSerialize(@Valid Boolean body);

    @POST
    @Path("/outer/composite")
    @Produces({ "*/*" })
    @ApiOperation(value = "", notes = "Test serialization of object with outer number type", tags={ "fake",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Output composite", response = OuterComposite.class) })
    Response fakeOuterCompositeSerialize(@Valid OuterComposite outerComposite);

    @POST
    @Path("/outer/number")
    @Produces({ "*/*" })
    @ApiOperation(value = "", notes = "Test serialization of outer number types", tags={ "fake",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Output number", response = BigDecimal.class) })
    Response fakeOuterNumberSerialize(@Valid BigDecimal body);

    @POST
    @Path("/outer/string")
    @Produces({ "*/*" })
    @ApiOperation(value = "", notes = "Test serialization of outer string types", tags={ "fake",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Output string", response = String.class) })
    Response fakeOuterStringSerialize(@Valid String body);

    @PUT
    @Path("/body-with-query-params")
    @Consumes({ "application/json" })
    @ApiOperation(value = "", notes = "", tags={ "fake",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Success", response = Void.class) })
    Response testBodyWithQueryParams(@QueryParam("query") @NotNull    String query,@Valid User user);

    @PATCH
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @ApiOperation(value = "To test \"client\" model", notes = "To test \"client\" model", tags={ "fake",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Client.class) })
    Response testClientModel(@Valid Client client);

    @POST
    @Consumes({ "application/x-www-form-urlencoded" })
    @ApiOperation(value = "Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 ", notes = "Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 ", authorizations = {
        @Authorization(value = "http_basic_test")
    }, tags={ "fake",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid username supplied", response = Void.class),
        @ApiResponse(code = 404, message = "User not found", response = Void.class) })
    Response testEndpointParameters(@FormParam(value = "number")  BigDecimal number,@FormParam(value = "double")  Double _double,@FormParam(value = "pattern_without_delimiter")  String patternWithoutDelimiter,@FormParam(value = "byte")  byte[] _byte,@FormParam(value = "integer")  Integer integer,@FormParam(value = "int32")  Integer int32,@FormParam(value = "int64")  Long int64,@FormParam(value = "float")  Float _float,@FormParam(value = "string")  String string, @FormParam(value = "binary") InputStream binaryInputStream,
   @FormParam(value = "binary") Attachment binaryDetail,@FormParam(value = "date")  LocalDate date,@FormParam(value = "dateTime")  Date dateTime,@FormParam(value = "password")  String password,@FormParam(value = "callback")  String paramCallback);

    @GET
    @Consumes({ "application/x-www-form-urlencoded" })
    @ApiOperation(value = "To test enum parameters", notes = "To test enum parameters", tags={ "fake",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid request", response = Void.class),
        @ApiResponse(code = 404, message = "Not found", response = Void.class) })
    Response testEnumParameters(@HeaderParam("enum_header_string_array")   @ApiParam("Header parameter enum test (string array)") List<String> enumHeaderStringArray,@HeaderParam("enum_header_string")  @DefaultValue("-efg")  @ApiParam("Header parameter enum test (string)") String enumHeaderString,@QueryParam("enum_query_string_array")   @ApiParam("Query parameter enum test (string array)")  List<String> enumQueryStringArray,@QueryParam("enum_query_string")  @DefaultValue("-efg")  @ApiParam("Query parameter enum test (string)")  String enumQueryString,@QueryParam("enum_query_integer")   @ApiParam("Query parameter enum test (double)")  Integer enumQueryInteger,@QueryParam("enum_query_double")   @ApiParam("Query parameter enum test (double)")  Double enumQueryDouble,@FormParam(value = "enum_form_string_array")  List<String> enumFormStringArray,@FormParam(value = "enum_form_string")  String enumFormString);

    @POST
    @Path("/inline-additionalProperties")
    @Consumes({ "application/json" })
    @ApiOperation(value = "test inline additionalProperties", notes = "", tags={ "fake",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class) })
    Response testInlineAdditionalProperties(@Valid Map<String, String> requestBody);

    @GET
    @Path("/jsonFormData")
    @Consumes({ "application/x-www-form-urlencoded" })
    @ApiOperation(value = "test json serialization of form data", notes = "", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class) })
    Response testJsonFormData(@FormParam(value = "param")  String param,@FormParam(value = "param2")  String param2);
}
