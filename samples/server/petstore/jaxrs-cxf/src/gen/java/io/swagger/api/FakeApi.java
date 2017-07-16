package io.swagger.api;

import java.math.BigDecimal;
import io.swagger.model.Client;
import java.util.Date;
import org.joda.time.LocalDate;
import io.swagger.model.OuterComposite;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;
import javax.ws.rs.*;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.MediaType;
import org.apache.cxf.jaxrs.ext.multipart.*;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponses;
import io.swagger.annotations.ApiResponse;
import io.swagger.jaxrs.PATCH;
import javax.validation.constraints.*;
import javax.validation.Valid;

@Path("/")
@Api(value = "/", description = "")
public interface FakeApi  {

    @POST
    @Path("/fake/outer/boolean")
    @ApiOperation(value = "", tags={ "fake",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Output boolean", response = Boolean.class) })
    public Boolean fakeOuterBooleanSerialize(@Valid Boolean body);

    @POST
    @Path("/fake/outer/composite")
    @ApiOperation(value = "", tags={ "fake",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Output composite", response = OuterComposite.class) })
    public OuterComposite fakeOuterCompositeSerialize(@Valid OuterComposite body);

    @POST
    @Path("/fake/outer/number")
    @ApiOperation(value = "", tags={ "fake",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Output number", response = BigDecimal.class) })
    public BigDecimal fakeOuterNumberSerialize(@Valid BigDecimal body);

    @POST
    @Path("/fake/outer/string")
    @ApiOperation(value = "", tags={ "fake",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Output string", response = String.class) })
    public String fakeOuterStringSerialize(@Valid String body);

    @PATCH
    @Path("/fake")
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @ApiOperation(value = "To test \"client\" model", tags={ "fake",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Client.class) })
    public Client testClientModel(@Valid Client body);

    @POST
    @Path("/fake")
    @Consumes({ "application/xml; charset=utf-8", "application/json; charset=utf-8" })
    @Produces({ "application/xml; charset=utf-8", "application/json; charset=utf-8" })
    @ApiOperation(value = "Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 ", tags={ "fake",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid username supplied"),
        @ApiResponse(code = 404, message = "User not found") })
    public void testEndpointParameters(@Multipart(value = "number")  BigDecimal number, @Multipart(value = "double")  Double _double, @Multipart(value = "pattern_without_delimiter")  String patternWithoutDelimiter, @Multipart(value = "byte")  byte[] _byte, @Multipart(value = "integer", required = false)  Integer integer, @Multipart(value = "int32", required = false)  Integer int32, @Multipart(value = "int64", required = false)  Long int64, @Multipart(value = "float", required = false)  Float _float, @Multipart(value = "string", required = false)  String string, @Multipart(value = "binary", required = false)  byte[] binary, @Multipart(value = "date", required = false)  LocalDate date, @Multipart(value = "dateTime", required = false)  Date dateTime, @Multipart(value = "password", required = false)  String password, @Multipart(value = "callback", required = false)  String paramCallback);

    @GET
    @Path("/fake")
    @Consumes({ "*/*" })
    @Produces({ "*/*" })
    @ApiOperation(value = "To test enum parameters", tags={ "fake",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid request"),
        @ApiResponse(code = 404, message = "Not found") })
    public void testEnumParameters(@Multipart(value = "enum_form_string_array", required = false)  List<String> enumFormStringArray, @Multipart(value = "enum_form_string", required = false)  String enumFormString, @HeaderParam("enum_header_string_array") List<String> enumHeaderStringArray, @HeaderParam("enum_header_string") String enumHeaderString, @QueryParam("enum_query_string_array") List<String> enumQueryStringArray, @QueryParam("enum_query_string") @DefaultValue("-efg") String enumQueryString, @QueryParam("enum_query_integer") Integer enumQueryInteger, @Multipart(value = "enum_query_double", required = false)  Double enumQueryDouble);

    @GET
    @Path("/fake/jsonFormData")
    @Consumes({ "application/json" })
    @ApiOperation(value = "test json serialization of form data", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation") })
    public void testJsonFormData(@Multipart(value = "param")  String param, @Multipart(value = "param2")  String param2);
}

