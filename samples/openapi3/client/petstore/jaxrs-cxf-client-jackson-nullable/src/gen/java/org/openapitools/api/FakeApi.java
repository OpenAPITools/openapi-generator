package org.openapitools.api;

import java.math.BigDecimal;
import org.openapitools.model.Client;
import java.util.Date;
import java.io.File;
import org.openapitools.model.FileSchemaTestClass;
import org.openapitools.model.HealthCheckResult;
import org.joda.time.LocalDate;
import org.openapitools.model.OuterComposite;
import org.openapitools.model.Pet;
import org.openapitools.model.User;

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

/**
 * OpenAPI Petstore
 *
 * <p>This spec is mainly for testing Petstore server and contains fake endpoints, models. Please do not use this for any other purpose. Special characters: \" \\
 *
 */
@Path("/fake")
@Api(value = "/", description = "")
public interface FakeApi  {

    /**
     * Health check endpoint
     *
     */
    @GET
    @Path("/health")
    @Produces({ "application/json" })
    @ApiOperation(value = "Health check endpoint", tags={  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "The instance started successfully", response = HealthCheckResult.class) })
    public HealthCheckResult fakeHealthGet();

    /**
     * test http signature authentication
     *
     */
    @GET
    @Path("/http-signature-test")
    @Consumes({ "application/json", "application/xml" })
    @ApiOperation(value = "test http signature authentication", tags={  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "The instance started successfully") })
    public void fakeHttpSignatureTest(Pet pet, @QueryParam("query_1") String query1, @HeaderParam("header_1")  String header1);

    @POST
    @Path("/outer/boolean")
    @Consumes({ "application/json" })
    @Produces({ "*/*" })
    @ApiOperation(value = "", tags={  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Output boolean", response = Boolean.class) })
    public Boolean fakeOuterBooleanSerialize(Boolean body);

    @POST
    @Path("/outer/composite")
    @Consumes({ "application/json" })
    @Produces({ "*/*" })
    @ApiOperation(value = "", tags={  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Output composite", response = OuterComposite.class) })
    public OuterComposite fakeOuterCompositeSerialize(OuterComposite outerComposite);

    @POST
    @Path("/outer/number")
    @Consumes({ "application/json" })
    @Produces({ "*/*" })
    @ApiOperation(value = "", tags={  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Output number", response = BigDecimal.class) })
    public BigDecimal fakeOuterNumberSerialize(BigDecimal body);

    @POST
    @Path("/outer/string")
    @Consumes({ "application/json" })
    @Produces({ "*/*" })
    @ApiOperation(value = "", tags={  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Output string", response = String.class) })
    public String fakeOuterStringSerialize(String body);

    @PUT
    @Path("/body-with-file-schema")
    @Consumes({ "application/json" })
    @ApiOperation(value = "", tags={  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Success") })
    public void testBodyWithFileSchema(FileSchemaTestClass fileSchemaTestClass);

    @PUT
    @Path("/body-with-query-params")
    @Consumes({ "application/json" })
    @ApiOperation(value = "", tags={  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Success") })
    public void testBodyWithQueryParams(@QueryParam("query") String query, User user);

    /**
     * To test \&quot;client\&quot; model
     *
     * To test \&quot;client\&quot; model
     *
     */
    @PATCH
    
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @ApiOperation(value = "To test \"client\" model", tags={  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Client.class) })
    public Client testClientModel(Client client);

    /**
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     *
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     *
     */
    @POST
    
    @Consumes({ "application/x-www-form-urlencoded" })
    @ApiOperation(value = "Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 ", tags={  })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid username supplied"),
        @ApiResponse(code = 404, message = "User not found") })
    public void testEndpointParameters(@Multipart(value = "number")  BigDecimal number, @Multipart(value = "double")  Double _double, @Multipart(value = "pattern_without_delimiter")  String patternWithoutDelimiter, @Multipart(value = "byte")  byte[] _byte, @Multipart(value = "integer", required = false)  Integer integer, @Multipart(value = "int32", required = false)  Integer int32, @Multipart(value = "int64", required = false)  Long int64, @Multipart(value = "float", required = false)  Float _float, @Multipart(value = "string", required = false)  String string,  @Multipart(value = "binary" , required = false) Attachment binaryDetail, @Multipart(value = "date", required = false)  LocalDate date, @Multipart(value = "dateTime", required = false)  Date dateTime, @Multipart(value = "password", required = false)  String password, @Multipart(value = "callback", required = false)  String paramCallback);

    /**
     * To test enum parameters
     *
     * To test enum parameters
     *
     */
    @GET
    
    @Consumes({ "application/x-www-form-urlencoded" })
    @ApiOperation(value = "To test enum parameters", tags={  })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid request"),
        @ApiResponse(code = 404, message = "Not found") })
    public void testEnumParameters(@HeaderParam("enum_header_string_array")  List<String> enumHeaderStringArray, @HeaderParam("enum_header_string")  String enumHeaderString, @QueryParam("enum_query_string_array") List<String> enumQueryStringArray, @QueryParam("enum_query_string") @DefaultValue("-efg")String enumQueryString, @QueryParam("enum_query_integer") Integer enumQueryInteger, @QueryParam("enum_query_double") Double enumQueryDouble, @Multipart(value = "enum_form_string_array", required = false)  List<String> enumFormStringArray, @Multipart(value = "enum_form_string", required = false)  String enumFormString);

    /**
     * Fake endpoint to test group parameters (optional)
     *
     * Fake endpoint to test group parameters (optional)
     *
     */
    @DELETE
    
    @ApiOperation(value = "Fake endpoint to test group parameters (optional)", tags={  })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Someting wrong") })
    public void testGroupParameters(@QueryParam("required_string_group") Integer requiredStringGroup, @HeaderParam("required_boolean_group")  Boolean requiredBooleanGroup, @QueryParam("required_int64_group") Long requiredInt64Group, @QueryParam("string_group") Integer stringGroup, @HeaderParam("boolean_group")  Boolean booleanGroup, @QueryParam("int64_group") Long int64Group);

    /**
     * test inline additionalProperties
     *
     */
    @POST
    @Path("/inline-additionalProperties")
    @Consumes({ "application/json" })
    @ApiOperation(value = "test inline additionalProperties", tags={  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation") })
    public void testInlineAdditionalProperties(Map<String, String> requestBody);

    /**
     * test json serialization of form data
     *
     */
    @GET
    @Path("/jsonFormData")
    @Consumes({ "application/x-www-form-urlencoded" })
    @ApiOperation(value = "test json serialization of form data", tags={  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation") })
    public void testJsonFormData(@Multipart(value = "param")  String param, @Multipart(value = "param2")  String param2);

    @PUT
    @Path("/test-query-parameters")
    @ApiOperation(value = "", tags={  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Success") })
    public void testQueryParameterCollectionFormat(@QueryParam("pipe") List<String> pipe, @QueryParam("ioutil") List<String> ioutil, @QueryParam("http") List<String> http, @QueryParam("url") List<String> url, @QueryParam("context") List<String> context);
}

