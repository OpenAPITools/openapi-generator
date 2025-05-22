package org.openapitools.api;

import java.math.BigDecimal;
import org.openapitools.model.Client;
import java.util.Date;
import java.io.File;
import org.openapitools.model.FileSchemaTestClass;
import org.joda.time.LocalDate;
import java.util.Map;
import org.openapitools.model.ModelApiResponse;
import org.openapitools.model.OuterComposite;
import org.openapitools.model.User;
import org.openapitools.model.XmlItem;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import io.swagger.annotations.*;

import java.io.InputStream;
import java.util.Map;
import java.util.List;
import javax.validation.constraints.*;
import javax.validation.Valid;

/**
* Represents a collection of functions to interact with the API endpoints.
*/
@Path("/fake")
@Api(description = "the fake API")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.14.0-SNAPSHOT")
public interface FakeApi {

    /**
     * this route creates an XmlItem
     *
     * @param xmlItem XmlItem Body
     * @return successful operation
     */
    @POST
    @Path("/create_xml_item")
    @Consumes({ "application/xml", "application/xml; charset=utf-8", "application/xml; charset=utf-16", "text/xml", "text/xml; charset=utf-8", "text/xml; charset=utf-16" })
    @ApiOperation(value = "creates an XmlItem", notes = "this route creates an XmlItem", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class) })
    void createXmlItem(@Valid @NotNull XmlItem xmlItem);


    /**
     * Test serialization of outer boolean types
     *
     * @param body Input boolean as post body
     * @return Output boolean
     */
    @POST
    @Path("/outer/boolean")
    @Produces({ "*/*" })
    @ApiOperation(value = "", notes = "Test serialization of outer boolean types", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Output boolean", response = Boolean.class) })
    Boolean fakeOuterBooleanSerialize(@Valid Boolean body);


    /**
     * Test serialization of object with outer number type
     *
     * @param body Input composite as post body
     * @return Output composite
     */
    @POST
    @Path("/outer/composite")
    @Produces({ "*/*" })
    @ApiOperation(value = "", notes = "Test serialization of object with outer number type", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Output composite", response = OuterComposite.class) })
    OuterComposite fakeOuterCompositeSerialize(@Valid OuterComposite body);


    /**
     * Test serialization of outer number types
     *
     * @param body Input number as post body
     * @return Output number
     */
    @POST
    @Path("/outer/number")
    @Produces({ "*/*" })
    @ApiOperation(value = "", notes = "Test serialization of outer number types", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Output number", response = BigDecimal.class) })
    BigDecimal fakeOuterNumberSerialize(@Valid BigDecimal body);


    /**
     * Test serialization of outer string types
     *
     * @param body Input string as post body
     * @return Output string
     */
    @POST
    @Path("/outer/string")
    @Produces({ "*/*" })
    @ApiOperation(value = "", notes = "Test serialization of outer string types", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Output string", response = String.class) })
    String fakeOuterStringSerialize(@Valid String body);


    /**
     * For this test, the body for this request much reference a schema named `File`.
     *
     * @param body 
     * @return Success
     */
    @PUT
    @Path("/body-with-file-schema")
    @Consumes({ "application/json" })
    @ApiOperation(value = "", notes = "For this test, the body for this request much reference a schema named `File`.", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Success", response = Void.class) })
    void testBodyWithFileSchema(@Valid @NotNull FileSchemaTestClass body);


    /**
     * 
     *
     * @param query 
     * @param body 
     * @return Success
     */
    @PUT
    @Path("/body-with-query-params")
    @Consumes({ "application/json" })
    @ApiOperation(value = "", notes = "", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Success", response = Void.class) })
    void testBodyWithQueryParams(@QueryParam("query") @NotNull   String query,@Valid @NotNull User body);


    /**
     * To test \"client\" model
     *
     * @param body client model
     * @return successful operation
     */
    @PATCH
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @ApiOperation(value = "To test \"client\" model", notes = "To test \"client\" model", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Client.class) })
    Client testClientModel(@Valid @NotNull Client body);


    /**
     * Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
     *
     * @param number None
     * @param _double None
     * @param patternWithoutDelimiter None
     * @param _byte None
     * @param integer None
     * @param int32 None
     * @param int64 None
     * @param _float None
     * @param string None
     * @param binary None
     * @param date None
     * @param dateTime None
     * @param password None
     * @param paramCallback None
     * @return Invalid username supplied
     * @return User not found
     */
    @POST
    @Consumes({ "application/x-www-form-urlencoded" })
    @ApiOperation(value = "Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트", notes = "Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트", authorizations = {
        
        @Authorization(value = "http_basic_test")
         }, tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid username supplied", response = Void.class),
        @ApiResponse(code = 404, message = "User not found", response = Void.class) })
    void testEndpointParameters(@FormParam(value = "number")  BigDecimal number,@FormParam(value = "double")  Double _double,@FormParam(value = "pattern_without_delimiter")  String patternWithoutDelimiter,@FormParam(value = "byte")  byte[] _byte,@FormParam(value = "integer")  Integer integer,@FormParam(value = "int32")  Integer int32,@FormParam(value = "int64")  Long int64,@FormParam(value = "float")  Float _float,@FormParam(value = "string")  String string, @FormParam(value = "binary") InputStream binaryInputStream,@FormParam(value = "date")  LocalDate date,@FormParam(value = "dateTime")  Date dateTime,@FormParam(value = "password")  String password,@FormParam(value = "callback")  String paramCallback);


    /**
     * To test enum parameters
     *
     * @param enumHeaderStringArray Header parameter enum test (string array)
     * @param enumQueryStringArray Query parameter enum test (string array)
     * @param enumQueryString Query parameter enum test (string)
     * @param enumQueryInteger Query parameter enum test (double)
     * @param enumQueryDouble Query parameter enum test (double)
     * @param enumFormStringArray Form parameter enum test (string array)
     * @param enumFormString Form parameter enum test (string)
     * @return Invalid request
     * @return Not found
     */
    @GET
    @Consumes({ "application/x-www-form-urlencoded" })
    @ApiOperation(value = "To test enum parameters", notes = "To test enum parameters", tags={ "fake" })
    @io.swagger.annotations.ApiImplicitParams({
        @io.swagger.annotations.ApiImplicitParam(name = "enum_header_string", value = "Header parameter enum test (string)",  dataType = "String", paramType = "header")
    })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid request", response = Void.class),
        @ApiResponse(code = 404, message = "Not found", response = Void.class) })
    void testEnumParameters(@HeaderParam("enum_header_string_array")   @ApiParam("Header parameter enum test (string array)") List<String> enumHeaderStringArray,@QueryParam("enum_query_string_array")  @ApiParam("Query parameter enum test (string array)")  List<String> enumQueryStringArray,@QueryParam("enum_query_string") @DefaultValue("-efg")  @ApiParam("Query parameter enum test (string)")  String enumQueryString,@QueryParam("enum_query_integer")  @ApiParam("Query parameter enum test (double)")  Integer enumQueryInteger,@QueryParam("enum_query_double")  @ApiParam("Query parameter enum test (double)")  Double enumQueryDouble,@FormParam(value = "enum_form_string_array")  List<String> enumFormStringArray,@FormParam(value = "enum_form_string")  String enumFormString);


    /**
     * Fake endpoint to test group parameters (optional)
     *
     * @param requiredStringGroup Required String in group parameters
     * @param requiredBooleanGroup Required Boolean in group parameters
     * @param requiredInt64Group Required Integer in group parameters
     * @param stringGroup String in group parameters
     * @param booleanGroup Boolean in group parameters
     * @param int64Group Integer in group parameters
     * @return Something wrong
     */
    @DELETE
    @ApiOperation(value = "Fake endpoint to test group parameters (optional)", notes = "Fake endpoint to test group parameters (optional)", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Something wrong", response = Void.class) })
    void testGroupParameters(@QueryParam("required_string_group") @NotNull  @ApiParam("Required String in group parameters")  Integer requiredStringGroup,@HeaderParam("required_boolean_group") @NotNull   @ApiParam("Required Boolean in group parameters") Boolean requiredBooleanGroup,@QueryParam("required_int64_group") @NotNull  @ApiParam("Required Integer in group parameters")  Long requiredInt64Group,@QueryParam("string_group")  @ApiParam("String in group parameters")  Integer stringGroup,@HeaderParam("boolean_group")   @ApiParam("Boolean in group parameters") Boolean booleanGroup,@QueryParam("int64_group")  @ApiParam("Integer in group parameters")  Long int64Group);


    /**
     * 
     *
     * @param param request body
     * @return successful operation
     */
    @POST
    @Path("/inline-additionalProperties")
    @Consumes({ "application/json" })
    @ApiOperation(value = "test inline additionalProperties", notes = "", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class) })
    void testInlineAdditionalProperties(@Valid @NotNull Map<String, String> param);


    /**
     * 
     *
     * @param param field1
     * @param param2 field2
     * @return successful operation
     */
    @GET
    @Path("/jsonFormData")
    @Consumes({ "application/x-www-form-urlencoded" })
    @ApiOperation(value = "test json serialization of form data", notes = "", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class) })
    void testJsonFormData(@FormParam(value = "param")  String param,@FormParam(value = "param2")  String param2);


    /**
     * To test the collection format in query parameters
     *
     * @param pipe 
     * @param ioutil 
     * @param http 
     * @param url 
     * @param context 
     * @return Success
     */
    @PUT
    @Path("/test-query-parameters")
    @ApiOperation(value = "", notes = "To test the collection format in query parameters", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Success", response = Void.class) })
    void testQueryParameterCollectionFormat(@QueryParam("pipe") @NotNull   List<String> pipe,@QueryParam("ioutil") @NotNull   List<String> ioutil,@QueryParam("http") @NotNull   List<String> http,@QueryParam("url") @NotNull   List<String> url,@QueryParam("context") @NotNull   List<String> context);


    /**
     * 
     *
     * @param petId ID of pet to update
     * @param requiredFile file to upload
     * @param additionalMetadata Additional data to pass to server
     * @return successful operation
     */
    @POST
    @Path("/{petId}/uploadImageWithRequiredFile")
    @Consumes({ "multipart/form-data" })
    @Produces({ "application/json" })
    @ApiOperation(value = "uploads an image (required)", notes = "", authorizations = {
        @Authorization(value = "petstore_auth", scopes = {
            @AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
            @AuthorizationScope(scope = "read:pets", description = "read your pets") })
         }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = ModelApiResponse.class) })
    ModelApiResponse uploadFileWithRequiredFile(@PathParam("petId") @ApiParam("ID of pet to update") Long petId, @FormParam(value = "requiredFile") InputStream requiredFileInputStream,@FormParam(value = "additionalMetadata")  String additionalMetadata);

}
