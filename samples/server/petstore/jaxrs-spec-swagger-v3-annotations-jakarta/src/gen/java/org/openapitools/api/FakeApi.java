package org.openapitools.api;

import java.math.BigDecimal;
import org.openapitools.model.ChildWithNullable;
import org.openapitools.model.Client;
import java.util.Date;
import org.openapitools.model.EnumClass;
import org.openapitools.model.FakeBigDecimalMap200Response;
import org.openapitools.model.FakeTestsDefaultsDefaultResponse;
import java.io.File;
import org.openapitools.model.FileSchemaTestClass;
import org.openapitools.model.HealthCheckResult;
import org.joda.time.LocalDate;
import java.util.Map;
import org.openapitools.model.ModelApiResponse;
import org.openapitools.model.OuterComposite;
import org.openapitools.model.OuterObjectWithEnumProperty;
import org.openapitools.model.Pet;
import org.openapitools.model.TestInlineFreeformAdditionalPropertiesRequest;
import org.openapitools.model.User;

import jakarta.ws.rs.*;
import jakarta.ws.rs.core.Response;

import io.swagger.v3.oas.annotations.*;
import io.swagger.v3.oas.annotations.media.*;
import io.swagger.v3.oas.annotations.responses.*;
import io.swagger.v3.oas.annotations.tags.Tag;

import java.io.InputStream;
import java.util.Map;
import java.util.List;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;

/**
* Represents a collection of functions to interact with the API endpoints.
*/
@Path("/fake")
@Tag(name = "fake")
@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.19.0-SNAPSHOT")
public class FakeApi {

    @GET
    @Path("/BigDecimalMap")
    @Produces({ "*/*" })
    @Operation(summary = "", description = "for Java apache and Java native, test toUrlQueryString for maps with BegDecimal keys")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation")
    })
    public Response fakeBigDecimalMap() {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/health")
    @Produces({ "application/json" })
    @Operation(summary = "Health check endpoint", description = "")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "The instance started successfully")
    })
    public Response fakeHealthGet() {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/http-signature-test")
    @Consumes({ "application/json", "application/xml" })
    @Operation(summary = "test http signature authentication", description = "")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "The instance started successfully")
    })
    public Response fakeHttpSignatureTest(@Valid @NotNull Pet pet,@QueryParam("query_1")   String query1,@HeaderParam("header_1")   String header1) {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/outer/boolean")
    @Consumes({ "application/json" })
    @Produces({ "*/*" })
    @Operation(summary = "", description = "Test serialization of outer boolean types")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "Output boolean")
    })
    public Response fakeOuterBooleanSerialize(@Valid Boolean body) {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/outer/composite")
    @Consumes({ "application/json" })
    @Produces({ "*/*" })
    @Operation(summary = "", description = "Test serialization of object with outer number type")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "Output composite")
    })
    public Response fakeOuterCompositeSerialize(@Valid OuterComposite outerComposite) {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/outer/number")
    @Consumes({ "application/json" })
    @Produces({ "*/*" })
    @Operation(summary = "", description = "Test serialization of outer number types")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "Output number")
    })
    public Response fakeOuterNumberSerialize(@Valid BigDecimal body) {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/outer/string")
    @Consumes({ "application/json" })
    @Produces({ "*/*" })
    @Operation(summary = "", description = "Test serialization of outer string types")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "Output string")
    })
    public Response fakeOuterStringSerialize(@Valid String body) {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/property/enum-int")
    @Consumes({ "application/json" })
    @Produces({ "*/*" })
    @Operation(summary = "", description = "Test serialization of enum (int) properties with examples")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "Output enum (int)")
    })
    public Response fakePropertyEnumIntegerSerialize(@Valid @NotNull OuterObjectWithEnumProperty outerObjectWithEnumProperty) {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/tests/defaults")
    @Produces({ "application/json" })
    @Operation(summary = "test enum default in request body", description = "")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "response")
    })
    public Response fakeTestsDefaults() {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/additionalProperties-reference")
    @Consumes({ "application/json" })
    @Operation(summary = "test referenced additionalProperties", description = "")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation")
    })
    public Response testAdditionalPropertiesReference(@Valid @NotNull Map<String, Object> requestBody) {
        return Response.ok().entity("magic!").build();
    }

    @PUT
    @Path("/body-with-binary")
    @Consumes({ "image/png" })
    @Operation(summary = "", description = "For this test, the body has to be a binary file.")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "Success")
    })
    public Response testBodyWithBinary(@Valid File body) {
        return Response.ok().entity("magic!").build();
    }

    @PUT
    @Path("/body-with-file-schema")
    @Consumes({ "application/json" })
    @Operation(summary = "", description = "For this test, the body for this request must reference a schema named `File`.")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "Success")
    })
    public Response testBodyWithFileSchema(@Valid @NotNull FileSchemaTestClass fileSchemaTestClass) {
        return Response.ok().entity("magic!").build();
    }

    @PUT
    @Path("/body-with-query-params")
    @Consumes({ "application/json" })
    @Operation(summary = "", description = "")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "Success")
    })
    public Response testBodyWithQueryParams(@QueryParam("query") @NotNull   String query,@Valid @NotNull User user) {
        return Response.ok().entity("magic!").build();
    }

    @PATCH
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @Operation(summary = "To test \"client\" model", description = "To test \"client\" model")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation")
    })
    public Response testClientModel(@Valid @NotNull Client client) {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Consumes({ "application/x-www-form-urlencoded" })
    @Operation(summary = "Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 ", description = "Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 ")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid username supplied"),
        @ApiResponse(responseCode = "404", description = "User not found")
    })
    public Response testEndpointParameters(@FormParam(value = "number")  BigDecimal number,@FormParam(value = "double")  Double _double,@FormParam(value = "pattern_without_delimiter")  String patternWithoutDelimiter,@FormParam(value = "byte")  byte[] _byte,@FormParam(value = "integer")  Integer integer,@FormParam(value = "int32")  Integer int32,@FormParam(value = "int64")  Long int64,@FormParam(value = "float")  Float _float,@FormParam(value = "string")  String string, @FormParam(value = "binary") InputStream binaryInputStream,@FormParam(value = "date")  LocalDate date,@FormParam(value = "dateTime")  Date dateTime,@FormParam(value = "password")  String password,@FormParam(value = "callback")  String paramCallback) {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Consumes({ "application/x-www-form-urlencoded" })
    @Operation(summary = "To test enum parameters", description = "To test enum parameters")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid request"),
        @ApiResponse(responseCode = "404", description = "Not found")
    })
    public Response testEnumParameters(@HeaderParam("enum_header_string_array")   List<String> enumHeaderStringArray,@QueryParam("enum_query_string_array")   List<String> enumQueryStringArray,@QueryParam("enum_query_string") @DefaultValue("-efg")   String enumQueryString,@QueryParam("enum_query_integer")   Integer enumQueryInteger,@QueryParam("enum_query_double")   Double enumQueryDouble,@QueryParam("enum_query_model_array")   List<EnumClass> enumQueryModelArray,@FormParam(value = "enum_form_string_array")  List<String> enumFormStringArray,@FormParam(value = "enum_form_string")  String enumFormString) {
        return Response.ok().entity("magic!").build();
    }

    @DELETE
    @Operation(summary = "Fake endpoint to test group parameters (optional)", description = "Fake endpoint to test group parameters (optional)")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Something wrong")
    })
    public Response testGroupParameters(@QueryParam("required_string_group") @NotNull   Integer requiredStringGroup,@HeaderParam("required_boolean_group") @NotNull   Boolean requiredBooleanGroup,@QueryParam("required_int64_group") @NotNull   Long requiredInt64Group,@QueryParam("string_group")   Integer stringGroup,@HeaderParam("boolean_group")   Boolean booleanGroup,@QueryParam("int64_group")   Long int64Group) {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/inline-additionalProperties")
    @Consumes({ "application/json" })
    @Operation(summary = "test inline additionalProperties", description = "")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation")
    })
    public Response testInlineAdditionalProperties(@Valid @NotNull Map<String, String> requestBody) {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/inline-freeform-additionalProperties")
    @Consumes({ "application/json" })
    @Operation(summary = "test inline free-form additionalProperties", description = "")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation")
    })
    public Response testInlineFreeformAdditionalProperties(@Valid @NotNull TestInlineFreeformAdditionalPropertiesRequest testInlineFreeformAdditionalPropertiesRequest) {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/jsonFormData")
    @Consumes({ "application/x-www-form-urlencoded" })
    @Operation(summary = "test json serialization of form data", description = "")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation")
    })
    public Response testJsonFormData(@FormParam(value = "param")  String param,@FormParam(value = "param2")  String param2) {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/nullable")
    @Consumes({ "application/json" })
    @Operation(summary = "test nullable parent property", description = "")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation")
    })
    public Response testNullable(@Valid @NotNull ChildWithNullable childWithNullable) {
        return Response.ok().entity("magic!").build();
    }

    @PUT
    @Path("/test-query-parameters")
    @Operation(summary = "", description = "To test the collection format in query parameters")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "Success")
    })
    public Response testQueryParameterCollectionFormat(@QueryParam("pipe") @NotNull   List<String> pipe,@QueryParam("ioutil") @NotNull   List<String> ioutil,@QueryParam("http") @NotNull   List<String> http,@QueryParam("url") @NotNull   List<String> url,@QueryParam("context") @NotNull   List<String> context,@QueryParam("allowEmpty") @NotNull   String allowEmpty,@QueryParam("language")   Map<String, String> language) {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/stringMap-reference")
    @Consumes({ "application/json" })
    @Operation(summary = "test referenced string map", description = "")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation")
    })
    public Response testStringMapReference(@Valid @NotNull Map<String, String> requestBody) {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/{petId}/uploadImageWithRequiredFile")
    @Consumes({ "multipart/form-data" })
    @Produces({ "application/json" })
    @Operation(summary = "uploads an image (required)", description = "")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation")
    })
    public Response uploadFileWithRequiredFile(@PathParam("petId") Long petId, @FormParam(value = "requiredFile") InputStream requiredFileInputStream,@FormParam(value = "additionalMetadata")  String additionalMetadata) {
        return Response.ok().entity("magic!").build();
    }
}
