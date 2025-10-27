package org.openapitools.api;

import java.math.BigDecimal;
import org.openapitools.model.ChildWithNullable;
import org.openapitools.model.Client;
import org.openapitools.model.EnumClass;
import org.openapitools.model.FakeBigDecimalMap200Response;
import java.io.File;
import org.openapitools.model.FileSchemaTestClass;
import org.openapitools.model.HealthCheckResult;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Map;
import org.openapitools.model.ModelApiResponse;
import org.openapitools.model.OuterComposite;
import org.openapitools.model.OuterObjectWithEnumProperty;
import org.openapitools.model.Pet;
import org.openapitools.model.TestInlineFreeformAdditionalPropertiesRequest;
import org.openapitools.model.User;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;





import java.io.InputStream;
import java.util.Map;
import java.util.List;
import javax.validation.constraints.*;
import javax.validation.Valid;

@org.eclipse.microprofile.openapi.annotations.OpenAPIDefinition(
   info = @org.eclipse.microprofile.openapi.annotations.info.Info(
        title = "", version="1.0.0", description="",
        license = @org.eclipse.microprofile.openapi.annotations.info.License(name = "Apache-2.0", url = "https://www.apache.org/licenses/LICENSE-2.0.html")
   ),
   tags = @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="", description="")
)
@org.eclipse.microprofile.openapi.annotations.tags.Tag(name="", description="")
@org.eclipse.microprofile.openapi.annotations.security.SecuritySchemes(value = {
    @org.eclipse.microprofile.openapi.annotations.security.SecurityScheme(
         securitySchemeName = "petstore_auth",
         type = org.eclipse.microprofile.openapi.annotations.enums.SecuritySchemeType.OAUTH2,
         description = "",
         flows = @org.eclipse.microprofile.openapi.annotations.security.OAuthFlows(
            implicit = @org.eclipse.microprofile.openapi.annotations.security.OAuthFlow(authorizationUrl = "http://petstore.swagger.io/api/oauth/dialog",
            tokenUrl = "",
            refreshUrl = "",
            scopes = {
                @org.eclipse.microprofile.openapi.annotations.security.OAuthScope(name = "write:pets", description = "modify pets in your account"),
                @org.eclipse.microprofile.openapi.annotations.security.OAuthScope(name = "read:pets", description = "read your pets")
                 })) 
    ), @org.eclipse.microprofile.openapi.annotations.security.SecurityScheme(
         securitySchemeName = "api_key",
         type = org.eclipse.microprofile.openapi.annotations.enums.SecuritySchemeType.APIKEY,
         description = "",
         apiKeyName = "api_key",
         in = org.eclipse.microprofile.openapi.annotations.enums.SecuritySchemeIn.HEADER
    ), @org.eclipse.microprofile.openapi.annotations.security.SecurityScheme(
         securitySchemeName = "api_key_query",
         type = org.eclipse.microprofile.openapi.annotations.enums.SecuritySchemeType.APIKEY,
         description = "",
         apiKeyName = "api_key_query",
         in = org.eclipse.microprofile.openapi.annotations.enums.SecuritySchemeIn.QUERY
    ), @org.eclipse.microprofile.openapi.annotations.security.SecurityScheme(
         securitySchemeName = "http_basic_test",
         type = org.eclipse.microprofile.openapi.annotations.enums.SecuritySchemeType.HTTP,
         description = "",
         scheme = "basic"
    ), @org.eclipse.microprofile.openapi.annotations.security.SecurityScheme(
        securitySchemeName = "bearer_test",
        type = org.eclipse.microprofile.openapi.annotations.enums.SecuritySchemeType.HTTP,
        description = "",
        scheme = "bearer", bearerFormat = "JWT"
    ), 
})
@Path("/fake")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")
public class FakeApi {

    @GET
    @Path("/BigDecimalMap")
    @Produces({ "*/*" })
    
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "fakeBigDecimalMap", summary = "", description = "for Java apache and Java native, test toUrlQueryString for maps with BegDecimal keys")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="fake")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "200", description = "successful operation",  content = { 
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="*/*", schema = @org.eclipse.microprofile.openapi.annotations.media.Schema(implementation = FakeBigDecimalMap200Response.class))
            })
        })
    public Response fakeBigDecimalMap() {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/health")
    @Produces({ "application/json" })
    
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "fakeHealthGet", summary = "Health check endpoint", description = "")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="fake")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "200", description = "The instance started successfully",  content = { 
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="application/json", schema = @org.eclipse.microprofile.openapi.annotations.media.Schema(implementation = HealthCheckResult.class))
            })
        })
    public Response fakeHealthGet() {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/http-signature-test")
    @Consumes({ "application/json", "application/xml" })
    @org.eclipse.microprofile.openapi.annotations.security.SecurityRequirements(value={
             @org.eclipse.microprofile.openapi.annotations.security.SecurityRequirement(name = "http_signature_test")
    })
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "fakeHttpSignatureTest", summary = "test http signature authentication", description = "")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="fake")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "200", description = "The instance started successfully",  content = {
                
            })
        })
    public Response fakeHttpSignatureTest(@Valid @NotNull Pet pet,@QueryParam("query_1")  @org.eclipse.microprofile.openapi.annotations.parameters.Parameter(description="query parameter")  String query1,@HeaderParam("header_1")   @org.eclipse.microprofile.openapi.annotations.parameters.Parameter(description="header parameter") String header1) {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/outer/boolean")
    @Consumes({ "application/json" })
    @Produces({ "*/*" })
    
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "fakeOuterBooleanSerialize", summary = "", description = "Test serialization of outer boolean types")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="fake")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "200", description = "Output boolean",  content = { 
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="*/*", schema = @org.eclipse.microprofile.openapi.annotations.media.Schema(implementation = Boolean.class))
            })
        })
    public Response fakeOuterBooleanSerialize(@Valid Boolean body) {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/outer/composite")
    @Consumes({ "application/json" })
    @Produces({ "*/*" })
    
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "fakeOuterCompositeSerialize", summary = "", description = "Test serialization of object with outer number type")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="fake")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "200", description = "Output composite",  content = { 
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="*/*", schema = @org.eclipse.microprofile.openapi.annotations.media.Schema(implementation = OuterComposite.class))
            })
        })
    public Response fakeOuterCompositeSerialize(@Valid OuterComposite outerComposite) {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/outer/number")
    @Consumes({ "application/json" })
    @Produces({ "*/*" })
    
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "fakeOuterNumberSerialize", summary = "", description = "Test serialization of outer number types")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="fake")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "200", description = "Output number",  content = { 
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="*/*", schema = @org.eclipse.microprofile.openapi.annotations.media.Schema(implementation = BigDecimal.class))
            })
        })
    public Response fakeOuterNumberSerialize(@Valid BigDecimal body) {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/outer/string")
    @Consumes({ "application/json" })
    @Produces({ "*/*" })
    
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "fakeOuterStringSerialize", summary = "", description = "Test serialization of outer string types")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="fake")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "200", description = "Output string",  content = { 
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="*/*", schema = @org.eclipse.microprofile.openapi.annotations.media.Schema(implementation = String.class))
            })
        })
    public Response fakeOuterStringSerialize(@Valid String body) {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/property/enum-int")
    @Consumes({ "application/json" })
    @Produces({ "*/*" })
    
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "fakePropertyEnumIntegerSerialize", summary = "", description = "Test serialization of enum (int) properties with examples")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="fake")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "200", description = "Output enum (int)",  content = { 
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="*/*", schema = @org.eclipse.microprofile.openapi.annotations.media.Schema(implementation = OuterObjectWithEnumProperty.class))
            })
        })
    public Response fakePropertyEnumIntegerSerialize(@Valid @NotNull OuterObjectWithEnumProperty outerObjectWithEnumProperty) {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/additionalProperties-reference")
    @Consumes({ "application/json" })
    
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "testAdditionalPropertiesReference", summary = "test referenced additionalProperties", description = "")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="fake")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "200", description = "successful operation",  content = {
                
            })
        })
    public Response testAdditionalPropertiesReference(@Valid @NotNull Map<String, Object> requestBody) {
        return Response.ok().entity("magic!").build();
    }

    @PUT
    @Path("/body-with-binary")
    @Consumes({ "image/png" })
    
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "testBodyWithBinary", summary = "", description = "For this test, the body has to be a binary file.")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="fake")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "200", description = "Success",  content = {
                
            })
        })
    public Response testBodyWithBinary(@Valid File body) {
        return Response.ok().entity("magic!").build();
    }

    @PUT
    @Path("/body-with-file-schema")
    @Consumes({ "application/json" })
    
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "testBodyWithFileSchema", summary = "", description = "For this test, the body for this request must reference a schema named `File`.")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="fake")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "200", description = "Success",  content = {
                
            })
        })
    public Response testBodyWithFileSchema(@Valid @NotNull FileSchemaTestClass fileSchemaTestClass) {
        return Response.ok().entity("magic!").build();
    }

    @PUT
    @Path("/body-with-query-params")
    @Consumes({ "application/json" })
    
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "testBodyWithQueryParams", summary = "", description = "")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="fake")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "200", description = "Success",  content = {
                
            })
        })
    public Response testBodyWithQueryParams(@QueryParam("query") @NotNull   String query,@Valid @NotNull User user) {
        return Response.ok().entity("magic!").build();
    }

    @PATCH
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "testClientModel", summary = "To test \"client\" model", description = "To test \"client\" model")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="fake")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "200", description = "successful operation",  content = { 
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="application/json", schema = @org.eclipse.microprofile.openapi.annotations.media.Schema(implementation = Client.class))
            })
        })
    public Response testClientModel(@Valid @NotNull Client client) {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Consumes({ "application/x-www-form-urlencoded" })
    @org.eclipse.microprofile.openapi.annotations.security.SecurityRequirements(value={
             @org.eclipse.microprofile.openapi.annotations.security.SecurityRequirement(name = "http_basic_test")
    })
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "testEndpointParameters", summary = "Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 ", description = "Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 ")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="fake")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "400", description = "Invalid username supplied",  content = {
                
            }),
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "404", description = "User not found",  content = {
                
            })
        })
    public Response testEndpointParameters(@FormParam(value = "number")  BigDecimal number,@FormParam(value = "double")  Double _double,@FormParam(value = "pattern_without_delimiter")  String patternWithoutDelimiter,@FormParam(value = "byte")  byte[] _byte,@FormParam(value = "integer")  Integer integer,@FormParam(value = "int32")  Integer int32,@FormParam(value = "int64")  Long int64,@FormParam(value = "float")  Float _float,@FormParam(value = "string")  String string, @FormParam(value = "binary") InputStream binaryInputStream,@FormParam(value = "date")  LocalDate date,@FormParam(value = "dateTime")  LocalDateTime dateTime,@FormParam(value = "password")  String password,@FormParam(value = "callback")  String paramCallback) {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Consumes({ "application/x-www-form-urlencoded" })
    
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "testEnumParameters", summary = "To test enum parameters", description = "To test enum parameters")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="fake")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "400", description = "Invalid request",  content = {
                
            }),
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "404", description = "Not found",  content = {
                
            })
        })
    public Response testEnumParameters(@HeaderParam("enum_header_string_array")   @org.eclipse.microprofile.openapi.annotations.parameters.Parameter(description="Header parameter enum test (string array)") List<String> enumHeaderStringArray,@QueryParam("enum_query_string_array")  @org.eclipse.microprofile.openapi.annotations.parameters.Parameter(description="Query parameter enum test (string array)")  List<String> enumQueryStringArray,@QueryParam("enum_query_string") @DefaultValue("-efg")  @org.eclipse.microprofile.openapi.annotations.parameters.Parameter(description="Query parameter enum test (string)")  String enumQueryString,@QueryParam("enum_query_integer")  @org.eclipse.microprofile.openapi.annotations.parameters.Parameter(description="Query parameter enum test (double)")  Integer enumQueryInteger,@QueryParam("enum_query_double")  @org.eclipse.microprofile.openapi.annotations.parameters.Parameter(description="Query parameter enum test (double)")  Double enumQueryDouble,@QueryParam("enum_query_model_array")   List<EnumClass> enumQueryModelArray,@FormParam(value = "enum_form_string_array")  List<String> enumFormStringArray,@FormParam(value = "enum_form_string")  String enumFormString) {
        return Response.ok().entity("magic!").build();
    }

    @DELETE
    @org.eclipse.microprofile.openapi.annotations.security.SecurityRequirements(value={
             @org.eclipse.microprofile.openapi.annotations.security.SecurityRequirement(name = "bearer_test")
    })
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "testGroupParameters", summary = "Fake endpoint to test group parameters (optional)", description = "Fake endpoint to test group parameters (optional)")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="fake")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "400", description = "Something wrong",  content = {
                
            })
        })
    public Response testGroupParameters(@QueryParam("required_string_group") @NotNull  @org.eclipse.microprofile.openapi.annotations.parameters.Parameter(description="Required String in group parameters")  Integer requiredStringGroup,@HeaderParam("required_boolean_group") @NotNull   @org.eclipse.microprofile.openapi.annotations.parameters.Parameter(description="Required Boolean in group parameters") Boolean requiredBooleanGroup,@QueryParam("required_int64_group") @NotNull  @org.eclipse.microprofile.openapi.annotations.parameters.Parameter(description="Required Integer in group parameters")  Long requiredInt64Group,@QueryParam("string_group")  @org.eclipse.microprofile.openapi.annotations.parameters.Parameter(description="String in group parameters")  Integer stringGroup,@HeaderParam("boolean_group")   @org.eclipse.microprofile.openapi.annotations.parameters.Parameter(description="Boolean in group parameters") Boolean booleanGroup,@QueryParam("int64_group")  @org.eclipse.microprofile.openapi.annotations.parameters.Parameter(description="Integer in group parameters")  Long int64Group) {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/inline-additionalProperties")
    @Consumes({ "application/json" })
    
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "testInlineAdditionalProperties", summary = "test inline additionalProperties", description = "")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="fake")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "200", description = "successful operation",  content = {
                
            })
        })
    public Response testInlineAdditionalProperties(@Valid @NotNull Map<String, String> requestBody) {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/inline-freeform-additionalProperties")
    @Consumes({ "application/json" })
    
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "testInlineFreeformAdditionalProperties", summary = "test inline free-form additionalProperties", description = "")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="fake")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "200", description = "successful operation",  content = {
                
            })
        })
    public Response testInlineFreeformAdditionalProperties(@Valid @NotNull TestInlineFreeformAdditionalPropertiesRequest testInlineFreeformAdditionalPropertiesRequest) {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/jsonFormData")
    @Consumes({ "application/x-www-form-urlencoded" })
    
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "testJsonFormData", summary = "test json serialization of form data", description = "")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="fake")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "200", description = "successful operation",  content = {
                
            })
        })
    public Response testJsonFormData(@FormParam(value = "param")  String param,@FormParam(value = "param2")  String param2) {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/nullable")
    @Consumes({ "application/json" })
    
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "testNullable", summary = "test nullable parent property", description = "")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="fake")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "200", description = "successful operation",  content = {
                
            })
        })
    public Response testNullable(@Valid @NotNull ChildWithNullable childWithNullable) {
        return Response.ok().entity("magic!").build();
    }

    @PUT
    @Path("/test-query-parameters")
    
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "testQueryParameterCollectionFormat", summary = "", description = "To test the collection format in query parameters")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="fake")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "200", description = "Success",  content = {
                
            })
        })
    public Response testQueryParameterCollectionFormat(@QueryParam("pipe") @NotNull   List<String> pipe,@QueryParam("ioutil") @NotNull   List<String> ioutil,@QueryParam("http") @NotNull   List<String> http,@QueryParam("url") @NotNull   List<String> url,@QueryParam("context") @NotNull   List<String> context,@QueryParam("allowEmpty") @NotNull   String allowEmpty,@QueryParam("language")   Map<String, String> language) {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/stringMap-reference")
    @Consumes({ "application/json" })
    
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "testStringMapReference", summary = "test referenced string map", description = "")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="fake")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "200", description = "successful operation",  content = {
                
            })
        })
    public Response testStringMapReference(@Valid @NotNull Map<String, String> requestBody) {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/{petId}/uploadImageWithRequiredFile")
    @Consumes({ "multipart/form-data" })
    @Produces({ "application/json" })
    @org.eclipse.microprofile.openapi.annotations.security.SecurityRequirements(value={
        @org.eclipse.microprofile.openapi.annotations.security.SecurityRequirement(name = "petstore_auth", scopes = {  "write:pets",  "read:pets"  })
    })
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "uploadFileWithRequiredFile", summary = "uploads an image (required)", description = "")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="pet")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "200", description = "successful operation",  content = { 
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="application/json", schema = @org.eclipse.microprofile.openapi.annotations.media.Schema(implementation = ModelApiResponse.class))
            })
        })
    public Response uploadFileWithRequiredFile(@PathParam("petId") @org.eclipse.microprofile.openapi.annotations.parameters.Parameter(description="ID of pet to update") Long petId, @FormParam(value = "requiredFile") InputStream requiredFileInputStream,@FormParam(value = "additionalMetadata")  String additionalMetadata) {
        return Response.ok().entity("magic!").build();
    }
}
