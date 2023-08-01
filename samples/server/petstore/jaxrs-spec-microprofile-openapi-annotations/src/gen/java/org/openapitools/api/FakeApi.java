package org.openapitools.api;

import java.math.BigDecimal;
import org.openapitools.model.Client;
import java.io.File;
import org.openapitools.model.FileSchemaTestClass;
import java.time.LocalDate;
import java.time.LocalDateTime;
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
    )
})
@Api(description = "the fake API")
@Path("/fake")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen")
public class FakeApi {

    @POST
    @Path("/create_xml_item")
    @Consumes({ "application/xml", "application/xml; charset=utf-8", "application/xml; charset=utf-16", "text/xml", "text/xml; charset=utf-8", "text/xml; charset=utf-16" })
    @ApiOperation(value = "creates an XmlItem", notes = "this route creates an XmlItem", response = Void.class, tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class)
    })
    
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "createXmlItem", summary = "creates an XmlItem", description = "this route creates an XmlItem")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="fake")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "200", description = "successful operation",  content = {
                
            })
        })
    public Response createXmlItem(@Valid @NotNull XmlItem xmlItem) {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/outer/boolean")
    @Produces({ "*/*" })
    @ApiOperation(value = "", notes = "Test serialization of outer boolean types", response = Boolean.class, tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Output boolean", response = Boolean.class)
    })
    
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
    @Produces({ "*/*" })
    @ApiOperation(value = "", notes = "Test serialization of object with outer number type", response = OuterComposite.class, tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Output composite", response = OuterComposite.class)
    })
    
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "fakeOuterCompositeSerialize", summary = "", description = "Test serialization of object with outer number type")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="fake")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "200", description = "Output composite",  content = { 
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="*/*", schema = @org.eclipse.microprofile.openapi.annotations.media.Schema(implementation = OuterComposite.class))
            })
        })
    public Response fakeOuterCompositeSerialize(@Valid OuterComposite body) {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/outer/number")
    @Produces({ "*/*" })
    @ApiOperation(value = "", notes = "Test serialization of outer number types", response = BigDecimal.class, tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Output number", response = BigDecimal.class)
    })
    
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
    @Produces({ "*/*" })
    @ApiOperation(value = "", notes = "Test serialization of outer string types", response = String.class, tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Output string", response = String.class)
    })
    
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

    @PUT
    @Path("/body-with-file-schema")
    @Consumes({ "application/json" })
    @ApiOperation(value = "", notes = "For this test, the body for this request much reference a schema named `File`.", response = Void.class, tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Success", response = Void.class)
    })
    
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "testBodyWithFileSchema", summary = "", description = "For this test, the body for this request much reference a schema named `File`.")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="fake")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "200", description = "Success",  content = {
                
            })
        })
    public Response testBodyWithFileSchema(@Valid @NotNull FileSchemaTestClass body) {
        return Response.ok().entity("magic!").build();
    }

    @PUT
    @Path("/body-with-query-params")
    @Consumes({ "application/json" })
    @ApiOperation(value = "", notes = "", response = Void.class, tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Success", response = Void.class)
    })
    
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "testBodyWithQueryParams", summary = "", description = "")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="fake")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "200", description = "Success",  content = {
                
            })
        })
    public Response testBodyWithQueryParams(@QueryParam("query") @NotNull   String query,@Valid @NotNull User body) {
        return Response.ok().entity("magic!").build();
    }

    @PATCH
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @ApiOperation(value = "To test \"client\" model", notes = "To test \"client\" model", response = Client.class, tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Client.class)
    })
    
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "testClientModel", summary = "To test \"client\" model", description = "To test \"client\" model")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="fake")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "200", description = "successful operation",  content = { 
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="application/json", schema = @org.eclipse.microprofile.openapi.annotations.media.Schema(implementation = Client.class))
            })
        })
    public Response testClientModel(@Valid @NotNull Client body) {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Consumes({ "application/x-www-form-urlencoded" })
    @ApiOperation(value = "Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트", notes = "Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트", response = Void.class, authorizations = {
        
        @Authorization(value = "http_basic_test")
         }, tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid username supplied", response = Void.class),
        @ApiResponse(code = 404, message = "User not found", response = Void.class)
    })
    @org.eclipse.microprofile.openapi.annotations.security.SecurityRequirements(value={
             @org.eclipse.microprofile.openapi.annotations.security.SecurityRequirement(name = "http_basic_test")
    })
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "testEndpointParameters", summary = "Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트", description = "Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트")
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
    @ApiOperation(value = "To test enum parameters", notes = "To test enum parameters", response = Void.class, tags={ "fake" })
    @io.swagger.annotations.ApiImplicitParams({
        @io.swagger.annotations.ApiImplicitParam(name = "enum_header_string", value = "Header parameter enum test (string)",  dataType = "String", paramType = "header")
    })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid request", response = Void.class),
        @ApiResponse(code = 404, message = "Not found", response = Void.class)
    })
    
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "testEnumParameters", summary = "To test enum parameters", description = "To test enum parameters")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="fake")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "400", description = "Invalid request",  content = {
                
            }),
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "404", description = "Not found",  content = {
                
            })
        })
    public Response testEnumParameters(@HeaderParam("enum_header_string_array")   @ApiParam("Header parameter enum test (string array)") @org.eclipse.microprofile.openapi.annotations.parameters.Parameter(description="Header parameter enum test (string array)") List<String> enumHeaderStringArray,@QueryParam("enum_query_string_array")  @ApiParam("Query parameter enum test (string array)") @org.eclipse.microprofile.openapi.annotations.parameters.Parameter(description="Query parameter enum test (string array)")  List<String> enumQueryStringArray,@QueryParam("enum_query_string") @DefaultValue("-efg")  @ApiParam("Query parameter enum test (string)") @org.eclipse.microprofile.openapi.annotations.parameters.Parameter(description="Query parameter enum test (string)")  String enumQueryString,@QueryParam("enum_query_integer")  @ApiParam("Query parameter enum test (double)") @org.eclipse.microprofile.openapi.annotations.parameters.Parameter(description="Query parameter enum test (double)")  Integer enumQueryInteger,@QueryParam("enum_query_double")  @ApiParam("Query parameter enum test (double)") @org.eclipse.microprofile.openapi.annotations.parameters.Parameter(description="Query parameter enum test (double)")  Double enumQueryDouble,@FormParam(value = "enum_form_string_array")  List<String> enumFormStringArray,@FormParam(value = "enum_form_string")  String enumFormString) {
        return Response.ok().entity("magic!").build();
    }

    @DELETE
    @ApiOperation(value = "Fake endpoint to test group parameters (optional)", notes = "Fake endpoint to test group parameters (optional)", response = Void.class, tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Something wrong", response = Void.class)
    })
    
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "testGroupParameters", summary = "Fake endpoint to test group parameters (optional)", description = "Fake endpoint to test group parameters (optional)")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="fake")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "400", description = "Something wrong",  content = {
                
            })
        })
    public Response testGroupParameters(@QueryParam("required_string_group") @NotNull  @ApiParam("Required String in group parameters") @org.eclipse.microprofile.openapi.annotations.parameters.Parameter(description="Required String in group parameters")  Integer requiredStringGroup,@HeaderParam("required_boolean_group") @NotNull   @ApiParam("Required Boolean in group parameters") @org.eclipse.microprofile.openapi.annotations.parameters.Parameter(description="Required Boolean in group parameters") Boolean requiredBooleanGroup,@QueryParam("required_int64_group") @NotNull  @ApiParam("Required Integer in group parameters") @org.eclipse.microprofile.openapi.annotations.parameters.Parameter(description="Required Integer in group parameters")  Long requiredInt64Group,@QueryParam("string_group")  @ApiParam("String in group parameters") @org.eclipse.microprofile.openapi.annotations.parameters.Parameter(description="String in group parameters")  Integer stringGroup,@HeaderParam("boolean_group")   @ApiParam("Boolean in group parameters") @org.eclipse.microprofile.openapi.annotations.parameters.Parameter(description="Boolean in group parameters") Boolean booleanGroup,@QueryParam("int64_group")  @ApiParam("Integer in group parameters") @org.eclipse.microprofile.openapi.annotations.parameters.Parameter(description="Integer in group parameters")  Long int64Group) {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/inline-additionalProperties")
    @Consumes({ "application/json" })
    @ApiOperation(value = "test inline additionalProperties", notes = "", response = Void.class, tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class)
    })
    
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "testInlineAdditionalProperties", summary = "test inline additionalProperties", description = "")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="fake")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "200", description = "successful operation",  content = {
                
            })
        })
    public Response testInlineAdditionalProperties(@Valid @NotNull Map<String, String> param) {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/jsonFormData")
    @Consumes({ "application/x-www-form-urlencoded" })
    @ApiOperation(value = "test json serialization of form data", notes = "", response = Void.class, tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class)
    })
    
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "testJsonFormData", summary = "test json serialization of form data", description = "")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="fake")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "200", description = "successful operation",  content = {
                
            })
        })
    public Response testJsonFormData(@FormParam(value = "param")  String param,@FormParam(value = "param2")  String param2) {
        return Response.ok().entity("magic!").build();
    }

    @PUT
    @Path("/test-query-parameters")
    @ApiOperation(value = "", notes = "To test the collection format in query parameters", response = Void.class, tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Success", response = Void.class)
    })
    
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "testQueryParameterCollectionFormat", summary = "", description = "To test the collection format in query parameters")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="fake")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "200", description = "Success",  content = {
                
            })
        })
    public Response testQueryParameterCollectionFormat(@QueryParam("pipe") @NotNull   List<String> pipe,@QueryParam("ioutil") @NotNull   List<String> ioutil,@QueryParam("http") @NotNull   List<String> http,@QueryParam("url") @NotNull   List<String> url,@QueryParam("context") @NotNull   List<String> context) {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/{petId}/uploadImageWithRequiredFile")
    @Consumes({ "multipart/form-data" })
    @Produces({ "application/json" })
    @ApiOperation(value = "uploads an image (required)", notes = "", response = ModelApiResponse.class, authorizations = {
        @Authorization(value = "petstore_auth", scopes = {
            @AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
            @AuthorizationScope(scope = "read:pets", description = "read your pets") })
         }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = ModelApiResponse.class)
    })
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
    public Response uploadFileWithRequiredFile(@PathParam("petId") @ApiParam("ID of pet to update") @org.eclipse.microprofile.openapi.annotations.parameters.Parameter(description="ID of pet to update") Long petId, @FormParam(value = "requiredFile") InputStream requiredFileInputStream,@FormParam(value = "additionalMetadata")  String additionalMetadata) {
        return Response.ok().entity("magic!").build();
    }
}
