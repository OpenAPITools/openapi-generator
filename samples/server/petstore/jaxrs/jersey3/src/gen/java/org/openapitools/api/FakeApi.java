package org.openapitools.api;

import org.openapitools.api.FakeApiService;
import org.openapitools.api.factories.FakeApiServiceFactory;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;

import java.math.BigDecimal;
import org.openapitools.model.Client;
import java.util.Date;
import org.openapitools.model.EnumClass;
import org.openapitools.model.FakeBigDecimalMap200Response;
import java.io.File;
import org.openapitools.model.FileSchemaTestClass;
import org.openapitools.model.HealthCheckResult;
import java.util.Map;
import org.openapitools.model.ModelApiResponse;
import org.openapitools.model.OuterComposite;
import org.openapitools.model.OuterObjectWithEnumProperty;
import org.openapitools.model.Pet;
import org.openapitools.model.User;

import java.util.Map;
import java.util.List;
import org.openapitools.api.NotFoundException;

import java.io.InputStream;

import org.glassfish.jersey.media.multipart.FormDataParam;
import org.glassfish.jersey.media.multipart.FormDataBodyPart;

import jakarta.servlet.ServletConfig;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.core.SecurityContext;
import jakarta.ws.rs.*;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;

@Path("/fake")


@Tag(description = "the fake API", name = "")
@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJerseyServerCodegen")
public class FakeApi  {
   private final FakeApiService delegate;

   public FakeApi(@Context ServletConfig servletContext) {
      FakeApiService delegate = null;

      if (servletContext != null) {
         String implClass = servletContext.getInitParameter("FakeApi.implementation");
         if (implClass != null && !"".equals(implClass.trim())) {
            try {
               delegate = (FakeApiService) Class.forName(implClass).newInstance();
            } catch (Exception e) {
               throw new RuntimeException(e);
            }
         }
      }

      if (delegate == null) {
         delegate = FakeApiServiceFactory.getFakeApi();
      }

      this.delegate = delegate;
   }

    @jakarta.ws.rs.GET
    @Path("/BigDecimalMap")
    
    @Produces({ "*/*" })
    @Operation(summary = "", description = "", 
        responses = {
            @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = FakeBigDecimalMap200Response.class))),
            }
    , tags={ "fake", }) 
//    ==> Previous Swagger1 annotations <== RESOLVED WITH THE ABOVE FOR SWAGGER 2.X   
//    @io.swagger.annotations.ApiOperation(value = "", notes = "for Java apache and Java native, test toUrlQueryString for maps with BegDecimal keys", response = FakeBigDecimalMap200Response.class, tags={ "fake", })
//    @io.swagger.annotations.ApiResponses(value = {
//        
//        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = FakeBigDecimalMap200Response.class)
//        
//    })
//    ==> Previous Swagger1 annotations <== HOW TO DEAL WITH THIS PART?
//    
    public Response fakeBigDecimalMap(@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.fakeBigDecimalMap(securityContext);
    }
    @jakarta.ws.rs.GET
    @Path("/health")
    
    @Produces({ "application/json" })
    @Operation(summary = "Health check endpoint", description = "", 
        responses = {
            @ApiResponse(responseCode = "200", description = "The instance started successfully", content = @Content(schema = @Schema(implementation = HealthCheckResult.class))),
            }
    , tags={ "fake", }) 
//    ==> Previous Swagger1 annotations <== RESOLVED WITH THE ABOVE FOR SWAGGER 2.X   
//    @io.swagger.annotations.ApiOperation(value = "Health check endpoint", notes = "", response = HealthCheckResult.class, tags={ "fake", })
//    @io.swagger.annotations.ApiResponses(value = {
//        
//        @io.swagger.annotations.ApiResponse(code = 200, message = "The instance started successfully", response = HealthCheckResult.class)
//        
//    })
//    ==> Previous Swagger1 annotations <== HOW TO DEAL WITH THIS PART?
//    
    public Response fakeHealthGet(@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.fakeHealthGet(securityContext);
    }
    @jakarta.ws.rs.GET
    @Path("/http-signature-test")
    @Consumes({ "application/json", "application/xml" })
    
    @Operation(summary = "test http signature authentication", description = "", 
        responses = {
            @ApiResponse(responseCode = "200", description = "The instance started successfully", content = @Content(schema = @Schema(implementation = Void.class))),
            }
    , tags={ "fake", }) 
//    ==> Previous Swagger1 annotations <== RESOLVED WITH THE ABOVE FOR SWAGGER 2.X   
//    @io.swagger.annotations.ApiOperation(value = "test http signature authentication", notes = "", response = Void.class, authorizations = {
//        @io.swagger.annotations.Authorization(value = "http_signature_test")
//    }, tags={ "fake", })
//    @io.swagger.annotations.ApiResponses(value = {
//        
//        @io.swagger.annotations.ApiResponse(code = 200, message = "The instance started successfully", response = Void.class)
//        
//    })
//    ==> Previous Swagger1 annotations <== HOW TO DEAL WITH THIS PART?
//    
    public Response fakeHttpSignatureTest(@Schema(description = "Pet object that needs to be added to the store", required = true) @NotNull @Valid  Pet pet,@Schema(description = "query parameter") @QueryParam("query_1")  String query1,@Schema(description = "header parameter" )@HeaderParam("header_1") String header1,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.fakeHttpSignatureTest(pet, query1, header1, securityContext);
    }
    @jakarta.ws.rs.POST
    @Path("/outer/boolean")
    @Consumes({ "application/json" })
    @Produces({ "*/*" })
    @Operation(summary = "", description = "", 
        responses = {
            @ApiResponse(responseCode = "200", description = "Output boolean", content = @Content(schema = @Schema(implementation = Boolean.class))),
            }
    , tags={ "fake", }) 
//    ==> Previous Swagger1 annotations <== RESOLVED WITH THE ABOVE FOR SWAGGER 2.X   
//    @io.swagger.annotations.ApiOperation(value = "", notes = "Test serialization of outer boolean types", response = Boolean.class, tags={ "fake", })
//    @io.swagger.annotations.ApiResponses(value = {
//        
//        @io.swagger.annotations.ApiResponse(code = 200, message = "Output boolean", response = Boolean.class)
//        
//    })
//    ==> Previous Swagger1 annotations <== HOW TO DEAL WITH THIS PART?
//    
    public Response fakeOuterBooleanSerialize(@Schema(description = "Input boolean as post body")  Boolean body,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.fakeOuterBooleanSerialize(body, securityContext);
    }
    @jakarta.ws.rs.POST
    @Path("/outer/composite")
    @Consumes({ "application/json" })
    @Produces({ "*/*" })
    @Operation(summary = "", description = "", 
        responses = {
            @ApiResponse(responseCode = "200", description = "Output composite", content = @Content(schema = @Schema(implementation = OuterComposite.class))),
            }
    , tags={ "fake", }) 
//    ==> Previous Swagger1 annotations <== RESOLVED WITH THE ABOVE FOR SWAGGER 2.X   
//    @io.swagger.annotations.ApiOperation(value = "", notes = "Test serialization of object with outer number type", response = OuterComposite.class, tags={ "fake", })
//    @io.swagger.annotations.ApiResponses(value = {
//        
//        @io.swagger.annotations.ApiResponse(code = 200, message = "Output composite", response = OuterComposite.class)
//        
//    })
//    ==> Previous Swagger1 annotations <== HOW TO DEAL WITH THIS PART?
//    
    public Response fakeOuterCompositeSerialize(@Schema(description = "Input composite as post body") @Valid  OuterComposite outerComposite,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.fakeOuterCompositeSerialize(outerComposite, securityContext);
    }
    @jakarta.ws.rs.POST
    @Path("/outer/number")
    @Consumes({ "application/json" })
    @Produces({ "*/*" })
    @Operation(summary = "", description = "", 
        responses = {
            @ApiResponse(responseCode = "200", description = "Output number", content = @Content(schema = @Schema(implementation = BigDecimal.class))),
            }
    , tags={ "fake", }) 
//    ==> Previous Swagger1 annotations <== RESOLVED WITH THE ABOVE FOR SWAGGER 2.X   
//    @io.swagger.annotations.ApiOperation(value = "", notes = "Test serialization of outer number types", response = BigDecimal.class, tags={ "fake", })
//    @io.swagger.annotations.ApiResponses(value = {
//        
//        @io.swagger.annotations.ApiResponse(code = 200, message = "Output number", response = BigDecimal.class)
//        
//    })
//    ==> Previous Swagger1 annotations <== HOW TO DEAL WITH THIS PART?
//    
    public Response fakeOuterNumberSerialize(@Schema(description = "Input number as post body")  BigDecimal body,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.fakeOuterNumberSerialize(body, securityContext);
    }
    @jakarta.ws.rs.POST
    @Path("/outer/string")
    @Consumes({ "application/json" })
    @Produces({ "*/*" })
    @Operation(summary = "", description = "", 
        responses = {
            @ApiResponse(responseCode = "200", description = "Output string", content = @Content(schema = @Schema(implementation = String.class))),
            }
    , tags={ "fake", }) 
//    ==> Previous Swagger1 annotations <== RESOLVED WITH THE ABOVE FOR SWAGGER 2.X   
//    @io.swagger.annotations.ApiOperation(value = "", notes = "Test serialization of outer string types", response = String.class, tags={ "fake", })
//    @io.swagger.annotations.ApiResponses(value = {
//        
//        @io.swagger.annotations.ApiResponse(code = 200, message = "Output string", response = String.class)
//        
//    })
//    ==> Previous Swagger1 annotations <== HOW TO DEAL WITH THIS PART?
//    
    public Response fakeOuterStringSerialize(@Schema(description = "Input string as post body")  String body,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.fakeOuterStringSerialize(body, securityContext);
    }
    @jakarta.ws.rs.POST
    @Path("/property/enum-int")
    @Consumes({ "application/json" })
    @Produces({ "*/*" })
    @Operation(summary = "", description = "", 
        responses = {
            @ApiResponse(responseCode = "200", description = "Output enum (int)", content = @Content(schema = @Schema(implementation = OuterObjectWithEnumProperty.class))),
            }
    , tags={ "fake", }) 
//    ==> Previous Swagger1 annotations <== RESOLVED WITH THE ABOVE FOR SWAGGER 2.X   
//    @io.swagger.annotations.ApiOperation(value = "", notes = "Test serialization of enum (int) properties with examples", response = OuterObjectWithEnumProperty.class, tags={ "fake", })
//    @io.swagger.annotations.ApiResponses(value = {
//        
//        @io.swagger.annotations.ApiResponse(code = 200, message = "Output enum (int)", response = OuterObjectWithEnumProperty.class)
//        
//    })
//    ==> Previous Swagger1 annotations <== HOW TO DEAL WITH THIS PART?
//    
    public Response fakePropertyEnumIntegerSerialize(@Schema(description = "Input enum (int) as post body", required = true) @NotNull @Valid  OuterObjectWithEnumProperty outerObjectWithEnumProperty,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.fakePropertyEnumIntegerSerialize(outerObjectWithEnumProperty, securityContext);
    }
    @jakarta.ws.rs.PUT
    @Path("/body-with-binary")
    @Consumes({ "image/png" })
    
    @Operation(summary = "", description = "", 
        responses = {
            @ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = Void.class))),
            }
    , tags={ "fake", }) 
//    ==> Previous Swagger1 annotations <== RESOLVED WITH THE ABOVE FOR SWAGGER 2.X   
//    @io.swagger.annotations.ApiOperation(value = "", notes = "For this test, the body has to be a binary file.", response = Void.class, tags={ "fake", })
//    @io.swagger.annotations.ApiResponses(value = {
//        
//        @io.swagger.annotations.ApiResponse(code = 200, message = "Success", response = Void.class)
//        
//    })
//    ==> Previous Swagger1 annotations <== HOW TO DEAL WITH THIS PART?
//    
    public Response testBodyWithBinary(@Schema(description = "image to upload", required = true) @NotNull  File body,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.testBodyWithBinary(body, securityContext);
    }
    @jakarta.ws.rs.PUT
    @Path("/body-with-file-schema")
    @Consumes({ "application/json" })
    
    @Operation(summary = "", description = "", 
        responses = {
            @ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = Void.class))),
            }
    , tags={ "fake", }) 
//    ==> Previous Swagger1 annotations <== RESOLVED WITH THE ABOVE FOR SWAGGER 2.X   
//    @io.swagger.annotations.ApiOperation(value = "", notes = "For this test, the body for this request must reference a schema named `File`.", response = Void.class, tags={ "fake", })
//    @io.swagger.annotations.ApiResponses(value = {
//        
//        @io.swagger.annotations.ApiResponse(code = 200, message = "Success", response = Void.class)
//        
//    })
//    ==> Previous Swagger1 annotations <== HOW TO DEAL WITH THIS PART?
//    
    public Response testBodyWithFileSchema(@Schema(description = "", required = true) @NotNull @Valid  FileSchemaTestClass fileSchemaTestClass,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.testBodyWithFileSchema(fileSchemaTestClass, securityContext);
    }
    @jakarta.ws.rs.PUT
    @Path("/body-with-query-params")
    @Consumes({ "application/json" })
    
    @Operation(summary = "", description = "", 
        responses = {
            @ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = Void.class))),
            }
    , tags={ "fake", }) 
//    ==> Previous Swagger1 annotations <== RESOLVED WITH THE ABOVE FOR SWAGGER 2.X   
//    @io.swagger.annotations.ApiOperation(value = "", notes = "", response = Void.class, tags={ "fake", })
//    @io.swagger.annotations.ApiResponses(value = {
//        
//        @io.swagger.annotations.ApiResponse(code = 200, message = "Success", response = Void.class)
//        
//    })
//    ==> Previous Swagger1 annotations <== HOW TO DEAL WITH THIS PART?
//    
    public Response testBodyWithQueryParams(@Schema(description = "", required = true) @QueryParam("query") @NotNull  String query,@Schema(description = "", required = true) @NotNull @Valid  User user,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.testBodyWithQueryParams(query, user, securityContext);
    }
    @jakarta.ws.rs.PATCH
    
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @Operation(summary = "To test \"client\" model", description = "", 
        responses = {
            @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = Client.class))),
            }
    , tags={ "fake", }) 
//    ==> Previous Swagger1 annotations <== RESOLVED WITH THE ABOVE FOR SWAGGER 2.X   
//    @io.swagger.annotations.ApiOperation(value = "To test \"client\" model", notes = "To test \"client\" model", response = Client.class, tags={ "fake", })
//    @io.swagger.annotations.ApiResponses(value = {
//        
//        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = Client.class)
//        
//    })
//    ==> Previous Swagger1 annotations <== HOW TO DEAL WITH THIS PART?
//    
    public Response testClientModel(@Schema(description = "client model", required = true) @NotNull @Valid  Client client,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.testClientModel(client, securityContext);
    }
    @jakarta.ws.rs.POST
    
    @Consumes({ "application/x-www-form-urlencoded" })
    
    @Operation(summary = "Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 ", description = "", 
        responses = {
            @ApiResponse(responseCode = "400", description = "Invalid username supplied", content = @Content(schema = @Schema(implementation = Void.class))),
            @ApiResponse(responseCode = "404", description = "User not found", content = @Content(schema = @Schema(implementation = Void.class))),
            }
    , tags={ "fake", }) 
//    ==> Previous Swagger1 annotations <== RESOLVED WITH THE ABOVE FOR SWAGGER 2.X   
//    @io.swagger.annotations.ApiOperation(value = "Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 ", notes = "Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 ", response = Void.class, authorizations = {
//        @io.swagger.annotations.Authorization(value = "http_basic_test")
//    }, tags={ "fake", })
//    @io.swagger.annotations.ApiResponses(value = {
//        
//        @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid username supplied", response = Void.class),
//        
//        @io.swagger.annotations.ApiResponse(code = 404, message = "User not found", response = Void.class)
//        
//    })
//    ==> Previous Swagger1 annotations <== HOW TO DEAL WITH THIS PART?
//    
    public Response testEndpointParameters(@Schema(description = "None", required=true)  @FormParam("number")  BigDecimal number,@Schema(description = "None", required=true)  @FormParam("double")  Double _double,@Schema(description = "None", required=true)  @FormParam("pattern_without_delimiter")  String patternWithoutDelimiter,@Schema(description = "None", required=true)  @FormParam("byte")  byte[] _byte,@Schema(description = "None")  @FormParam("integer")  Integer integer,@Schema(description = "None")  @FormParam("int32")  Integer int32,@Schema(description = "None")  @FormParam("int64")  Long int64,@Schema(description = "None")  @FormParam("float")  Float _float,@Schema(description = "None")  @FormParam("string")  String string,
 @FormDataParam("binary") FormDataBodyPart binaryBodypart ,@Schema(description = "None")  @FormParam("date")  Date date,@Schema(description = "None")  @FormParam("dateTime")  Date dateTime,@Schema(description = "None")  @FormParam("password")  String password,@Schema(description = "None")  @FormParam("callback")  String paramCallback,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.testEndpointParameters(number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, string, binaryBodypart, date, dateTime, password, paramCallback, securityContext);
    }
    @jakarta.ws.rs.GET
    
    @Consumes({ "application/x-www-form-urlencoded" })
    
    @Operation(summary = "To test enum parameters", description = "", 
        responses = {
            @ApiResponse(responseCode = "400", description = "Invalid request", content = @Content(schema = @Schema(implementation = Void.class))),
            @ApiResponse(responseCode = "404", description = "Not found", content = @Content(schema = @Schema(implementation = Void.class))),
            }
    , tags={ "fake", }) 
//    ==> Previous Swagger1 annotations <== RESOLVED WITH THE ABOVE FOR SWAGGER 2.X   
//    @io.swagger.annotations.ApiOperation(value = "To test enum parameters", notes = "To test enum parameters", response = Void.class, tags={ "fake", })
//    @io.swagger.annotations.ApiResponses(value = {
//        
//        @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid request", response = Void.class),
//        
//        @io.swagger.annotations.ApiResponse(code = 404, message = "Not found", response = Void.class)
//        
//    })
//    ==> Previous Swagger1 annotations <== HOW TO DEAL WITH THIS PART?
//    
    public Response testEnumParameters(@Schema(description = "Header parameter enum test (string array)" , allowableValues=">, $")@HeaderParam("enum_header_string_array") List<String> enumHeaderStringArray,@Schema(description = "Header parameter enum test (string)" , allowableValues="_abc, -efg, (xyz)", defaultValue="-efg")@HeaderParam("enum_header_string") String enumHeaderString,@Schema(description = "Query parameter enum test (string array)") @QueryParam("enum_query_string_array") @Valid  List<String> enumQueryStringArray,@Schema(description = "Query parameter enum test (string)", allowableValues="_abc, -efg, (xyz)", defaultValue = "-efg") @DefaultValue("-efg") @QueryParam("enum_query_string")  String enumQueryString,@Schema(description = "Query parameter enum test (double)", allowableValues="1, -2") @QueryParam("enum_query_integer")  Integer enumQueryInteger,@Schema(description = "Query parameter enum test (double)", allowableValues="1.1, -1.2") @QueryParam("enum_query_double")  Double enumQueryDouble,@Schema(description = "") @QueryParam("enum_query_model_array") @Valid  List<EnumClass> enumQueryModelArray,@Schema(description = "Form parameter enum test (string array)", allowableValues=">, $", defaultValue="$")  @DefaultValue("$") @FormParam("enum_form_string_array")  List<String> enumFormStringArray,@Schema(description = "Form parameter enum test (string)", allowableValues="_abc, -efg, (xyz)", defaultValue="-efg")  @DefaultValue("-efg") @FormParam("enum_form_string")  String enumFormString,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.testEnumParameters(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumQueryModelArray, enumFormStringArray, enumFormString, securityContext);
    }
    @jakarta.ws.rs.DELETE
    
    
    
    @Operation(summary = "Fake endpoint to test group parameters (optional)", description = "", 
        responses = {
            @ApiResponse(responseCode = "400", description = "Something wrong", content = @Content(schema = @Schema(implementation = Void.class))),
            }
    , tags={ "fake", }) 
//    ==> Previous Swagger1 annotations <== RESOLVED WITH THE ABOVE FOR SWAGGER 2.X   
//    @io.swagger.annotations.ApiOperation(value = "Fake endpoint to test group parameters (optional)", notes = "Fake endpoint to test group parameters (optional)", response = Void.class, authorizations = {
//        @io.swagger.annotations.Authorization(value = "bearer_test")
//    }, tags={ "fake", })
//    @io.swagger.annotations.ApiResponses(value = {
//        
//        @io.swagger.annotations.ApiResponse(code = 400, message = "Something wrong", response = Void.class)
//        
//    })
//    ==> Previous Swagger1 annotations <== HOW TO DEAL WITH THIS PART?
//    
    public Response testGroupParameters(@Schema(description = "Required String in group parameters", required = true) @QueryParam("required_string_group") @NotNull  Integer requiredStringGroup,@Schema(description = "Required Boolean in group parameters" ,required=true)@HeaderParam("required_boolean_group") Boolean requiredBooleanGroup,@Schema(description = "Required Integer in group parameters", required = true) @QueryParam("required_int64_group") @NotNull  Long requiredInt64Group,@Schema(description = "String in group parameters") @QueryParam("string_group")  Integer stringGroup,@Schema(description = "Boolean in group parameters" )@HeaderParam("boolean_group") Boolean booleanGroup,@Schema(description = "Integer in group parameters") @QueryParam("int64_group")  Long int64Group,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.testGroupParameters(requiredStringGroup, requiredBooleanGroup, requiredInt64Group, stringGroup, booleanGroup, int64Group, securityContext);
    }
    @jakarta.ws.rs.POST
    @Path("/inline-additionalProperties")
    @Consumes({ "application/json" })
    
    @Operation(summary = "test inline additionalProperties", description = "", 
        responses = {
            @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = Void.class))),
            }
    , tags={ "fake", }) 
//    ==> Previous Swagger1 annotations <== RESOLVED WITH THE ABOVE FOR SWAGGER 2.X   
//    @io.swagger.annotations.ApiOperation(value = "test inline additionalProperties", notes = "", response = Void.class, tags={ "fake", })
//    @io.swagger.annotations.ApiResponses(value = {
//        
//        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = Void.class)
//        
//    })
//    ==> Previous Swagger1 annotations <== HOW TO DEAL WITH THIS PART?
//    
    public Response testInlineAdditionalProperties(@Schema(description = "request body", required = true) @NotNull @Valid  Map<String, String> requestBody,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.testInlineAdditionalProperties(requestBody, securityContext);
    }
    @jakarta.ws.rs.GET
    @Path("/jsonFormData")
    @Consumes({ "application/x-www-form-urlencoded" })
    
    @Operation(summary = "test json serialization of form data", description = "", 
        responses = {
            @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = Void.class))),
            }
    , tags={ "fake", }) 
//    ==> Previous Swagger1 annotations <== RESOLVED WITH THE ABOVE FOR SWAGGER 2.X   
//    @io.swagger.annotations.ApiOperation(value = "test json serialization of form data", notes = "", response = Void.class, tags={ "fake", })
//    @io.swagger.annotations.ApiResponses(value = {
//        
//        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = Void.class)
//        
//    })
//    ==> Previous Swagger1 annotations <== HOW TO DEAL WITH THIS PART?
//    
    public Response testJsonFormData(@Schema(description = "field1", required=true)  @FormParam("param")  String param,@Schema(description = "field2", required=true)  @FormParam("param2")  String param2,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.testJsonFormData(param, param2, securityContext);
    }
    @jakarta.ws.rs.PUT
    @Path("/test-query-parameters")
    
    
    @Operation(summary = "", description = "", 
        responses = {
            @ApiResponse(responseCode = "200", description = "Success", content = @Content(schema = @Schema(implementation = Void.class))),
            }
    , tags={ "fake", }) 
//    ==> Previous Swagger1 annotations <== RESOLVED WITH THE ABOVE FOR SWAGGER 2.X   
//    @io.swagger.annotations.ApiOperation(value = "", notes = "To test the collection format in query parameters", response = Void.class, tags={ "fake", })
//    @io.swagger.annotations.ApiResponses(value = {
//        
//        @io.swagger.annotations.ApiResponse(code = 200, message = "Success", response = Void.class)
//        
//    })
//    ==> Previous Swagger1 annotations <== HOW TO DEAL WITH THIS PART?
//    
    public Response testQueryParameterCollectionFormat(@Schema(description = "", required = true) @QueryParam("pipe") @NotNull @Valid  List<String> pipe,@Schema(description = "", required = true) @QueryParam("ioutil") @NotNull @Valid  List<String> ioutil,@Schema(description = "", required = true) @QueryParam("http") @NotNull @Valid  List<String> http,@Schema(description = "", required = true) @QueryParam("url") @NotNull @Valid  List<String> url,@Schema(description = "", required = true) @QueryParam("context") @NotNull @Valid  List<String> context,@Schema(description = "", required = true) @QueryParam("allowEmpty") @NotNull  String allowEmpty,@Schema(description = "") @QueryParam("language") @Valid  Map<String, String> language,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.testQueryParameterCollectionFormat(pipe, ioutil, http, url, context, allowEmpty, language, securityContext);
    }
    @jakarta.ws.rs.POST
    @Path("/{petId}/uploadImageWithRequiredFile")
    @Consumes({ "multipart/form-data" })
    @Produces({ "application/json" })
    @Operation(summary = "uploads an image (required)", description = "", 
        responses = {
            @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = ModelApiResponse.class))),
            }
    , tags={ "pet", }) 
//    ==> Previous Swagger1 annotations <== RESOLVED WITH THE ABOVE FOR SWAGGER 2.X   
//    @io.swagger.annotations.ApiOperation(value = "uploads an image (required)", notes = "", response = ModelApiResponse.class, authorizations = {
//        @io.swagger.annotations.Authorization(value = "petstore_auth", scopes = {
//            @io.swagger.annotations.AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
//            @io.swagger.annotations.AuthorizationScope(scope = "read:pets", description = "read your pets")
//        })
//    }, tags={ "pet", })
//    @io.swagger.annotations.ApiResponses(value = {
//        
//        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = ModelApiResponse.class)
//        
//    })
//    ==> Previous Swagger1 annotations <== HOW TO DEAL WITH THIS PART?
//    
    public Response uploadFileWithRequiredFile(@Schema(description= "ID of pet to update", required = true) @PathParam("petId") @NotNull  Long petId,
 @FormDataParam("requiredFile") FormDataBodyPart requiredFileBodypart ,@Schema(description = "Additional data to pass to server")@FormDataParam("additionalMetadata")  String additionalMetadata,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.uploadFileWithRequiredFile(petId, requiredFileBodypart, additionalMetadata, securityContext);
    }
}
