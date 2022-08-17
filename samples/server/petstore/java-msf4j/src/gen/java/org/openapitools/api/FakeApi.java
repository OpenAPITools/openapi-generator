package org.openapitools.api;

import org.openapitools.model.*;
import org.openapitools.api.FakeApiService;
import org.openapitools.api.factories.FakeApiServiceFactory;

import io.swagger.annotations.ApiParam;
import io.swagger.jaxrs.*;

import java.math.BigDecimal;
import org.openapitools.model.Client;
import java.util.Date;
import java.io.File;
import org.openapitools.model.FileSchemaTestClass;
import java.util.Map;
import org.openapitools.model.ModelApiResponse;
import org.openapitools.model.OuterComposite;
import org.openapitools.model.User;
import org.openapitools.model.XmlItem;

import java.util.List;
import org.openapitools.api.NotFoundException;

import java.io.InputStream;

import org.wso2.msf4j.formparam.FormDataParam;
import org.wso2.msf4j.formparam.FileInfo;

import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.*;

@Path("/fake")


@io.swagger.annotations.Api(description = "the fake API")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaMSF4JServerCodegen")
public class FakeApi  {
   private final FakeApiService delegate = FakeApiServiceFactory.getFakeApi();

    @POST
    @Path("/create_xml_item")
    @Consumes({ "application/xml", "application/xml; charset=utf-8", "application/xml; charset=utf-16", "text/xml", "text/xml; charset=utf-8", "text/xml; charset=utf-16" })
    
    @io.swagger.annotations.ApiOperation(value = "creates an XmlItem", notes = "this route creates an XmlItem", response = Void.class, tags={ "fake", })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = Void.class) })
    public Response createXmlItem(@ApiParam(value = "XmlItem Body" ,required=true) XmlItem xmlItem
)
    throws NotFoundException {
        return delegate.createXmlItem(xmlItem);
    }
    @POST
    @Path("/outer/boolean")
    
    @Produces({ "*/*" })
    @io.swagger.annotations.ApiOperation(value = "", notes = "Test serialization of outer boolean types", response = Boolean.class, tags={ "fake", })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "Output boolean", response = Boolean.class) })
    public Response fakeOuterBooleanSerialize(@ApiParam(value = "Input boolean as post body" ) Boolean body
)
    throws NotFoundException {
        return delegate.fakeOuterBooleanSerialize(body);
    }
    @POST
    @Path("/outer/composite")
    
    @Produces({ "*/*" })
    @io.swagger.annotations.ApiOperation(value = "", notes = "Test serialization of object with outer number type", response = OuterComposite.class, tags={ "fake", })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "Output composite", response = OuterComposite.class) })
    public Response fakeOuterCompositeSerialize(@ApiParam(value = "Input composite as post body" ) OuterComposite body
)
    throws NotFoundException {
        return delegate.fakeOuterCompositeSerialize(body);
    }
    @POST
    @Path("/outer/number")
    
    @Produces({ "*/*" })
    @io.swagger.annotations.ApiOperation(value = "", notes = "Test serialization of outer number types", response = BigDecimal.class, tags={ "fake", })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "Output number", response = BigDecimal.class) })
    public Response fakeOuterNumberSerialize(@ApiParam(value = "Input number as post body" ) BigDecimal body
)
    throws NotFoundException {
        return delegate.fakeOuterNumberSerialize(body);
    }
    @POST
    @Path("/outer/string")
    
    @Produces({ "*/*" })
    @io.swagger.annotations.ApiOperation(value = "", notes = "Test serialization of outer string types", response = String.class, tags={ "fake", })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "Output string", response = String.class) })
    public Response fakeOuterStringSerialize(@ApiParam(value = "Input string as post body" ) String body
)
    throws NotFoundException {
        return delegate.fakeOuterStringSerialize(body);
    }
    @PUT
    @Path("/body-with-file-schema")
    @Consumes({ "application/json" })
    
    @io.swagger.annotations.ApiOperation(value = "", notes = "For this test, the body for this request much reference a schema named `File`.", response = Void.class, tags={ "fake", })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "Success", response = Void.class) })
    public Response testBodyWithFileSchema(@ApiParam(value = "" ,required=true) FileSchemaTestClass body
)
    throws NotFoundException {
        return delegate.testBodyWithFileSchema(body);
    }
    @PUT
    @Path("/body-with-query-params")
    @Consumes({ "application/json" })
    
    @io.swagger.annotations.ApiOperation(value = "", notes = "", response = Void.class, tags={ "fake", })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "Success", response = Void.class) })
    public Response testBodyWithQueryParams(@ApiParam(value = "",required=true) @QueryParam("query") String query
,@ApiParam(value = "" ,required=true) User body
)
    throws NotFoundException {
        return delegate.testBodyWithQueryParams(query,body);
    }
    @PATCH
    
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "To test \"client\" model", notes = "To test \"client\" model", response = Client.class, tags={ "fake", })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = Client.class) })
    public Response testClientModel(@ApiParam(value = "client model" ,required=true) Client body
)
    throws NotFoundException {
        return delegate.testClientModel(body);
    }
    @POST
    
    @Consumes({ "application/x-www-form-urlencoded" })
    
    @io.swagger.annotations.ApiOperation(value = "Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트", notes = "Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트", response = Void.class, authorizations = {
        @io.swagger.annotations.Authorization(value = "http_basic_test")
    }, tags={ "fake", })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid username supplied", response = Void.class),
        
        @io.swagger.annotations.ApiResponse(code = 404, message = "User not found", response = Void.class) })
    public Response testEndpointParameters(@ApiParam(value = "None", required=true)  @FormParam("number")  BigDecimal number
,@ApiParam(value = "None", required=true)  @FormParam("double")  Double _double
,@ApiParam(value = "None", required=true)  @FormParam("pattern_without_delimiter")  String patternWithoutDelimiter
,@ApiParam(value = "None", required=true)  @FormParam("byte")  byte[] _byte
,@ApiParam(value = "None")  @FormParam("integer")  Integer integer
,@ApiParam(value = "None")  @FormParam("int32")  Integer int32
,@ApiParam(value = "None")  @FormParam("int64")  Long int64
,@ApiParam(value = "None")  @FormParam("float")  Float _float
,@ApiParam(value = "None")  @FormParam("string")  String string
,
            @FormDataParam("binary") InputStream binaryInputStream,
            @FormDataParam("binary") FileInfo binaryDetail
,@ApiParam(value = "None")  @FormParam("date")  Date date
,@ApiParam(value = "None")  @FormParam("dateTime")  Date dateTime
,@ApiParam(value = "None")  @FormParam("password")  String password
,@ApiParam(value = "None")  @FormParam("callback")  String paramCallback
)
    throws NotFoundException {
        return delegate.testEndpointParameters(number,_double,patternWithoutDelimiter,_byte,integer,int32,int64,_float,string,binaryInputStream, binaryDetail,date,dateTime,password,paramCallback);
    }
    @GET
    
    @Consumes({ "application/x-www-form-urlencoded" })
    
    @io.swagger.annotations.ApiOperation(value = "To test enum parameters", notes = "To test enum parameters", response = Void.class, tags={ "fake", })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid request", response = Void.class),
        
        @io.swagger.annotations.ApiResponse(code = 404, message = "Not found", response = Void.class) })
    public Response testEnumParameters(@ApiParam(value = "Header parameter enum test (string array)" , allowableValues=">, $")@HeaderParam("enum_header_string_array") List<String> enumHeaderStringArray
,@ApiParam(value = "Header parameter enum test (string)" , allowableValues="_abc, -efg, (xyz)", defaultValue="-efg")@HeaderParam("enum_header_string") String enumHeaderString
,@ApiParam(value = "Query parameter enum test (string array)", allowableValues=">, $") @QueryParam("enum_query_string_array") List<String> enumQueryStringArray
,@ApiParam(value = "Query parameter enum test (string)", allowableValues="_abc, -efg, (xyz)", defaultValue="-efg") @DefaultValue("-efg") @QueryParam("enum_query_string") String enumQueryString
,@ApiParam(value = "Query parameter enum test (double)", allowableValues="1, -2") @QueryParam("enum_query_integer") Integer enumQueryInteger
,@ApiParam(value = "Query parameter enum test (double)", allowableValues="1.1, -1.2") @QueryParam("enum_query_double") Double enumQueryDouble
,@ApiParam(value = "Form parameter enum test (string array)", allowableValues=">, $", defaultValue="$")  @DefaultValue("$") @FormParam("enum_form_string_array")  List<String> enumFormStringArray
,@ApiParam(value = "Form parameter enum test (string)", allowableValues="_abc, -efg, (xyz)", defaultValue="-efg")  @DefaultValue("-efg") @FormParam("enum_form_string")  String enumFormString
)
    throws NotFoundException {
        return delegate.testEnumParameters(enumHeaderStringArray,enumHeaderString,enumQueryStringArray,enumQueryString,enumQueryInteger,enumQueryDouble,enumFormStringArray,enumFormString);
    }
    @DELETE
    
    
    
    @io.swagger.annotations.ApiOperation(value = "Fake endpoint to test group parameters (optional)", notes = "Fake endpoint to test group parameters (optional)", response = Void.class, tags={ "fake", })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 400, message = "Someting wrong", response = Void.class) })
    public Response testGroupParameters(@ApiParam(value = "Required String in group parameters",required=true) @QueryParam("required_string_group") Integer requiredStringGroup
,@ApiParam(value = "Required Boolean in group parameters" ,required=true)@HeaderParam("required_boolean_group") Boolean requiredBooleanGroup
,@ApiParam(value = "Required Integer in group parameters",required=true) @QueryParam("required_int64_group") Long requiredInt64Group
,@ApiParam(value = "String in group parameters") @QueryParam("string_group") Integer stringGroup
,@ApiParam(value = "Boolean in group parameters" )@HeaderParam("boolean_group") Boolean booleanGroup
,@ApiParam(value = "Integer in group parameters") @QueryParam("int64_group") Long int64Group
)
    throws NotFoundException {
        return delegate.testGroupParameters(requiredStringGroup,requiredBooleanGroup,requiredInt64Group,stringGroup,booleanGroup,int64Group);
    }
    @POST
    @Path("/inline-additionalProperties")
    @Consumes({ "application/json" })
    
    @io.swagger.annotations.ApiOperation(value = "test inline additionalProperties", notes = "", response = Void.class, tags={ "fake", })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = Void.class) })
    public Response testInlineAdditionalProperties(@ApiParam(value = "request body" ,required=true) Map<String, String> param
)
    throws NotFoundException {
        return delegate.testInlineAdditionalProperties(param);
    }
    @GET
    @Path("/jsonFormData")
    @Consumes({ "application/x-www-form-urlencoded" })
    
    @io.swagger.annotations.ApiOperation(value = "test json serialization of form data", notes = "", response = Void.class, tags={ "fake", })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = Void.class) })
    public Response testJsonFormData(@ApiParam(value = "field1", required=true)  @FormParam("param")  String param
,@ApiParam(value = "field2", required=true)  @FormParam("param2")  String param2
)
    throws NotFoundException {
        return delegate.testJsonFormData(param,param2);
    }
    @PUT
    @Path("/test-query-parameters")
    
    
    @io.swagger.annotations.ApiOperation(value = "", notes = "To test the collection format in query parameters", response = Void.class, tags={ "fake", })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "Success", response = Void.class) })
    public Response testQueryParameterCollectionFormat(@ApiParam(value = "",required=true) @QueryParam("pipe") List<String> pipe
,@ApiParam(value = "",required=true) @QueryParam("ioutil") List<String> ioutil
,@ApiParam(value = "",required=true) @QueryParam("http") List<String> http
,@ApiParam(value = "",required=true) @QueryParam("url") List<String> url
,@ApiParam(value = "",required=true) @QueryParam("context") List<String> context
)
    throws NotFoundException {
        return delegate.testQueryParameterCollectionFormat(pipe,ioutil,http,url,context);
    }
    @POST
    @Path("/{petId}/uploadImageWithRequiredFile")
    @Consumes({ "multipart/form-data" })
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "uploads an image (required)", notes = "", response = ModelApiResponse.class, authorizations = {
        @io.swagger.annotations.Authorization(value = "petstore_auth", scopes = {
            @io.swagger.annotations.AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
            @io.swagger.annotations.AuthorizationScope(scope = "read:pets", description = "read your pets")
        })
    }, tags={ "pet", })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = ModelApiResponse.class) })
    public Response uploadFileWithRequiredFile(@ApiParam(value = "ID of pet to update",required=true) @PathParam("petId") Long petId
,
            @FormDataParam("requiredFile") InputStream requiredFileInputStream,
            @FormDataParam("requiredFile") FileInfo requiredFileDetail
,@ApiParam(value = "Additional data to pass to server")@FormDataParam("additionalMetadata")  String additionalMetadata
)
    throws NotFoundException {
        return delegate.uploadFileWithRequiredFile(petId,requiredFileInputStream, requiredFileDetail,additionalMetadata);
    }
}
