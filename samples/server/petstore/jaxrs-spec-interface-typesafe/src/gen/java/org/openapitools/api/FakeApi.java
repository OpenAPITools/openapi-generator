package org.openapitools.api;

import java.math.BigDecimal;
import org.openapitools.model.Client;
import java.util.Date;
import java.io.File;
import org.openapitools.model.FileSchemaTestClass;
import org.joda.time.LocalDate;
import java.util.Map;
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

@Path("/fake")
@Api(description = "the Fake API")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen")public interface FakeApi {

    @POST
    @Path("/create_xml_item")
    @Consumes({ "application/xml", "application/xml; charset=utf-8", "application/xml; charset=utf-16", "text/xml", "text/xml; charset=utf-8", "text/xml; charset=utf-16" })
    @ApiOperation(value = "creates an XmlItem", notes = "this route creates an XmlItem", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class) })
    CreateXmlItemResponse createXmlItem(@Valid @NotNull XmlItem xmlItem);

    public static class CreateXmlItemResponse extends org.openapitools.api.support.ResponseWrapper {
        private CreateXmlItemResponse(Response delegate) {
            super(delegate);
        }
        public static CreateXmlItemResponse with200() {
            return new CreateXmlItemResponse(Response.status(200).build());
        }
        public static CreateXmlItemResponse withCustomResponse(Response response) {
            return new CreateXmlItemResponse(response);
        }
    }

    @POST
    @Path("/outer/boolean")
    @Produces({ "*/*" })
    @ApiOperation(value = "", notes = "Test serialization of outer boolean types", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Output boolean", response = Boolean.class) })
    FakeOuterBooleanSerializeResponse fakeOuterBooleanSerialize(@Valid Boolean body);

    public static class FakeOuterBooleanSerializeResponse extends org.openapitools.api.support.ResponseWrapper {
        private FakeOuterBooleanSerializeResponse(Response delegate) {
            super(delegate);
        }
        public static FakeOuterBooleanSerializeResponse with200StarStar(Boolean entity) {
            return new FakeOuterBooleanSerializeResponse(Response.status(200).header("Content-Type", "*/*").entity(entity).build());
        }
        public static FakeOuterBooleanSerializeResponse withCustomResponse(Response response) {
            return new FakeOuterBooleanSerializeResponse(response);
        }
    }

    @POST
    @Path("/outer/composite")
    @Produces({ "*/*" })
    @ApiOperation(value = "", notes = "Test serialization of object with outer number type", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Output composite", response = OuterComposite.class) })
    FakeOuterCompositeSerializeResponse fakeOuterCompositeSerialize(@Valid OuterComposite body);

    public static class FakeOuterCompositeSerializeResponse extends org.openapitools.api.support.ResponseWrapper {
        private FakeOuterCompositeSerializeResponse(Response delegate) {
            super(delegate);
        }
        public static FakeOuterCompositeSerializeResponse with200StarStar(OuterComposite entity) {
            return new FakeOuterCompositeSerializeResponse(Response.status(200).header("Content-Type", "*/*").entity(entity).build());
        }
        public static FakeOuterCompositeSerializeResponse withCustomResponse(Response response) {
            return new FakeOuterCompositeSerializeResponse(response);
        }
    }

    @POST
    @Path("/outer/number")
    @Produces({ "*/*" })
    @ApiOperation(value = "", notes = "Test serialization of outer number types", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Output number", response = BigDecimal.class) })
    FakeOuterNumberSerializeResponse fakeOuterNumberSerialize(@Valid BigDecimal body);

    public static class FakeOuterNumberSerializeResponse extends org.openapitools.api.support.ResponseWrapper {
        private FakeOuterNumberSerializeResponse(Response delegate) {
            super(delegate);
        }
        public static FakeOuterNumberSerializeResponse with200StarStar(BigDecimal entity) {
            return new FakeOuterNumberSerializeResponse(Response.status(200).header("Content-Type", "*/*").entity(entity).build());
        }
        public static FakeOuterNumberSerializeResponse withCustomResponse(Response response) {
            return new FakeOuterNumberSerializeResponse(response);
        }
    }

    @POST
    @Path("/outer/string")
    @Produces({ "*/*" })
    @ApiOperation(value = "", notes = "Test serialization of outer string types", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Output string", response = String.class) })
    FakeOuterStringSerializeResponse fakeOuterStringSerialize(@Valid String body);

    public static class FakeOuterStringSerializeResponse extends org.openapitools.api.support.ResponseWrapper {
        private FakeOuterStringSerializeResponse(Response delegate) {
            super(delegate);
        }
        public static FakeOuterStringSerializeResponse with200StarStar(String entity) {
            return new FakeOuterStringSerializeResponse(Response.status(200).header("Content-Type", "*/*").entity(entity).build());
        }
        public static FakeOuterStringSerializeResponse withCustomResponse(Response response) {
            return new FakeOuterStringSerializeResponse(response);
        }
    }

    @PUT
    @Path("/body-with-file-schema")
    @Consumes({ "application/json" })
    @ApiOperation(value = "", notes = "For this test, the body for this request much reference a schema named `File`.", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Success", response = Void.class) })
    TestBodyWithFileSchemaResponse testBodyWithFileSchema(@Valid @NotNull FileSchemaTestClass body);

    public static class TestBodyWithFileSchemaResponse extends org.openapitools.api.support.ResponseWrapper {
        private TestBodyWithFileSchemaResponse(Response delegate) {
            super(delegate);
        }
        public static TestBodyWithFileSchemaResponse with200() {
            return new TestBodyWithFileSchemaResponse(Response.status(200).build());
        }
        public static TestBodyWithFileSchemaResponse withCustomResponse(Response response) {
            return new TestBodyWithFileSchemaResponse(response);
        }
    }

    @PUT
    @Path("/body-with-query-params")
    @Consumes({ "application/json" })
    @ApiOperation(value = "", notes = "", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Success", response = Void.class) })
    TestBodyWithQueryParamsResponse testBodyWithQueryParams(@QueryParam("query") @NotNull    String query,@Valid @NotNull User body);

    public static class TestBodyWithQueryParamsResponse extends org.openapitools.api.support.ResponseWrapper {
        private TestBodyWithQueryParamsResponse(Response delegate) {
            super(delegate);
        }
        public static TestBodyWithQueryParamsResponse with200() {
            return new TestBodyWithQueryParamsResponse(Response.status(200).build());
        }
        public static TestBodyWithQueryParamsResponse withCustomResponse(Response response) {
            return new TestBodyWithQueryParamsResponse(response);
        }
    }

    @PATCH
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    @ApiOperation(value = "To test \"client\" model", notes = "To test \"client\" model", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Client.class) })
    TestClientModelResponse testClientModel(@Valid @NotNull Client body);

    public static class TestClientModelResponse extends org.openapitools.api.support.ResponseWrapper {
        private TestClientModelResponse(Response delegate) {
            super(delegate);
        }
        public static TestClientModelResponse with200ApplicationJson(Client entity) {
            return new TestClientModelResponse(Response.status(200).header("Content-Type", "application/json").entity(entity).build());
        }
        public static TestClientModelResponse withCustomResponse(Response response) {
            return new TestClientModelResponse(response);
        }
    }

    @POST
    @Consumes({ "application/x-www-form-urlencoded" })
    @ApiOperation(value = "Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트", notes = "Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트", authorizations = {
        
        @Authorization(value = "http_basic_test")
         }, tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid username supplied", response = Void.class),
        @ApiResponse(code = 404, message = "User not found", response = Void.class) })
    TestEndpointParametersResponse testEndpointParameters(@FormParam(value = "number")  BigDecimal number,@FormParam(value = "double")  Double _double,@FormParam(value = "pattern_without_delimiter")  String patternWithoutDelimiter,@FormParam(value = "byte")  byte[] _byte,@FormParam(value = "integer")  Integer integer,@FormParam(value = "int32")  Integer int32,@FormParam(value = "int64")  Long int64,@FormParam(value = "float")  Float _float,@FormParam(value = "string")  String string, @FormParam(value = "binary") InputStream binaryInputStream,@FormParam(value = "date")  LocalDate date,@FormParam(value = "dateTime")  Date dateTime,@FormParam(value = "password")  String password,@FormParam(value = "callback")  String paramCallback);

    public static class TestEndpointParametersResponse extends org.openapitools.api.support.ResponseWrapper {
        private TestEndpointParametersResponse(Response delegate) {
            super(delegate);
        }
        public static TestEndpointParametersResponse with400() {
            return new TestEndpointParametersResponse(Response.status(400).build());
        }
        public static TestEndpointParametersResponse with404() {
            return new TestEndpointParametersResponse(Response.status(404).build());
        }
        public static TestEndpointParametersResponse withCustomResponse(Response response) {
            return new TestEndpointParametersResponse(response);
        }
    }

    @GET
    @Consumes({ "application/x-www-form-urlencoded" })
    @ApiOperation(value = "To test enum parameters", notes = "To test enum parameters", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid request", response = Void.class),
        @ApiResponse(code = 404, message = "Not found", response = Void.class) })
    TestEnumParametersResponse testEnumParameters(@HeaderParam("enum_header_string_array")    @ApiParam("Header parameter enum test (string array)") List<String> enumHeaderStringArray,@HeaderParam("enum_header_string")   @DefaultValue("-efg")  @ApiParam("Header parameter enum test (string)") String enumHeaderString,@QueryParam("enum_query_string_array")   @ApiParam("Query parameter enum test (string array)")  List<String> enumQueryStringArray,@QueryParam("enum_query_string")  @DefaultValue("-efg")  @ApiParam("Query parameter enum test (string)")  String enumQueryString,@QueryParam("enum_query_integer")   @ApiParam("Query parameter enum test (double)")  Integer enumQueryInteger,@QueryParam("enum_query_double")   @ApiParam("Query parameter enum test (double)")  Double enumQueryDouble,@FormParam(value = "enum_form_string_array")  List<String> enumFormStringArray,@FormParam(value = "enum_form_string")  String enumFormString);

    public static class TestEnumParametersResponse extends org.openapitools.api.support.ResponseWrapper {
        private TestEnumParametersResponse(Response delegate) {
            super(delegate);
        }
        public static TestEnumParametersResponse with400() {
            return new TestEnumParametersResponse(Response.status(400).build());
        }
        public static TestEnumParametersResponse with404() {
            return new TestEnumParametersResponse(Response.status(404).build());
        }
        public static TestEnumParametersResponse withCustomResponse(Response response) {
            return new TestEnumParametersResponse(response);
        }
    }

    @DELETE
    @ApiOperation(value = "Fake endpoint to test group parameters (optional)", notes = "Fake endpoint to test group parameters (optional)", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Someting wrong", response = Void.class) })
    TestGroupParametersResponse testGroupParameters(@QueryParam("required_string_group") @NotNull   @ApiParam("Required String in group parameters")  Integer requiredStringGroup,@HeaderParam("required_boolean_group") @NotNull    @ApiParam("Required Boolean in group parameters") Boolean requiredBooleanGroup,@QueryParam("required_int64_group") @NotNull   @ApiParam("Required Integer in group parameters")  Long requiredInt64Group,@QueryParam("string_group")   @ApiParam("String in group parameters")  Integer stringGroup,@HeaderParam("boolean_group")    @ApiParam("Boolean in group parameters") Boolean booleanGroup,@QueryParam("int64_group")   @ApiParam("Integer in group parameters")  Long int64Group);

    public static class TestGroupParametersResponse extends org.openapitools.api.support.ResponseWrapper {
        private TestGroupParametersResponse(Response delegate) {
            super(delegate);
        }
        public static TestGroupParametersResponse with400() {
            return new TestGroupParametersResponse(Response.status(400).build());
        }
        public static TestGroupParametersResponse withCustomResponse(Response response) {
            return new TestGroupParametersResponse(response);
        }
    }

    @POST
    @Path("/inline-additionalProperties")
    @Consumes({ "application/json" })
    @ApiOperation(value = "test inline additionalProperties", notes = "", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class) })
    TestInlineAdditionalPropertiesResponse testInlineAdditionalProperties(@Valid @NotNull Map<String, String> param);

    public static class TestInlineAdditionalPropertiesResponse extends org.openapitools.api.support.ResponseWrapper {
        private TestInlineAdditionalPropertiesResponse(Response delegate) {
            super(delegate);
        }
        public static TestInlineAdditionalPropertiesResponse with200() {
            return new TestInlineAdditionalPropertiesResponse(Response.status(200).build());
        }
        public static TestInlineAdditionalPropertiesResponse withCustomResponse(Response response) {
            return new TestInlineAdditionalPropertiesResponse(response);
        }
    }

    @GET
    @Path("/jsonFormData")
    @Consumes({ "application/x-www-form-urlencoded" })
    @ApiOperation(value = "test json serialization of form data", notes = "", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class) })
    TestJsonFormDataResponse testJsonFormData(@FormParam(value = "param")  String param,@FormParam(value = "param2")  String param2);

    public static class TestJsonFormDataResponse extends org.openapitools.api.support.ResponseWrapper {
        private TestJsonFormDataResponse(Response delegate) {
            super(delegate);
        }
        public static TestJsonFormDataResponse with200() {
            return new TestJsonFormDataResponse(Response.status(200).build());
        }
        public static TestJsonFormDataResponse withCustomResponse(Response response) {
            return new TestJsonFormDataResponse(response);
        }
    }

    @PUT
    @Path("/test-query-paramters")
    @ApiOperation(value = "", notes = "To test the collection format in query parameters", tags={ "fake" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "Success", response = Void.class) })
    TestQueryParameterCollectionFormatResponse testQueryParameterCollectionFormat(@QueryParam("pipe") @NotNull    List<String> pipe,@QueryParam("ioutil") @NotNull    List<String> ioutil,@QueryParam("http") @NotNull    List<String> http,@QueryParam("url") @NotNull    List<String> url,@QueryParam("context") @NotNull    List<String> context);

    public static class TestQueryParameterCollectionFormatResponse extends org.openapitools.api.support.ResponseWrapper {
        private TestQueryParameterCollectionFormatResponse(Response delegate) {
            super(delegate);
        }
        public static TestQueryParameterCollectionFormatResponse with200() {
            return new TestQueryParameterCollectionFormatResponse(Response.status(200).build());
        }
        public static TestQueryParameterCollectionFormatResponse withCustomResponse(Response response) {
            return new TestQueryParameterCollectionFormatResponse(response);
        }
    }
}
