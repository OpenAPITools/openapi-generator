package org.openapitools.api;

import java.math.BigDecimal;
import org.openapitools.model.Client;
import java.io.File;
import org.openapitools.model.FileSchemaTestClass;
import java.time.LocalDate;
import java.util.Map;
import org.openapitools.model.ModelApiResponse;
import java.time.OffsetDateTime;
import org.openapitools.model.OuterComposite;
import org.openapitools.model.User;
import org.openapitools.model.XmlItem;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;


import java.io.InputStream;
import java.util.Map;
import java.util.List;
import javax.validation.constraints.*;
import javax.validation.Valid;

@Path("/fake")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen")public interface FakeApi {

    @POST
    @Path("/create_xml_item")
    @Consumes({ "application/xml", "application/xml; charset=utf-8", "application/xml; charset=utf-16", "text/xml", "text/xml; charset=utf-8", "text/xml; charset=utf-16" })
    void createXmlItem(@Valid @NotNull XmlItem xmlItem);

    @POST
    @Path("/outer/boolean")
    @Produces({ "*/*" })
    Boolean fakeOuterBooleanSerialize(@Valid Boolean body);

    @POST
    @Path("/outer/composite")
    @Produces({ "*/*" })
    OuterComposite fakeOuterCompositeSerialize(@Valid OuterComposite body);

    @POST
    @Path("/outer/number")
    @Produces({ "*/*" })
    BigDecimal fakeOuterNumberSerialize(@Valid BigDecimal body);

    @POST
    @Path("/outer/string")
    @Produces({ "*/*" })
    String fakeOuterStringSerialize(@Valid String body);

    @PUT
    @Path("/body-with-file-schema")
    @Consumes({ "application/json" })
    void testBodyWithFileSchema(@Valid @NotNull FileSchemaTestClass body);

    @PUT
    @Path("/body-with-query-params")
    @Consumes({ "application/json" })
    void testBodyWithQueryParams(@QueryParam("query") @NotNull   String query,@Valid @NotNull User body);

    @PATCH
    @Consumes({ "application/json" })
    @Produces({ "application/json" })
    Client testClientModel(@Valid @NotNull Client body);

    @POST
    @Consumes({ "application/x-www-form-urlencoded" })
    void testEndpointParameters(@FormParam(value = "number")  BigDecimal number,@FormParam(value = "double")  Double _double,@FormParam(value = "pattern_without_delimiter")  String patternWithoutDelimiter,@FormParam(value = "byte")  byte[] _byte,@FormParam(value = "integer")  Integer integer,@FormParam(value = "int32")  Integer int32,@FormParam(value = "int64")  Long int64,@FormParam(value = "float")  Float _float,@FormParam(value = "string")  String string, @FormParam(value = "binary") InputStream binaryInputStream,@FormParam(value = "date")  LocalDate date,@FormParam(value = "dateTime")  OffsetDateTime dateTime,@FormParam(value = "password")  String password,@FormParam(value = "callback")  String paramCallback);

    @GET
    @Consumes({ "application/x-www-form-urlencoded" })
    void testEnumParameters(@HeaderParam("enum_header_string_array")   List<String> enumHeaderStringArray,@HeaderParam("enum_header_string")  @DefaultValue("-efg")  String enumHeaderString,@QueryParam("enum_query_string_array")   List<String> enumQueryStringArray,@QueryParam("enum_query_string") @DefaultValue("-efg")   String enumQueryString,@QueryParam("enum_query_integer")   Integer enumQueryInteger,@QueryParam("enum_query_double")   Double enumQueryDouble,@FormParam(value = "enum_form_string_array")  List<String> enumFormStringArray,@FormParam(value = "enum_form_string")  String enumFormString);

    @DELETE
    void testGroupParameters(@QueryParam("required_string_group") @NotNull   Integer requiredStringGroup,@HeaderParam("required_boolean_group") @NotNull   Boolean requiredBooleanGroup,@QueryParam("required_int64_group") @NotNull   Long requiredInt64Group,@QueryParam("string_group")   Integer stringGroup,@HeaderParam("boolean_group")   Boolean booleanGroup,@QueryParam("int64_group")   Long int64Group);

    @POST
    @Path("/inline-additionalProperties")
    @Consumes({ "application/json" })
    void testInlineAdditionalProperties(@Valid @NotNull Map<String, String> param);

    @GET
    @Path("/jsonFormData")
    @Consumes({ "application/x-www-form-urlencoded" })
    void testJsonFormData(@FormParam(value = "param")  String param,@FormParam(value = "param2")  String param2);

    @PUT
    @Path("/test-query-parameters")
    void testQueryParameterCollectionFormat(@QueryParam("pipe") @NotNull   List<String> pipe,@QueryParam("ioutil") @NotNull   List<String> ioutil,@QueryParam("http") @NotNull   List<String> http,@QueryParam("url") @NotNull   List<String> url,@QueryParam("context") @NotNull   List<String> context);

    @POST
    @Path("/{petId}/uploadImageWithRequiredFile")
    @Consumes({ "multipart/form-data" })
    @Produces({ "application/json" })
    ModelApiResponse uploadFileWithRequiredFile(@PathParam("petId") Long petId, @FormParam(value = "requiredFile") InputStream requiredFileInputStream,@FormParam(value = "additionalMetadata")  String additionalMetadata);
}
