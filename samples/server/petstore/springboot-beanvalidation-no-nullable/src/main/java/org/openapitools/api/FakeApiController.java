package org.openapitools.api;

import java.math.BigDecimal;
import org.openapitools.model.Client;
import org.openapitools.model.FileSchemaTestClass;
import org.threeten.bp.LocalDate;
import java.util.Map;
import org.openapitools.model.ModelApiResponse;
import org.threeten.bp.OffsetDateTime;
import org.openapitools.model.OuterComposite;
import org.openapitools.model.User;
import org.openapitools.model.XmlItem;
import io.swagger.annotations.*;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.CookieValue;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.context.request.NativeWebRequest;
import org.springframework.web.multipart.MultipartFile;

import javax.validation.constraints.*;
import javax.validation.Valid;
import java.util.List;
import java.util.Map;
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
@Controller
@RequestMapping("${openapi.openAPIPetstore.base-path:/v2}")
public class FakeApiController implements FakeApi {

    private final NativeWebRequest request;

    @org.springframework.beans.factory.annotation.Autowired
    public FakeApiController(NativeWebRequest request) {
        this.request = request;
    }

    /**
     * POST /fake/create_xml_item : creates an XmlItem
     * this route creates an XmlItem
     *
     * @param xmlItem XmlItem Body (required)
     * @return successful operation (status code 200)
     * @see FakeApi#createXmlItem
     */
    public ResponseEntity<Void> createXmlItem(
        @ApiParam(value = "XmlItem Body", required = true) @Valid @RequestBody XmlItem xmlItem
    ) {
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * POST /fake/outer/boolean
     * Test serialization of outer boolean types
     *
     * @param body Input boolean as post body (optional)
     * @return Output boolean (status code 200)
     * @see FakeApi#fakeOuterBooleanSerialize
     */
    public ResponseEntity<Boolean> fakeOuterBooleanSerialize(
        @ApiParam(value = "Input boolean as post body") @Valid @RequestBody(required = false) Boolean body
    ) {
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * POST /fake/outer/composite
     * Test serialization of object with outer number type
     *
     * @param body Input composite as post body (optional)
     * @return Output composite (status code 200)
     * @see FakeApi#fakeOuterCompositeSerialize
     */
    public ResponseEntity<OuterComposite> fakeOuterCompositeSerialize(
        @ApiParam(value = "Input composite as post body") @Valid @RequestBody(required = false) OuterComposite body
    ) {
        for (MediaType mediaType: MediaType.parseMediaTypes(request.getHeader("Accept"))) {
            if (mediaType.isCompatibleWith(MediaType.valueOf("*/*"))) {
                String exampleString = "{ \"my_string\" : \"my_string\", \"my_number\" : 0.8008281904610115, \"my_boolean\" : true }";
                ApiUtil.setExampleResponse(request, "*/*", exampleString);
                break;
            }
        }
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * POST /fake/outer/number
     * Test serialization of outer number types
     *
     * @param body Input number as post body (optional)
     * @return Output number (status code 200)
     * @see FakeApi#fakeOuterNumberSerialize
     */
    public ResponseEntity<BigDecimal> fakeOuterNumberSerialize(
        @ApiParam(value = "Input number as post body") @Valid @RequestBody(required = false) BigDecimal body
    ) {
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * POST /fake/outer/string
     * Test serialization of outer string types
     *
     * @param body Input string as post body (optional)
     * @return Output string (status code 200)
     * @see FakeApi#fakeOuterStringSerialize
     */
    public ResponseEntity<String> fakeOuterStringSerialize(
        @ApiParam(value = "Input string as post body") @Valid @RequestBody(required = false) String body
    ) {
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * PUT /fake/body-with-file-schema
     * For this test, the body for this request much reference a schema named &#x60;File&#x60;.
     *
     * @param body  (required)
     * @return Success (status code 200)
     * @see FakeApi#testBodyWithFileSchema
     */
    public ResponseEntity<Void> testBodyWithFileSchema(
        @ApiParam(value = "", required = true) @Valid @RequestBody FileSchemaTestClass body
    ) {
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * PUT /fake/body-with-query-params
     *
     * @param query  (required)
     * @param body  (required)
     * @return Success (status code 200)
     * @see FakeApi#testBodyWithQueryParams
     */
    public ResponseEntity<Void> testBodyWithQueryParams(
        @NotNull @ApiParam(value = "", required = true) @Valid @RequestParam(value = "query", required = true) String query,
        @ApiParam(value = "", required = true) @Valid @RequestBody User body
    ) {
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * PATCH /fake : To test \&quot;client\&quot; model
     * To test \&quot;client\&quot; model
     *
     * @param body client model (required)
     * @return successful operation (status code 200)
     * @see FakeApi#testClientModel
     */
    public ResponseEntity<Client> testClientModel(
        @ApiParam(value = "client model", required = true) @Valid @RequestBody Client body
    ) {
        for (MediaType mediaType: MediaType.parseMediaTypes(request.getHeader("Accept"))) {
            if (mediaType.isCompatibleWith(MediaType.valueOf("application/json"))) {
                String exampleString = "{ \"client\" : \"client\" }";
                ApiUtil.setExampleResponse(request, "application/json", exampleString);
                break;
            }
        }
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * POST /fake : Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
     * Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
     *
     * @param number None (required)
     * @param _double None (required)
     * @param patternWithoutDelimiter None (required)
     * @param _byte None (required)
     * @param integer None (optional)
     * @param int32 None (optional)
     * @param int64 None (optional)
     * @param _float None (optional)
     * @param string None (optional)
     * @param binary None (optional)
     * @param date None (optional)
     * @param dateTime None (optional)
     * @param password None (optional)
     * @param paramCallback None (optional)
     * @return Invalid username supplied (status code 400)
     *         or User not found (status code 404)
     * @see FakeApi#testEndpointParameters
     */
    public ResponseEntity<Void> testEndpointParameters(
        @ApiParam(value = "None", required = true) @Valid @RequestPart(value = "number", required = true) BigDecimal number,
        @ApiParam(value = "None", required = true) @Valid @RequestPart(value = "double", required = true) Double _double,
        @ApiParam(value = "None", required = true) @Valid @RequestPart(value = "pattern_without_delimiter", required = true) String patternWithoutDelimiter,
        @ApiParam(value = "None", required = true) @Valid @RequestPart(value = "byte", required = true) byte[] _byte,
        @ApiParam(value = "None") @Valid @RequestPart(value = "integer", required = false) Integer integer,
        @ApiParam(value = "None") @Valid @RequestPart(value = "int32", required = false) Integer int32,
        @ApiParam(value = "None") @Valid @RequestPart(value = "int64", required = false) Long int64,
        @ApiParam(value = "None") @Valid @RequestPart(value = "float", required = false) Float _float,
        @ApiParam(value = "None") @Valid @RequestPart(value = "string", required = false) String string,
        @ApiParam(value = "None") @RequestPart(value = "binary", required = false) MultipartFile binary,
        @ApiParam(value = "None") @Valid @RequestPart(value = "date", required = false) @org.springframework.format.annotation.DateTimeFormat(iso = org.springframework.format.annotation.DateTimeFormat.ISO.DATE) LocalDate date,
        @ApiParam(value = "None") @Valid @RequestPart(value = "dateTime", required = false) @org.springframework.format.annotation.DateTimeFormat(iso = org.springframework.format.annotation.DateTimeFormat.ISO.DATE_TIME) OffsetDateTime dateTime,
        @ApiParam(value = "None") @Valid @RequestPart(value = "password", required = false) String password,
        @ApiParam(value = "None") @Valid @RequestPart(value = "callback", required = false) String paramCallback
    ) {
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * GET /fake : To test enum parameters
     * To test enum parameters
     *
     * @param enumHeaderStringArray Header parameter enum test (string array) (optional)
     * @param enumHeaderString Header parameter enum test (string) (optional, default to -efg)
     * @param enumQueryStringArray Query parameter enum test (string array) (optional)
     * @param enumQueryString Query parameter enum test (string) (optional, default to -efg)
     * @param enumQueryInteger Query parameter enum test (double) (optional)
     * @param enumQueryDouble Query parameter enum test (double) (optional)
     * @param enumFormStringArray Form parameter enum test (string array) (optional, default to $)
     * @param enumFormString Form parameter enum test (string) (optional, default to -efg)
     * @return Invalid request (status code 400)
     *         or Not found (status code 404)
     * @see FakeApi#testEnumParameters
     */
    public ResponseEntity<Void> testEnumParameters(
        @ApiParam(value = "Header parameter enum test (string array)", allowableValues = ">, $") @RequestHeader(value = "enum_header_string_array", required = false) List<String> enumHeaderStringArray,
        @ApiParam(value = "Header parameter enum test (string)", allowableValues = "_abc, -efg, (xyz)", defaultValue = "-efg") @RequestHeader(value = "enum_header_string", required = false) String enumHeaderString,
        @ApiParam(value = "Query parameter enum test (string array)", allowableValues = ">, $") @Valid @RequestParam(value = "enum_query_string_array", required = false) List<String> enumQueryStringArray,
        @ApiParam(value = "Query parameter enum test (string)", allowableValues = "_abc, -efg, (xyz)", defaultValue = "-efg") @Valid @RequestParam(value = "enum_query_string", required = false, defaultValue = "-efg") String enumQueryString,
        @ApiParam(value = "Query parameter enum test (double)", allowableValues = "1, -2") @Valid @RequestParam(value = "enum_query_integer", required = false) Integer enumQueryInteger,
        @ApiParam(value = "Query parameter enum test (double)", allowableValues = "1.1, -1.2") @Valid @RequestParam(value = "enum_query_double", required = false) Double enumQueryDouble,
        @ApiParam(value = "Form parameter enum test (string array)", allowableValues = ">, $") @Valid @RequestPart(value = "enum_form_string_array", required = false) List<String> enumFormStringArray,
        @ApiParam(value = "Form parameter enum test (string)", allowableValues = "_abc, -efg, (xyz)", defaultValue = "-efg") @Valid @RequestPart(value = "enum_form_string", required = false) String enumFormString
    ) {
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * DELETE /fake : Fake endpoint to test group parameters (optional)
     * Fake endpoint to test group parameters (optional)
     *
     * @param requiredStringGroup Required String in group parameters (required)
     * @param requiredBooleanGroup Required Boolean in group parameters (required)
     * @param requiredInt64Group Required Integer in group parameters (required)
     * @param stringGroup String in group parameters (optional)
     * @param booleanGroup Boolean in group parameters (optional)
     * @param int64Group Integer in group parameters (optional)
     * @return Someting wrong (status code 400)
     * @see FakeApi#testGroupParameters
     */
    public ResponseEntity<Void> testGroupParameters(
        @NotNull @ApiParam(value = "Required String in group parameters", required = true) @Valid @RequestParam(value = "required_string_group", required = true) Integer requiredStringGroup,
        @ApiParam(value = "Required Boolean in group parameters", required = true) @RequestHeader(value = "required_boolean_group", required = true) Boolean requiredBooleanGroup,
        @NotNull @ApiParam(value = "Required Integer in group parameters", required = true) @Valid @RequestParam(value = "required_int64_group", required = true) Long requiredInt64Group,
        @ApiParam(value = "String in group parameters") @Valid @RequestParam(value = "string_group", required = false) Integer stringGroup,
        @ApiParam(value = "Boolean in group parameters") @RequestHeader(value = "boolean_group", required = false) Boolean booleanGroup,
        @ApiParam(value = "Integer in group parameters") @Valid @RequestParam(value = "int64_group", required = false) Long int64Group
    ) {
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * POST /fake/inline-additionalProperties : test inline additionalProperties
     *
     * @param param request body (required)
     * @return successful operation (status code 200)
     * @see FakeApi#testInlineAdditionalProperties
     */
    public ResponseEntity<Void> testInlineAdditionalProperties(
        @ApiParam(value = "request body", required = true) @Valid @RequestBody Map<String, String> param
    ) {
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * GET /fake/jsonFormData : test json serialization of form data
     *
     * @param param field1 (required)
     * @param param2 field2 (required)
     * @return successful operation (status code 200)
     * @see FakeApi#testJsonFormData
     */
    public ResponseEntity<Void> testJsonFormData(
        @ApiParam(value = "field1", required = true) @Valid @RequestPart(value = "param", required = true) String param,
        @ApiParam(value = "field2", required = true) @Valid @RequestPart(value = "param2", required = true) String param2
    ) {
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * PUT /fake/test-query-parameters
     * To test the collection format in query parameters
     *
     * @param pipe  (required)
     * @param ioutil  (required)
     * @param http  (required)
     * @param url  (required)
     * @param context  (required)
     * @return Success (status code 200)
     * @see FakeApi#testQueryParameterCollectionFormat
     */
    public ResponseEntity<Void> testQueryParameterCollectionFormat(
        @NotNull @ApiParam(value = "", required = true) @Valid @RequestParam(value = "pipe", required = true) List<String> pipe,
        @NotNull @ApiParam(value = "", required = true) @Valid @RequestParam(value = "ioutil", required = true) List<String> ioutil,
        @NotNull @ApiParam(value = "", required = true) @Valid @RequestParam(value = "http", required = true) List<String> http,
        @NotNull @ApiParam(value = "", required = true) @Valid @RequestParam(value = "url", required = true) List<String> url,
        @NotNull @ApiParam(value = "", required = true) @Valid @RequestParam(value = "context", required = true) List<String> context
    ) {
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * POST /fake/{petId}/uploadImageWithRequiredFile : uploads an image (required)
     *
     * @param petId ID of pet to update (required)
     * @param requiredFile file to upload (required)
     * @param additionalMetadata Additional data to pass to server (optional)
     * @return successful operation (status code 200)
     * @see FakeApi#uploadFileWithRequiredFile
     */
    public ResponseEntity<ModelApiResponse> uploadFileWithRequiredFile(
        @ApiParam(value = "ID of pet to update", required = true) @PathVariable("petId") Long petId,
        @ApiParam(value = "file to upload", required = true) @RequestPart(value = "requiredFile", required = true) MultipartFile requiredFile,
        @ApiParam(value = "Additional data to pass to server") @Valid @RequestPart(value = "additionalMetadata", required = false) String additionalMetadata
    ) {
        for (MediaType mediaType: MediaType.parseMediaTypes(request.getHeader("Accept"))) {
            if (mediaType.isCompatibleWith(MediaType.valueOf("application/json"))) {
                String exampleString = "{ \"code\" : 0, \"type\" : \"type\", \"message\" : \"message\" }";
                ApiUtil.setExampleResponse(request, "application/json", exampleString);
                break;
            }
        }
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

}
