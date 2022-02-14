package org.openapitools.api;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.beans.factory.annotation.Autowired;
import java.util.Optional;
import javax.annotation.Generated;

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
@Controller
@RequestMapping("${openapi.openAPIPetstore.base-path:/v2}")
public class FakeApiController implements FakeApi {

    private final FakeApiDelegate delegate;

    public FakeApiController(@Autowired(required = false) FakeApiDelegate delegate) {
        this.delegate = Optional.ofNullable(delegate).orElse(new FakeApiDelegate() {});
    }

    @Override
    public FakeApiDelegate getDelegate() {
        return delegate;
    }

    /**
     * POST /fake/create_xml_item : creates an XmlItem
     * this route creates an XmlItem
     *
     * @param xmlItem XmlItem Body (required)
     * @return successful operation (status code 200)
     * @see FakeApi#createXmlItem
     */
    public Mono<ResponseEntity<Void>> createXmlItem(
        @ApiParam(value = "XmlItem Body", required = true) @Valid @RequestBody Mono<XmlItem> xmlItem
    ) {
        return delegate.createXmlItem(xmlItem);
    }

    /**
     * POST /fake/outer/boolean
     * Test serialization of outer boolean types
     *
     * @param body Input boolean as post body (optional)
     * @return Output boolean (status code 200)
     * @see FakeApi#fakeOuterBooleanSerialize
     */
    public Mono<ResponseEntity<Boolean>> fakeOuterBooleanSerialize(
        @ApiParam(value = "Input boolean as post body") @Valid @RequestBody(required = false) Mono<Boolean> body
    ) {
        return delegate.fakeOuterBooleanSerialize(body);
    }

    /**
     * POST /fake/outer/composite
     * Test serialization of object with outer number type
     *
     * @param body Input composite as post body (optional)
     * @return Output composite (status code 200)
     * @see FakeApi#fakeOuterCompositeSerialize
     */
    public Mono<ResponseEntity<OuterComposite>> fakeOuterCompositeSerialize(
        @ApiParam(value = "Input composite as post body") @Valid @RequestBody(required = false) Mono<OuterComposite> body
    ) {
        return delegate.fakeOuterCompositeSerialize(body);
    }

    /**
     * POST /fake/outer/number
     * Test serialization of outer number types
     *
     * @param body Input number as post body (optional)
     * @return Output number (status code 200)
     * @see FakeApi#fakeOuterNumberSerialize
     */
    public Mono<ResponseEntity<BigDecimal>> fakeOuterNumberSerialize(
        @ApiParam(value = "Input number as post body") @Valid @RequestBody(required = false) Mono<BigDecimal> body
    ) {
        return delegate.fakeOuterNumberSerialize(body);
    }

    /**
     * POST /fake/outer/string
     * Test serialization of outer string types
     *
     * @param body Input string as post body (optional)
     * @return Output string (status code 200)
     * @see FakeApi#fakeOuterStringSerialize
     */
    public Mono<ResponseEntity<String>> fakeOuterStringSerialize(
        @ApiParam(value = "Input string as post body") @Valid @RequestBody(required = false) Mono<String> body
    ) {
        return delegate.fakeOuterStringSerialize(body);
    }

    /**
     * PUT /fake/body-with-file-schema
     * For this test, the body for this request much reference a schema named &#x60;File&#x60;.
     *
     * @param body  (required)
     * @return Success (status code 200)
     * @see FakeApi#testBodyWithFileSchema
     */
    public Mono<ResponseEntity<Void>> testBodyWithFileSchema(
        @ApiParam(value = "", required = true) @Valid @RequestBody Mono<FileSchemaTestClass> body
    ) {
        return delegate.testBodyWithFileSchema(body);
    }

    /**
     * PUT /fake/body-with-query-params
     *
     * @param query  (required)
     * @param body  (required)
     * @return Success (status code 200)
     * @see FakeApi#testBodyWithQueryParams
     */
    public Mono<ResponseEntity<Void>> testBodyWithQueryParams(
        @NotNull @ApiParam(value = "", required = true) @Valid @RequestParam(value = "query", required = true) String query,
        @ApiParam(value = "", required = true) @Valid @RequestBody Mono<User> body
    ) {
        return delegate.testBodyWithQueryParams(query, body);
    }

    /**
     * PATCH /fake : To test \&quot;client\&quot; model
     * To test \&quot;client\&quot; model
     *
     * @param body client model (required)
     * @return successful operation (status code 200)
     * @see FakeApi#testClientModel
     */
    public Mono<ResponseEntity<Client>> testClientModel(
        @ApiParam(value = "client model", required = true) @Valid @RequestBody Mono<Client> body
    ) {
        return delegate.testClientModel(body);
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
    public Mono<ResponseEntity<Void>> testEndpointParameters(
        @ApiParam(value = "None", required = true) @Valid @RequestPart(value = "number", required = true) BigDecimal number,
        @ApiParam(value = "None", required = true) @Valid @RequestPart(value = "double", required = true) Double _double,
        @ApiParam(value = "None", required = true) @Valid @RequestPart(value = "pattern_without_delimiter", required = true) String patternWithoutDelimiter,
        @ApiParam(value = "None", required = true) @Valid @RequestPart(value = "byte", required = true) byte[] _byte,
        @ApiParam(value = "None") @Valid @RequestPart(value = "integer", required = false) Integer integer,
        @ApiParam(value = "None") @Valid @RequestPart(value = "int32", required = false) Integer int32,
        @ApiParam(value = "None") @Valid @RequestPart(value = "int64", required = false) Long int64,
        @ApiParam(value = "None") @Valid @RequestPart(value = "float", required = false) Float _float,
        @ApiParam(value = "None") @Valid @RequestPart(value = "string", required = false) String string,
        @ApiParam(value = "None") @RequestPart(value = "binary", required = false) Flux<Part> binary,
        @ApiParam(value = "None") @Valid @RequestPart(value = "date", required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate date,
        @ApiParam(value = "None") @Valid @RequestPart(value = "dateTime", required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME) OffsetDateTime dateTime,
        @ApiParam(value = "None") @Valid @RequestPart(value = "password", required = false) String password,
        @ApiParam(value = "None") @Valid @RequestPart(value = "callback", required = false) String paramCallback
    ) {
        return delegate.testEndpointParameters(number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, string, binary, date, dateTime, password, paramCallback);
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
    public Mono<ResponseEntity<Void>> testEnumParameters(
        @ApiParam(value = "Header parameter enum test (string array)", allowableValues = ">, $") @RequestHeader(value = "enum_header_string_array", required = false) List<String> enumHeaderStringArray,
        @ApiParam(value = "Header parameter enum test (string)", allowableValues = "_abc, -efg, (xyz)", defaultValue = "-efg") @RequestHeader(value = "enum_header_string", required = false, defaultValue = "-efg") String enumHeaderString,
        @ApiParam(value = "Query parameter enum test (string array)", allowableValues = ">, $") @Valid @RequestParam(value = "enum_query_string_array", required = false) List<String> enumQueryStringArray,
        @ApiParam(value = "Query parameter enum test (string)", allowableValues = "_abc, -efg, (xyz)", defaultValue = "-efg") @Valid @RequestParam(value = "enum_query_string", required = false, defaultValue = "-efg") String enumQueryString,
        @ApiParam(value = "Query parameter enum test (double)", allowableValues = "1, -2") @Valid @RequestParam(value = "enum_query_integer", required = false) Integer enumQueryInteger,
        @ApiParam(value = "Query parameter enum test (double)", allowableValues = "1.1, -1.2") @Valid @RequestParam(value = "enum_query_double", required = false) Double enumQueryDouble,
        @ApiParam(value = "Form parameter enum test (string array)", allowableValues = ">, $") @Valid @RequestPart(value = "enum_form_string_array", required = false) List<String> enumFormStringArray,
        @ApiParam(value = "Form parameter enum test (string)", allowableValues = "_abc, -efg, (xyz)", defaultValue = "-efg") @Valid @RequestPart(value = "enum_form_string", required = false) String enumFormString
    ) {
        return delegate.testEnumParameters(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumFormStringArray, enumFormString);
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
    public Mono<ResponseEntity<Void>> testGroupParameters(
        @NotNull @ApiParam(value = "Required String in group parameters", required = true) @Valid @RequestParam(value = "required_string_group", required = true) Integer requiredStringGroup,
        @ApiParam(value = "Required Boolean in group parameters", required = true) @RequestHeader(value = "required_boolean_group", required = true) Boolean requiredBooleanGroup,
        @NotNull @ApiParam(value = "Required Integer in group parameters", required = true) @Valid @RequestParam(value = "required_int64_group", required = true) Long requiredInt64Group,
        @ApiParam(value = "String in group parameters") @Valid @RequestParam(value = "string_group", required = false) Integer stringGroup,
        @ApiParam(value = "Boolean in group parameters") @RequestHeader(value = "boolean_group", required = false) Boolean booleanGroup,
        @ApiParam(value = "Integer in group parameters") @Valid @RequestParam(value = "int64_group", required = false) Long int64Group
    ) {
        return delegate.testGroupParameters(requiredStringGroup, requiredBooleanGroup, requiredInt64Group, stringGroup, booleanGroup, int64Group);
    }

    /**
     * POST /fake/inline-additionalProperties : test inline additionalProperties
     *
     * @param param request body (required)
     * @return successful operation (status code 200)
     * @see FakeApi#testInlineAdditionalProperties
     */
    public Mono<ResponseEntity<Void>> testInlineAdditionalProperties(
        @ApiParam(value = "request body", required = true) @Valid @RequestBody Mono<Map<String, String>> param
    ) {
        return delegate.testInlineAdditionalProperties(param);
    }

    /**
     * GET /fake/jsonFormData : test json serialization of form data
     *
     * @param param field1 (required)
     * @param param2 field2 (required)
     * @return successful operation (status code 200)
     * @see FakeApi#testJsonFormData
     */
    public Mono<ResponseEntity<Void>> testJsonFormData(
        @ApiParam(value = "field1", required = true) @Valid @RequestPart(value = "param", required = true) String param,
        @ApiParam(value = "field2", required = true) @Valid @RequestPart(value = "param2", required = true) String param2
    ) {
        return delegate.testJsonFormData(param, param2);
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
    public Mono<ResponseEntity<Void>> testQueryParameterCollectionFormat(
        @NotNull @ApiParam(value = "", required = true) @Valid @RequestParam(value = "pipe", required = true) List<String> pipe,
        @NotNull @ApiParam(value = "", required = true) @Valid @RequestParam(value = "ioutil", required = true) List<String> ioutil,
        @NotNull @ApiParam(value = "", required = true) @Valid @RequestParam(value = "http", required = true) List<String> http,
        @NotNull @ApiParam(value = "", required = true) @Valid @RequestParam(value = "url", required = true) List<String> url,
        @NotNull @ApiParam(value = "", required = true) @Valid @RequestParam(value = "context", required = true) List<String> context
    ) {
        return delegate.testQueryParameterCollectionFormat(pipe, ioutil, http, url, context);
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
    public Mono<ResponseEntity<ModelApiResponse>> uploadFileWithRequiredFile(
        @ApiParam(value = "ID of pet to update", required = true) @PathVariable("petId") Long petId,
        @ApiParam(value = "file to upload", required = true) @RequestPart(value = "requiredFile", required = true) Flux<Part> requiredFile,
        @ApiParam(value = "Additional data to pass to server") @Valid @RequestPart(value = "additionalMetadata", required = false) String additionalMetadata
    ) {
        return delegate.uploadFileWithRequiredFile(petId, requiredFile, additionalMetadata);
    }

}
