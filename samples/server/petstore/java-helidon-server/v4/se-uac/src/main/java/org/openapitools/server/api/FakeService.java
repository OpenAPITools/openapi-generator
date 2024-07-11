package org.openapitools.server.api;

import java.util.ArrayList;
import java.math.BigDecimal;
import org.openapitools.server.model.ChildWithNullable;
import org.openapitools.server.model.Client;
import java.util.stream.Collectors;
import org.openapitools.server.model.EnumClass;
import org.openapitools.server.model.FakeBigDecimalMap200Response;
import java.io.File;
import org.openapitools.server.model.FileSchemaTestClass;
import java.nio.file.Files;
import org.openapitools.server.model.GenericTypes;
import java.util.HashMap;
import io.helidon.http.HeaderNames;
import io.helidon.http.Headers;
import org.openapitools.server.model.HealthCheckResult;
import java.util.HexFormat;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.time.LocalDate;
import java.util.Map;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.Objects;
import java.time.OffsetDateTime;
import java.util.Optional;
import org.openapitools.server.model.OuterComposite;
import org.openapitools.server.model.OuterObjectWithEnumProperty;
import io.helidon.common.parameters.Parameters;
import java.nio.file.Path;
import org.openapitools.server.model.Pet;
import io.helidon.http.media.multipart.ReadablePart;
import io.helidon.http.Status;
import org.openapitools.server.model.TestInlineFreeformAdditionalPropertiesRequest;
import java.io.UncheckedIOException;
import org.openapitools.server.model.User;
import io.helidon.common.mapper.Value;

import java.util.Optional;
import java.util.logging.Logger;

import io.helidon.webserver.http.HttpRules;
import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;
import io.helidon.webserver.http.HttpService;


public abstract class FakeService implements HttpService {

    protected static final Logger LOGGER = Logger.getLogger(FakeService.class.getName());
    protected static final ObjectMapper MAPPER = JsonProvider.objectMapper();

    protected FakeBigDecimalMap fakeBigDecimalMap = fakeBigDecimalMap();
    protected FakeHealthGet fakeHealthGet = fakeHealthGet();
    protected FakeHttpSignatureTest fakeHttpSignatureTest = fakeHttpSignatureTest();
    protected FakeOuterBooleanSerialize fakeOuterBooleanSerialize = fakeOuterBooleanSerialize();
    protected FakeOuterCompositeSerialize fakeOuterCompositeSerialize = fakeOuterCompositeSerialize();
    protected FakeOuterNumberSerialize fakeOuterNumberSerialize = fakeOuterNumberSerialize();
    protected FakeOuterStringSerialize fakeOuterStringSerialize = fakeOuterStringSerialize();
    protected FakePropertyEnumIntegerSerialize fakePropertyEnumIntegerSerialize = fakePropertyEnumIntegerSerialize();
    protected TestAdditionalPropertiesReference testAdditionalPropertiesReference = testAdditionalPropertiesReference();
    protected TestBodyWithBinary testBodyWithBinary = testBodyWithBinary();
    protected TestBodyWithFileSchema testBodyWithFileSchema = testBodyWithFileSchema();
    protected TestBodyWithQueryParams testBodyWithQueryParams = testBodyWithQueryParams();
    protected TestClientModel testClientModel = testClientModel();
    protected TestEndpointParameters testEndpointParameters = testEndpointParameters();
    protected TestEnumParameters testEnumParameters = testEnumParameters();
    protected TestGroupParameters testGroupParameters = testGroupParameters();
    protected TestInlineAdditionalProperties testInlineAdditionalProperties = testInlineAdditionalProperties();
    protected TestInlineFreeformAdditionalProperties testInlineFreeformAdditionalProperties = testInlineFreeformAdditionalProperties();
    protected TestJsonFormData testJsonFormData = testJsonFormData();
    protected TestNullable testNullable = testNullable();
    protected TestQueryParameterCollectionFormat testQueryParameterCollectionFormat = testQueryParameterCollectionFormat();
    protected TestStringMapReference testStringMapReference = testStringMapReference();


    /**
     * A service registers itself by updating the routing rules.
     * @param rules the routing rules.
     */
    @Override
    public void routing(HttpRules rules) {
        rules.get("/fake/BigDecimalMap", this::fakeBigDecimalMap);
        rules.get("/fake/health", this::fakeHealthGet);
        rules.get("/fake/http-signature-test", this::fakeHttpSignatureTest);
        rules.post("/fake/outer/boolean", this::fakeOuterBooleanSerialize);
        rules.post("/fake/outer/composite", this::fakeOuterCompositeSerialize);
        rules.post("/fake/outer/number", this::fakeOuterNumberSerialize);
        rules.post("/fake/outer/string", this::fakeOuterStringSerialize);
        rules.post("/fake/property/enum-int", this::fakePropertyEnumIntegerSerialize);
        rules.post("/fake/additionalProperties-reference", this::testAdditionalPropertiesReference);
        rules.put("/fake/body-with-binary", this::testBodyWithBinary);
        rules.put("/fake/body-with-file-schema", this::testBodyWithFileSchema);
        rules.put("/fake/body-with-query-params", this::testBodyWithQueryParams);
        rules.patch("/fake", this::testClientModel);
        rules.post("/fake", this::testEndpointParameters);
        rules.get("/fake", this::testEnumParameters);
        rules.delete("/fake", this::testGroupParameters);
        rules.post("/fake/inline-additionalProperties", this::testInlineAdditionalProperties);
        rules.post("/fake/inline-freeform-additionalProperties", this::testInlineFreeformAdditionalProperties);
        rules.get("/fake/jsonFormData", this::testJsonFormData);
        rules.post("/fake/nullable", this::testNullable);
        rules.put("/fake/test-query-parameters", this::testQueryParameterCollectionFormat);
        rules.post("/fake/stringMap-reference", this::testStringMapReference);
    }


    /**
     * GET /fake/BigDecimalMap.
     *
     * @param request the server request
     * @param response the server response
     */
    protected void fakeBigDecimalMap(ServerRequest request, ServerResponse response) { 

                handleFakeBigDecimalMap(request, response);
    }

    /**
     * Handle GET /fake/BigDecimalMap.
     *
     * @param request the server request
     * @param response the server response
     */
    protected abstract void handleFakeBigDecimalMap(ServerRequest request, ServerResponse response);

    /**
     * GET /fake/health : Health check endpoint.
     *
     * @param request the server request
     * @param response the server response
     */
    protected void fakeHealthGet(ServerRequest request, ServerResponse response) { 

                handleFakeHealthGet(request, response);
    }

    /**
     * Handle GET /fake/health : Health check endpoint.
     *
     * @param request the server request
     * @param response the server response
     */
    protected abstract void handleFakeHealthGet(ServerRequest request, ServerResponse response);

    /**
     * GET /fake/http-signature-test : test http signature authentication.
     *
     * @param request the server request
     * @param response the server response
     */
    protected void fakeHttpSignatureTest(ServerRequest request, ServerResponse response) { 

        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);

        // Parameter: Pet
        Pet pet = fakeHttpSignatureTest.pet(request, validator);


        // Parameter: query_1
        Optional<String> query1 = fakeHttpSignatureTest.query1(request, validator);


        // Parameter: header_1
        Optional<String> header1 = fakeHttpSignatureTest.header1(request, validator);

        validator.execute();

        handleFakeHttpSignatureTest(request, response, 
                    pet, 
                    query1, 
                    header1);
    }

    /**
     * Handle GET /fake/http-signature-test : test http signature authentication.
     *
     * @param request the server request
     * @param response the server response
     * @param pet Pet object that needs to be added to the store 
     * @param query1 query parameter 
     * @param header1 header parameter 
     */
    protected abstract void handleFakeHttpSignatureTest(ServerRequest request, ServerResponse response, 
                Pet pet, 
                Optional<String> query1, 
                Optional<String> header1);

    /**
     * POST /fake/outer/boolean.
     *
     * @param request the server request
     * @param response the server response
     */
    protected void fakeOuterBooleanSerialize(ServerRequest request, ServerResponse response) { 

        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);

        // Parameter: body
        Optional<Boolean> body = fakeOuterBooleanSerialize.body(request, validator);

        validator.execute();

        handleFakeOuterBooleanSerialize(request, response, 
                    body);
    }

    /**
     * Handle POST /fake/outer/boolean.
     *
     * @param request the server request
     * @param response the server response
     * @param body Input boolean as post body 
     */
    protected abstract void handleFakeOuterBooleanSerialize(ServerRequest request, ServerResponse response, 
                Optional<Boolean> body);

    /**
     * POST /fake/outer/composite.
     *
     * @param request the server request
     * @param response the server response
     */
    protected void fakeOuterCompositeSerialize(ServerRequest request, ServerResponse response) { 

        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);

        // Parameter: OuterComposite
        Optional<OuterComposite> outerComposite = fakeOuterCompositeSerialize.outerComposite(request, validator);

        validator.execute();

        handleFakeOuterCompositeSerialize(request, response, 
                    outerComposite);
    }

    /**
     * Handle POST /fake/outer/composite.
     *
     * @param request the server request
     * @param response the server response
     * @param outerComposite Input composite as post body 
     */
    protected abstract void handleFakeOuterCompositeSerialize(ServerRequest request, ServerResponse response, 
                Optional<OuterComposite> outerComposite);

    /**
     * POST /fake/outer/number.
     *
     * @param request the server request
     * @param response the server response
     */
    protected void fakeOuterNumberSerialize(ServerRequest request, ServerResponse response) { 

        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);

        // Parameter: body
        Optional<BigDecimal> body = fakeOuterNumberSerialize.body(request, validator);

        validator.execute();

        handleFakeOuterNumberSerialize(request, response, 
                    body);
    }

    /**
     * Handle POST /fake/outer/number.
     *
     * @param request the server request
     * @param response the server response
     * @param body Input number as post body 
     */
    protected abstract void handleFakeOuterNumberSerialize(ServerRequest request, ServerResponse response, 
                Optional<BigDecimal> body);

    /**
     * POST /fake/outer/string.
     *
     * @param request the server request
     * @param response the server response
     */
    protected void fakeOuterStringSerialize(ServerRequest request, ServerResponse response) { 

        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);

        // Parameter: body
        Optional<String> body = fakeOuterStringSerialize.body(request, validator);

        validator.execute();

        handleFakeOuterStringSerialize(request, response, 
                    body);
    }

    /**
     * Handle POST /fake/outer/string.
     *
     * @param request the server request
     * @param response the server response
     * @param body Input string as post body 
     */
    protected abstract void handleFakeOuterStringSerialize(ServerRequest request, ServerResponse response, 
                Optional<String> body);

    /**
     * POST /fake/property/enum-int.
     *
     * @param request the server request
     * @param response the server response
     */
    protected void fakePropertyEnumIntegerSerialize(ServerRequest request, ServerResponse response) { 

        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);

        // Parameter: OuterObjectWithEnumProperty
        OuterObjectWithEnumProperty outerObjectWithEnumProperty = fakePropertyEnumIntegerSerialize.outerObjectWithEnumProperty(request, validator);

        validator.execute();

        handleFakePropertyEnumIntegerSerialize(request, response, 
                    outerObjectWithEnumProperty);
    }

    /**
     * Handle POST /fake/property/enum-int.
     *
     * @param request the server request
     * @param response the server response
     * @param outerObjectWithEnumProperty Input enum (int) as post body 
     */
    protected abstract void handleFakePropertyEnumIntegerSerialize(ServerRequest request, ServerResponse response, 
                OuterObjectWithEnumProperty outerObjectWithEnumProperty);

    /**
     * POST /fake/additionalProperties-reference : test referenced additionalProperties.
     *
     * @param request the server request
     * @param response the server response
     */
    protected void testAdditionalPropertiesReference(ServerRequest request, ServerResponse response) { 

        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);

        // Parameter: request_body
        Map<String, Object> requestBody = testAdditionalPropertiesReference.requestBody(request, validator);

        validator.execute();

        handleTestAdditionalPropertiesReference(request, response, 
                    requestBody);
    }

    /**
     * Handle POST /fake/additionalProperties-reference : test referenced additionalProperties.
     *
     * @param request the server request
     * @param response the server response
     * @param requestBody request body 
     */
    protected abstract void handleTestAdditionalPropertiesReference(ServerRequest request, ServerResponse response, 
                Map<String, Object> requestBody);

    /**
     * PUT /fake/body-with-binary.
     *
     * @param request the server request
     * @param response the server response
     */
    protected void testBodyWithBinary(ServerRequest request, ServerResponse response) { 

        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);

        // Parameter: body
        InputStream body = testBodyWithBinary.body(request, validator);

        validator.execute();

        handleTestBodyWithBinary(request, response, 
                    body);
    }

    /**
     * Handle PUT /fake/body-with-binary.
     *
     * @param request the server request
     * @param response the server response
     * @param body image to upload 
     */
    protected abstract void handleTestBodyWithBinary(ServerRequest request, ServerResponse response, 
                InputStream body);

    /**
     * PUT /fake/body-with-file-schema.
     *
     * @param request the server request
     * @param response the server response
     */
    protected void testBodyWithFileSchema(ServerRequest request, ServerResponse response) { 

        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);

        // Parameter: FileSchemaTestClass
        FileSchemaTestClass fileSchemaTestClass = testBodyWithFileSchema.fileSchemaTestClass(request, validator);

        validator.execute();

        handleTestBodyWithFileSchema(request, response, 
                    fileSchemaTestClass);
    }

    /**
     * Handle PUT /fake/body-with-file-schema.
     *
     * @param request the server request
     * @param response the server response
     * @param fileSchemaTestClass fileSchemaTestClass 
     */
    protected abstract void handleTestBodyWithFileSchema(ServerRequest request, ServerResponse response, 
                FileSchemaTestClass fileSchemaTestClass);

    /**
     * PUT /fake/body-with-query-params.
     *
     * @param request the server request
     * @param response the server response
     */
    protected void testBodyWithQueryParams(ServerRequest request, ServerResponse response) { 

        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);

        // Parameter: query
        String query = testBodyWithQueryParams.query(request, validator);

        validator.require("query", query);

        // Parameter: User
        User user = testBodyWithQueryParams.user(request, validator);

        validator.execute();

        handleTestBodyWithQueryParams(request, response, 
                    query, 
                    user);
    }

    /**
     * Handle PUT /fake/body-with-query-params.
     *
     * @param request the server request
     * @param response the server response
     * @param query query 
     * @param user user 
     */
    protected abstract void handleTestBodyWithQueryParams(ServerRequest request, ServerResponse response, 
                String query, 
                User user);

    /**
     * PATCH /fake : To test \&quot;client\&quot; model.
     *
     * @param request the server request
     * @param response the server response
     */
    protected void testClientModel(ServerRequest request, ServerResponse response) { 

        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);

        // Parameter: Client
        Client client = testClientModel.client(request, validator);

        validator.execute();

        handleTestClientModel(request, response, 
                    client);
    }

    /**
     * Handle PATCH /fake : To test \&quot;client\&quot; model.
     *
     * @param request the server request
     * @param response the server response
     * @param client client model 
     */
    protected abstract void handleTestClientModel(ServerRequest request, ServerResponse response, 
                Client client);

    /**
     * POST /fake : Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 .
     *
     * @param request the server request
     * @param response the server response
     */
    protected void testEndpointParameters(ServerRequest request, ServerResponse response) { 

        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);

        
        Parameters formParams = request.content().as(Parameters.class);

        // Parameter: number
        BigDecimal number = testEndpointParameters.number(request, formParams, validator);

        validator.require("number", number);
        validator.validateMin("number", number, "32.1", true);
        validator.validateMax("number", number, "543.2", true);

        // Parameter: double
        Double _double = testEndpointParameters._double(request, formParams, validator);

        validator.require("_double", _double);
        validator.validateMin("_double", _double, 67.8d, true);
        validator.validateMax("_double", _double, 123.4d, true);

        // Parameter: pattern_without_delimiter
        String patternWithoutDelimiter = testEndpointParameters.patternWithoutDelimiter(request, formParams, validator);

        validator.require("patternWithoutDelimiter", patternWithoutDelimiter);        validator.validatePattern("patternWithoutDelimiter", patternWithoutDelimiter, "^[A-Z].*");

        // Parameter: byte
        byte[] _byte = testEndpointParameters._byte(request, formParams, validator);

        validator.require("_byte", _byte);

        // Parameter: integer
        Optional<Integer> integer = testEndpointParameters.integer(request, formParams, validator);

        validator.validateMin("integer", integer, 10, true);
        validator.validateMax("integer", integer, 100, true);

        // Parameter: int32
        Optional<Integer> int32 = testEndpointParameters.int32(request, formParams, validator);

        validator.validateMin("int32", int32, 20, true);
        validator.validateMax("int32", int32, 200, true);

        // Parameter: int64
        Optional<Long> int64 = testEndpointParameters.int64(request, formParams, validator);


        // Parameter: float
        Optional<Float> _float = testEndpointParameters._float(request, formParams, validator);

        validator.validateMax("_float", _float, 987.6f, true);

        // Parameter: string
        Optional<String> string = testEndpointParameters.string(request, formParams, validator);
        validator.validatePattern("string", string, "/[a-z]/i");

        // Parameter: binary
        Optional<byte[]> binary = testEndpointParameters.binary(request, formParams, validator);


        // Parameter: date
        Optional<LocalDate> date = testEndpointParameters.date(request, formParams, validator);


        // Parameter: dateTime
        Optional<OffsetDateTime> dateTime = testEndpointParameters.dateTime(request, formParams, validator);


        // Parameter: password
        Optional<String> password = testEndpointParameters.password(request, formParams, validator);

        validator.validateSize("password", password, 10, 64);

        // Parameter: callback
        Optional<String> paramCallback = testEndpointParameters.paramCallback(request, formParams, validator);

        validator.execute();

        handleTestEndpointParameters(request, response, 
                    number, 
                    _double, 
                    patternWithoutDelimiter, 
                    _byte, 
                    integer, 
                    int32, 
                    int64, 
                    _float, 
                    string, 
                    binary, 
                    date, 
                    dateTime, 
                    password, 
                    paramCallback);
    }

    /**
     * Handle POST /fake : Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 .
     *
     * @param request the server request
     * @param response the server response
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
     */
    protected abstract void handleTestEndpointParameters(ServerRequest request, ServerResponse response, 
                BigDecimal number, 
                Double _double, 
                String patternWithoutDelimiter, 
                byte[] _byte, 
                Optional<Integer> integer, 
                Optional<Integer> int32, 
                Optional<Long> int64, 
                Optional<Float> _float, 
                Optional<String> string, 
                Optional<byte[]> binary, 
                Optional<LocalDate> date, 
                Optional<OffsetDateTime> dateTime, 
                Optional<String> password, 
                Optional<String> paramCallback);

    /**
     * GET /fake : To test enum parameters.
     *
     * @param request the server request
     * @param response the server response
     */
    protected void testEnumParameters(ServerRequest request, ServerResponse response) { 

        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);

        
        Parameters formParams = request.content().as(Parameters.class);

        // Parameter: enum_header_string_array
        List<String> enumHeaderStringArray = testEnumParameters.enumHeaderStringArray(request, validator);


        // Parameter: enum_header_string
        Optional<String> enumHeaderString = testEnumParameters.enumHeaderString(request, validator);


        // Parameter: enum_query_string_array
        List<String> enumQueryStringArray = testEnumParameters.enumQueryStringArray(request, validator);


        // Parameter: enum_query_string
        Optional<String> enumQueryString = testEnumParameters.enumQueryString(request, validator);


        // Parameter: enum_query_integer
        Optional<Integer> enumQueryInteger = testEnumParameters.enumQueryInteger(request, validator);


        // Parameter: enum_query_double
        Optional<Double> enumQueryDouble = testEnumParameters.enumQueryDouble(request, validator);


        // Parameter: enum_query_model_array
        List<EnumClass> enumQueryModelArray = testEnumParameters.enumQueryModelArray(request, validator);


        // Parameter: enum_form_string_array
        List<String> enumFormStringArray = testEnumParameters.enumFormStringArray(request, formParams, validator);


        // Parameter: enum_form_string
        Optional<String> enumFormString = testEnumParameters.enumFormString(request, formParams, validator);

        validator.execute();

        handleTestEnumParameters(request, response, 
                    enumHeaderStringArray, 
                    enumHeaderString, 
                    enumQueryStringArray, 
                    enumQueryString, 
                    enumQueryInteger, 
                    enumQueryDouble, 
                    enumQueryModelArray, 
                    enumFormStringArray, 
                    enumFormString);
    }

    /**
     * Handle GET /fake : To test enum parameters.
     *
     * @param request the server request
     * @param response the server response
     * @param enumHeaderStringArray Header parameter enum test (string array) 
     * @param enumHeaderString Header parameter enum test (string) 
     * @param enumQueryStringArray Query parameter enum test (string array) 
     * @param enumQueryString Query parameter enum test (string) 
     * @param enumQueryInteger Query parameter enum test (double) 
     * @param enumQueryDouble Query parameter enum test (double) 
     * @param enumQueryModelArray enumQueryModelArray 
     * @param enumFormStringArray Form parameter enum test (string array) 
     * @param enumFormString Form parameter enum test (string) 
     */
    protected abstract void handleTestEnumParameters(ServerRequest request, ServerResponse response, 
                List<String> enumHeaderStringArray, 
                Optional<String> enumHeaderString, 
                List<String> enumQueryStringArray, 
                Optional<String> enumQueryString, 
                Optional<Integer> enumQueryInteger, 
                Optional<Double> enumQueryDouble, 
                List<EnumClass> enumQueryModelArray, 
                List<String> enumFormStringArray, 
                Optional<String> enumFormString);

    /**
     * DELETE /fake : Fake endpoint to test group parameters (optional).
     *
     * @param request the server request
     * @param response the server response
     */
    protected void testGroupParameters(ServerRequest request, ServerResponse response) { 

        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);

        // Parameter: required_string_group
        Integer requiredStringGroup = testGroupParameters.requiredStringGroup(request, validator);

        validator.require("requiredStringGroup", requiredStringGroup);

        // Parameter: required_boolean_group
        Boolean requiredBooleanGroup = testGroupParameters.requiredBooleanGroup(request, validator);

        validator.require("requiredBooleanGroup", requiredBooleanGroup);

        // Parameter: required_int64_group
        Long requiredInt64Group = testGroupParameters.requiredInt64Group(request, validator);

        validator.require("requiredInt64Group", requiredInt64Group);

        // Parameter: string_group
        Optional<Integer> stringGroup = testGroupParameters.stringGroup(request, validator);


        // Parameter: boolean_group
        Optional<Boolean> booleanGroup = testGroupParameters.booleanGroup(request, validator);


        // Parameter: int64_group
        Optional<Long> int64Group = testGroupParameters.int64Group(request, validator);

        validator.execute();

        handleTestGroupParameters(request, response, 
                    requiredStringGroup, 
                    requiredBooleanGroup, 
                    requiredInt64Group, 
                    stringGroup, 
                    booleanGroup, 
                    int64Group);
    }

    /**
     * Handle DELETE /fake : Fake endpoint to test group parameters (optional).
     *
     * @param request the server request
     * @param response the server response
     * @param requiredStringGroup Required String in group parameters 
     * @param requiredBooleanGroup Required Boolean in group parameters 
     * @param requiredInt64Group Required Integer in group parameters 
     * @param stringGroup String in group parameters 
     * @param booleanGroup Boolean in group parameters 
     * @param int64Group Integer in group parameters 
     */
    protected abstract void handleTestGroupParameters(ServerRequest request, ServerResponse response, 
                Integer requiredStringGroup, 
                Boolean requiredBooleanGroup, 
                Long requiredInt64Group, 
                Optional<Integer> stringGroup, 
                Optional<Boolean> booleanGroup, 
                Optional<Long> int64Group);

    /**
     * POST /fake/inline-additionalProperties : test inline additionalProperties.
     *
     * @param request the server request
     * @param response the server response
     */
    protected void testInlineAdditionalProperties(ServerRequest request, ServerResponse response) { 

        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);

        // Parameter: request_body
        Map<String, String> requestBody = testInlineAdditionalProperties.requestBody(request, validator);

        validator.execute();

        handleTestInlineAdditionalProperties(request, response, 
                    requestBody);
    }

    /**
     * Handle POST /fake/inline-additionalProperties : test inline additionalProperties.
     *
     * @param request the server request
     * @param response the server response
     * @param requestBody request body 
     */
    protected abstract void handleTestInlineAdditionalProperties(ServerRequest request, ServerResponse response, 
                Map<String, String> requestBody);

    /**
     * POST /fake/inline-freeform-additionalProperties : test inline free-form additionalProperties.
     *
     * @param request the server request
     * @param response the server response
     */
    protected void testInlineFreeformAdditionalProperties(ServerRequest request, ServerResponse response) { 

        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);

        // Parameter: TestInlineFreeformAdditionalPropertiesRequest
        TestInlineFreeformAdditionalPropertiesRequest testInlineFreeformAdditionalPropertiesRequest = testInlineFreeformAdditionalProperties.testInlineFreeformAdditionalPropertiesRequest(request, validator);

        validator.execute();

        handleTestInlineFreeformAdditionalProperties(request, response, 
                    testInlineFreeformAdditionalPropertiesRequest);
    }

    /**
     * Handle POST /fake/inline-freeform-additionalProperties : test inline free-form additionalProperties.
     *
     * @param request the server request
     * @param response the server response
     * @param testInlineFreeformAdditionalPropertiesRequest request body 
     */
    protected abstract void handleTestInlineFreeformAdditionalProperties(ServerRequest request, ServerResponse response, 
                TestInlineFreeformAdditionalPropertiesRequest testInlineFreeformAdditionalPropertiesRequest);

    /**
     * GET /fake/jsonFormData : test json serialization of form data.
     *
     * @param request the server request
     * @param response the server response
     */
    protected void testJsonFormData(ServerRequest request, ServerResponse response) { 

        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);

        
        Parameters formParams = request.content().as(Parameters.class);

        // Parameter: param
        String param = testJsonFormData.param(request, formParams, validator);

        validator.require("param", param);

        // Parameter: param2
        String param2 = testJsonFormData.param2(request, formParams, validator);

        validator.require("param2", param2);
        validator.execute();

        handleTestJsonFormData(request, response, 
                    param, 
                    param2);
    }

    /**
     * Handle GET /fake/jsonFormData : test json serialization of form data.
     *
     * @param request the server request
     * @param response the server response
     * @param param field1 
     * @param param2 field2 
     */
    protected abstract void handleTestJsonFormData(ServerRequest request, ServerResponse response, 
                String param, 
                String param2);

    /**
     * POST /fake/nullable : test nullable parent property.
     *
     * @param request the server request
     * @param response the server response
     */
    protected void testNullable(ServerRequest request, ServerResponse response) { 

        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);

        // Parameter: ChildWithNullable
        ChildWithNullable childWithNullable = testNullable.childWithNullable(request, validator);

        validator.execute();

        handleTestNullable(request, response, 
                    childWithNullable);
    }

    /**
     * Handle POST /fake/nullable : test nullable parent property.
     *
     * @param request the server request
     * @param response the server response
     * @param childWithNullable request body 
     */
    protected abstract void handleTestNullable(ServerRequest request, ServerResponse response, 
                ChildWithNullable childWithNullable);

    /**
     * PUT /fake/test-query-parameters.
     *
     * @param request the server request
     * @param response the server response
     */
    protected void testQueryParameterCollectionFormat(ServerRequest request, ServerResponse response) { 

        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);

        // Parameter: pipe
        List<String> pipe = testQueryParameterCollectionFormat.pipe(request, validator);

        validator.require("pipe", pipe);

        // Parameter: ioutil
        List<String> ioutil = testQueryParameterCollectionFormat.ioutil(request, validator);

        validator.require("ioutil", ioutil);

        // Parameter: http
        List<String> http = testQueryParameterCollectionFormat.http(request, validator);

        validator.require("http", http);

        // Parameter: url
        List<String> url = testQueryParameterCollectionFormat.url(request, validator);

        validator.require("url", url);

        // Parameter: context
        List<String> context = testQueryParameterCollectionFormat.context(request, validator);

        validator.require("context", context);

        // Parameter: allowEmpty
        String allowEmpty = testQueryParameterCollectionFormat.allowEmpty(request, validator);

        validator.require("allowEmpty", allowEmpty);

        // Parameter: language
        Map<String, String> language = testQueryParameterCollectionFormat.language(request, validator);

        validator.execute();

        handleTestQueryParameterCollectionFormat(request, response, 
                    pipe, 
                    ioutil, 
                    http, 
                    url, 
                    context, 
                    allowEmpty, 
                    language);
    }

    /**
     * Handle PUT /fake/test-query-parameters.
     *
     * @param request the server request
     * @param response the server response
     * @param pipe pipe 
     * @param ioutil ioutil 
     * @param http http 
     * @param url url 
     * @param context context 
     * @param allowEmpty allowEmpty 
     * @param language language 
     */
    protected abstract void handleTestQueryParameterCollectionFormat(ServerRequest request, ServerResponse response, 
                List<String> pipe, 
                List<String> ioutil, 
                List<String> http, 
                List<String> url, 
                List<String> context, 
                String allowEmpty, 
                Map<String, String> language);

    /**
     * POST /fake/stringMap-reference : test referenced string map.
     *
     * @param request the server request
     * @param response the server response
     */
    protected void testStringMapReference(ServerRequest request, ServerResponse response) { 

        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);

        // Parameter: request_body
        Map<String, String> requestBody = testStringMapReference.requestBody(request, validator);

        validator.execute();

        handleTestStringMapReference(request, response, 
                    requestBody);
    }

    /**
     * Handle POST /fake/stringMap-reference : test referenced string map.
     *
     * @param request the server request
     * @param response the server response
     * @param requestBody request body 
     */
    protected abstract void handleTestStringMapReference(ServerRequest request, ServerResponse response, 
                Map<String, String> requestBody);

    /**
     * Returns a new instance of the class which handles parameters to and responses from the fakeBigDecimalMap operation.
     * <p>
     *     Developers can override this method if they extend the FakeService class.
     * </p>
     *
     * @return new FakeBigDecimalMap
     */
    protected FakeBigDecimalMap fakeBigDecimalMap() {
        return new FakeBigDecimalMap();
    }

    /**
     * Helper elements for the fakeBigDecimalMap operation.
     */
    static protected class FakeBigDecimalMap {

        /**
         * Responses for operation {@code fakeBigDecimalMap} organized by response status.
         * <p>
         *     Once your code determines which (if any) response to send it can use the static {@code create} method for that
         *     specific result, passing the required elements of the response as parameters, and then assign any optional
         *     response elements using the record fields or their setter methods.
         * <p>
         *     Finally, your code should invoke the {@code apply} method, passing the original {@link ServerResponse}. The
         *     generated method sets any headers you have assigned, sets the correct status in the response, and sends
         *     the response including any appropriate entity.
         * </p>
         */
        interface Result {

            /**
             * Result for HTTP status code {@code 200}.
            *
             * @param response 
             */
            record S200(FakeBigDecimalMap200Response response)     {

                /**
                 * Creates a result builder for the status {@code 200} result
                 * for the fakeBigDecimalMap operation; there are no required result values for this response.
                 *
                 * @return new builder for status 200
                 */
                static Builder builder() {
                    return new Builder();
                }

                static class Builder implements io.helidon.common.Builder<Builder, S200> {

                    private FakeBigDecimalMap200Response response;

                    @Override
                    public S200 build() {
                        return new S200(response);
                    }

                    void apply(ServerResponse serverResponse) {
                        build().apply(serverResponse);
                    }

                    Builder response(FakeBigDecimalMap200Response response) {
                        this.response = response;
                        return this;
                    }
                }

                /**
                 * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
                 * HTTP status, any response headers, and any response entity.
                 *
                 * @param serverResponse the server response to which to apply these result values
                 * @return the updated server response
                 */
                ServerResponse apply(ServerResponse serverResponse) {
                    serverResponse.status(Status.create(200));
                    if (response != null) { 
                        serverResponse.send(response);
                    } else {
                        serverResponse.send();
                    }
                    return serverResponse;
                }
            }
        }
    }

    /**
     * Returns a new instance of the class which handles parameters to and responses from the fakeHealthGet operation.
     * <p>
     *     Developers can override this method if they extend the FakeService class.
     * </p>
     *
     * @return new FakeHealthGet
     */
    protected FakeHealthGet fakeHealthGet() {
        return new FakeHealthGet();
    }

    /**
     * Helper elements for the fakeHealthGet operation.
     */
    static protected class FakeHealthGet {

        /**
         * Responses for operation {@code fakeHealthGet} organized by response status.
         * <p>
         *     Once your code determines which (if any) response to send it can use the static {@code create} method for that
         *     specific result, passing the required elements of the response as parameters, and then assign any optional
         *     response elements using the record fields or their setter methods.
         * <p>
         *     Finally, your code should invoke the {@code apply} method, passing the original {@link ServerResponse}. The
         *     generated method sets any headers you have assigned, sets the correct status in the response, and sends
         *     the response including any appropriate entity.
         * </p>
         */
        interface Result {

            /**
             * Result for HTTP status code {@code 200}.
            *
             * @param response 
             */
            record S200(HealthCheckResult response)     {

                /**
                 * Creates a result builder for the status {@code 200} result
                 * for the fakeHealthGet operation; there are no required result values for this response.
                 *
                 * @return new builder for status 200
                 */
                static Builder builder() {
                    return new Builder();
                }

                static class Builder implements io.helidon.common.Builder<Builder, S200> {

                    private HealthCheckResult response;

                    @Override
                    public S200 build() {
                        return new S200(response);
                    }

                    void apply(ServerResponse serverResponse) {
                        build().apply(serverResponse);
                    }

                    Builder response(HealthCheckResult response) {
                        this.response = response;
                        return this;
                    }
                }

                /**
                 * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
                 * HTTP status, any response headers, and any response entity.
                 *
                 * @param serverResponse the server response to which to apply these result values
                 * @return the updated server response
                 */
                ServerResponse apply(ServerResponse serverResponse) {
                    serverResponse.status(Status.create(200));
                    if (response != null) { 
                        serverResponse.send(response);
                    } else {
                        serverResponse.send();
                    }
                    return serverResponse;
                }
            }
        }
    }

    /**
     * Returns a new instance of the class which handles parameters to and responses from the fakeHttpSignatureTest operation.
     * <p>
     *     Developers can override this method if they extend the FakeService class.
     * </p>
     *
     * @return new FakeHttpSignatureTest
     */
    protected FakeHttpSignatureTest fakeHttpSignatureTest() {
        return new FakeHttpSignatureTest();
    }

    /**
     * Helper elements for the fakeHttpSignatureTest operation.
     */
    static protected class FakeHttpSignatureTest {

        /**
         * Prepares the pet parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return pet parameter value
         */
        protected Pet pet(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.content().hasEntity()
                ? request.content().as(Pet.class)
                : null;
        }

        /**
         * Prepares the query1 parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return query1 parameter value
         */
        protected Optional<String> query1(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.query()
                .first("query_1")
                .asOptional();
        }

        /**
         * Prepares the header1 parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return header1 parameter value
         */
        protected Optional<String> header1(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.headers()
                .first(HeaderNames.create("header_1"));
        }

        /**
         * Responses for operation {@code fakeHttpSignatureTest} organized by response status.
         * <p>
         *     Once your code determines which (if any) response to send it can use the static {@code create} method for that
         *     specific result, passing the required elements of the response as parameters, and then assign any optional
         *     response elements using the record fields or their setter methods.
         * <p>
         *     Finally, your code should invoke the {@code apply} method, passing the original {@link ServerResponse}. The
         *     generated method sets any headers you have assigned, sets the correct status in the response, and sends
         *     the response including any appropriate entity.
         * </p>
         */
        interface Result {

            /**
             * Result for HTTP status code {@code 200}.
             */
            record S200()     {

                /**
                 * Creates a result builder for the status {@code 200} result
                 * for the fakeHttpSignatureTest operation; there are no required result values for this response.
                 *
                 * @return new builder for status 200
                 */
                static Builder builder() {
                    return new Builder();
                }

                static class Builder implements io.helidon.common.Builder<Builder, S200> {


                    @Override
                    public S200 build() {
                        return new S200();
                    }

                    void apply(ServerResponse serverResponse) {
                        build().apply(serverResponse);
                    }
                }

                /**
                 * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
                 * HTTP status, any response headers, and any response entity.
                 *
                 * @param serverResponse the server response to which to apply these result values
                 * @return the updated server response
                 */
                ServerResponse apply(ServerResponse serverResponse) {
                    serverResponse.status(Status.create(200));
                    serverResponse.send();
                    return serverResponse;
                }
            }
        }
    }

    /**
     * Returns a new instance of the class which handles parameters to and responses from the fakeOuterBooleanSerialize operation.
     * <p>
     *     Developers can override this method if they extend the FakeService class.
     * </p>
     *
     * @return new FakeOuterBooleanSerialize
     */
    protected FakeOuterBooleanSerialize fakeOuterBooleanSerialize() {
        return new FakeOuterBooleanSerialize();
    }

    /**
     * Helper elements for the fakeOuterBooleanSerialize operation.
     */
    static protected class FakeOuterBooleanSerialize {

        /**
         * Prepares the body parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return body parameter value
         */
        protected Optional<Boolean> body(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.content().hasEntity()
                ? Optional.of(request.content().as(Boolean.class))
                : Optional.empty();
        }

        /**
         * Responses for operation {@code fakeOuterBooleanSerialize} organized by response status.
         * <p>
         *     Once your code determines which (if any) response to send it can use the static {@code create} method for that
         *     specific result, passing the required elements of the response as parameters, and then assign any optional
         *     response elements using the record fields or their setter methods.
         * <p>
         *     Finally, your code should invoke the {@code apply} method, passing the original {@link ServerResponse}. The
         *     generated method sets any headers you have assigned, sets the correct status in the response, and sends
         *     the response including any appropriate entity.
         * </p>
         */
        interface Result {

            /**
             * Result for HTTP status code {@code 200}.
            *
             * @param response 
             */
            record S200(Boolean response)     {

                /**
                 * Creates a result builder for the status {@code 200} result
                 * for the fakeOuterBooleanSerialize operation; there are no required result values for this response.
                 *
                 * @return new builder for status 200
                 */
                static Builder builder() {
                    return new Builder();
                }

                static class Builder implements io.helidon.common.Builder<Builder, S200> {

                    private Boolean response;

                    @Override
                    public S200 build() {
                        return new S200(response);
                    }

                    void apply(ServerResponse serverResponse) {
                        build().apply(serverResponse);
                    }

                    Builder response(Boolean response) {
                        this.response = response;
                        return this;
                    }
                }

                /**
                 * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
                 * HTTP status, any response headers, and any response entity.
                 *
                 * @param serverResponse the server response to which to apply these result values
                 * @return the updated server response
                 */
                ServerResponse apply(ServerResponse serverResponse) {
                    serverResponse.status(Status.create(200));
                    if (response != null) { 
                        serverResponse.send(response);
                    } else {
                        serverResponse.send();
                    }
                    return serverResponse;
                }
            }
        }
    }

    /**
     * Returns a new instance of the class which handles parameters to and responses from the fakeOuterCompositeSerialize operation.
     * <p>
     *     Developers can override this method if they extend the FakeService class.
     * </p>
     *
     * @return new FakeOuterCompositeSerialize
     */
    protected FakeOuterCompositeSerialize fakeOuterCompositeSerialize() {
        return new FakeOuterCompositeSerialize();
    }

    /**
     * Helper elements for the fakeOuterCompositeSerialize operation.
     */
    static protected class FakeOuterCompositeSerialize {

        /**
         * Prepares the outerComposite parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return outerComposite parameter value
         */
        protected Optional<OuterComposite> outerComposite(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.content().hasEntity()
                ? Optional.of(request.content().as(OuterComposite.class))
                : Optional.empty();
        }

        /**
         * Responses for operation {@code fakeOuterCompositeSerialize} organized by response status.
         * <p>
         *     Once your code determines which (if any) response to send it can use the static {@code create} method for that
         *     specific result, passing the required elements of the response as parameters, and then assign any optional
         *     response elements using the record fields or their setter methods.
         * <p>
         *     Finally, your code should invoke the {@code apply} method, passing the original {@link ServerResponse}. The
         *     generated method sets any headers you have assigned, sets the correct status in the response, and sends
         *     the response including any appropriate entity.
         * </p>
         */
        interface Result {

            /**
             * Result for HTTP status code {@code 200}.
            *
             * @param response 
             */
            record S200(OuterComposite response)     {

                /**
                 * Creates a result builder for the status {@code 200} result
                 * for the fakeOuterCompositeSerialize operation; there are no required result values for this response.
                 *
                 * @return new builder for status 200
                 */
                static Builder builder() {
                    return new Builder();
                }

                static class Builder implements io.helidon.common.Builder<Builder, S200> {

                    private OuterComposite response;

                    @Override
                    public S200 build() {
                        return new S200(response);
                    }

                    void apply(ServerResponse serverResponse) {
                        build().apply(serverResponse);
                    }

                    Builder response(OuterComposite response) {
                        this.response = response;
                        return this;
                    }
                }

                /**
                 * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
                 * HTTP status, any response headers, and any response entity.
                 *
                 * @param serverResponse the server response to which to apply these result values
                 * @return the updated server response
                 */
                ServerResponse apply(ServerResponse serverResponse) {
                    serverResponse.status(Status.create(200));
                    if (response != null) { 
                        serverResponse.send(response);
                    } else {
                        serverResponse.send();
                    }
                    return serverResponse;
                }
            }
        }
    }

    /**
     * Returns a new instance of the class which handles parameters to and responses from the fakeOuterNumberSerialize operation.
     * <p>
     *     Developers can override this method if they extend the FakeService class.
     * </p>
     *
     * @return new FakeOuterNumberSerialize
     */
    protected FakeOuterNumberSerialize fakeOuterNumberSerialize() {
        return new FakeOuterNumberSerialize();
    }

    /**
     * Helper elements for the fakeOuterNumberSerialize operation.
     */
    static protected class FakeOuterNumberSerialize {

        /**
         * Prepares the body parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return body parameter value
         */
        protected Optional<BigDecimal> body(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.content().hasEntity()
                ? Optional.of(request.content().as(BigDecimal.class))
                : Optional.empty();
        }

        /**
         * Responses for operation {@code fakeOuterNumberSerialize} organized by response status.
         * <p>
         *     Once your code determines which (if any) response to send it can use the static {@code create} method for that
         *     specific result, passing the required elements of the response as parameters, and then assign any optional
         *     response elements using the record fields or their setter methods.
         * <p>
         *     Finally, your code should invoke the {@code apply} method, passing the original {@link ServerResponse}. The
         *     generated method sets any headers you have assigned, sets the correct status in the response, and sends
         *     the response including any appropriate entity.
         * </p>
         */
        interface Result {

            /**
             * Result for HTTP status code {@code 200}.
            *
             * @param response 
             */
            record S200(BigDecimal response)     {

                /**
                 * Creates a result builder for the status {@code 200} result
                 * for the fakeOuterNumberSerialize operation; there are no required result values for this response.
                 *
                 * @return new builder for status 200
                 */
                static Builder builder() {
                    return new Builder();
                }

                static class Builder implements io.helidon.common.Builder<Builder, S200> {

                    private BigDecimal response;

                    @Override
                    public S200 build() {
                        return new S200(response);
                    }

                    void apply(ServerResponse serverResponse) {
                        build().apply(serverResponse);
                    }

                    Builder response(BigDecimal response) {
                        this.response = response;
                        return this;
                    }
                }

                /**
                 * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
                 * HTTP status, any response headers, and any response entity.
                 *
                 * @param serverResponse the server response to which to apply these result values
                 * @return the updated server response
                 */
                ServerResponse apply(ServerResponse serverResponse) {
                    serverResponse.status(Status.create(200));
                    if (response != null) { 
                        serverResponse.send(response);
                    } else {
                        serverResponse.send();
                    }
                    return serverResponse;
                }
            }
        }
    }

    /**
     * Returns a new instance of the class which handles parameters to and responses from the fakeOuterStringSerialize operation.
     * <p>
     *     Developers can override this method if they extend the FakeService class.
     * </p>
     *
     * @return new FakeOuterStringSerialize
     */
    protected FakeOuterStringSerialize fakeOuterStringSerialize() {
        return new FakeOuterStringSerialize();
    }

    /**
     * Helper elements for the fakeOuterStringSerialize operation.
     */
    static protected class FakeOuterStringSerialize {

        /**
         * Prepares the body parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return body parameter value
         */
        protected Optional<String> body(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.content().hasEntity()
                ? Optional.of(request.content().as(String.class))
                : Optional.empty();
        }

        /**
         * Responses for operation {@code fakeOuterStringSerialize} organized by response status.
         * <p>
         *     Once your code determines which (if any) response to send it can use the static {@code create} method for that
         *     specific result, passing the required elements of the response as parameters, and then assign any optional
         *     response elements using the record fields or their setter methods.
         * <p>
         *     Finally, your code should invoke the {@code apply} method, passing the original {@link ServerResponse}. The
         *     generated method sets any headers you have assigned, sets the correct status in the response, and sends
         *     the response including any appropriate entity.
         * </p>
         */
        interface Result {

            /**
             * Result for HTTP status code {@code 200}.
            *
             * @param response 
             */
            record S200(String response)     {

                /**
                 * Creates a result builder for the status {@code 200} result
                 * for the fakeOuterStringSerialize operation; there are no required result values for this response.
                 *
                 * @return new builder for status 200
                 */
                static Builder builder() {
                    return new Builder();
                }

                static class Builder implements io.helidon.common.Builder<Builder, S200> {

                    private String response;

                    @Override
                    public S200 build() {
                        return new S200(response);
                    }

                    void apply(ServerResponse serverResponse) {
                        build().apply(serverResponse);
                    }

                    Builder response(String response) {
                        this.response = response;
                        return this;
                    }
                }

                /**
                 * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
                 * HTTP status, any response headers, and any response entity.
                 *
                 * @param serverResponse the server response to which to apply these result values
                 * @return the updated server response
                 */
                ServerResponse apply(ServerResponse serverResponse) {
                    serverResponse.status(Status.create(200));
                    if (response != null) { 
                        serverResponse.send(response);
                    } else {
                        serverResponse.send();
                    }
                    return serverResponse;
                }
            }
        }
    }

    /**
     * Returns a new instance of the class which handles parameters to and responses from the fakePropertyEnumIntegerSerialize operation.
     * <p>
     *     Developers can override this method if they extend the FakeService class.
     * </p>
     *
     * @return new FakePropertyEnumIntegerSerialize
     */
    protected FakePropertyEnumIntegerSerialize fakePropertyEnumIntegerSerialize() {
        return new FakePropertyEnumIntegerSerialize();
    }

    /**
     * Helper elements for the fakePropertyEnumIntegerSerialize operation.
     */
    static protected class FakePropertyEnumIntegerSerialize {

        /**
         * Prepares the outerObjectWithEnumProperty parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return outerObjectWithEnumProperty parameter value
         */
        protected OuterObjectWithEnumProperty outerObjectWithEnumProperty(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.content().hasEntity()
                ? request.content().as(OuterObjectWithEnumProperty.class)
                : null;
        }

        /**
         * Responses for operation {@code fakePropertyEnumIntegerSerialize} organized by response status.
         * <p>
         *     Once your code determines which (if any) response to send it can use the static {@code create} method for that
         *     specific result, passing the required elements of the response as parameters, and then assign any optional
         *     response elements using the record fields or their setter methods.
         * <p>
         *     Finally, your code should invoke the {@code apply} method, passing the original {@link ServerResponse}. The
         *     generated method sets any headers you have assigned, sets the correct status in the response, and sends
         *     the response including any appropriate entity.
         * </p>
         */
        interface Result {

            /**
             * Result for HTTP status code {@code 200}.
            *
             * @param response 
             */
            record S200(OuterObjectWithEnumProperty response)     {

                /**
                 * Creates a result builder for the status {@code 200} result
                 * for the fakePropertyEnumIntegerSerialize operation; there are no required result values for this response.
                 *
                 * @return new builder for status 200
                 */
                static Builder builder() {
                    return new Builder();
                }

                static class Builder implements io.helidon.common.Builder<Builder, S200> {

                    private OuterObjectWithEnumProperty response;

                    @Override
                    public S200 build() {
                        return new S200(response);
                    }

                    void apply(ServerResponse serverResponse) {
                        build().apply(serverResponse);
                    }

                    Builder response(OuterObjectWithEnumProperty response) {
                        this.response = response;
                        return this;
                    }
                }

                /**
                 * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
                 * HTTP status, any response headers, and any response entity.
                 *
                 * @param serverResponse the server response to which to apply these result values
                 * @return the updated server response
                 */
                ServerResponse apply(ServerResponse serverResponse) {
                    serverResponse.status(Status.create(200));
                    if (response != null) { 
                        serverResponse.send(response);
                    } else {
                        serverResponse.send();
                    }
                    return serverResponse;
                }
            }
        }
    }

    /**
     * Returns a new instance of the class which handles parameters to and responses from the testAdditionalPropertiesReference operation.
     * <p>
     *     Developers can override this method if they extend the FakeService class.
     * </p>
     *
     * @return new TestAdditionalPropertiesReference
     */
    protected TestAdditionalPropertiesReference testAdditionalPropertiesReference() {
        return new TestAdditionalPropertiesReference();
    }

    /**
     * Helper elements for the testAdditionalPropertiesReference operation.
     */
    static protected class TestAdditionalPropertiesReference {

        /**
         * Prepares the requestBody parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return requestBody parameter value
         */
        protected Map<String, Object> requestBody(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.content().hasEntity()
                ? request.content().as(GenericTypes.TYPE__Map_Object)
                : Map.of();
        }

        /**
         * Responses for operation {@code testAdditionalPropertiesReference} organized by response status.
         * <p>
         *     Once your code determines which (if any) response to send it can use the static {@code create} method for that
         *     specific result, passing the required elements of the response as parameters, and then assign any optional
         *     response elements using the record fields or their setter methods.
         * <p>
         *     Finally, your code should invoke the {@code apply} method, passing the original {@link ServerResponse}. The
         *     generated method sets any headers you have assigned, sets the correct status in the response, and sends
         *     the response including any appropriate entity.
         * </p>
         */
        interface Result {

            /**
             * Result for HTTP status code {@code 200}.
             */
            record S200()     {

                /**
                 * Creates a result builder for the status {@code 200} result
                 * for the testAdditionalPropertiesReference operation; there are no required result values for this response.
                 *
                 * @return new builder for status 200
                 */
                static Builder builder() {
                    return new Builder();
                }

                static class Builder implements io.helidon.common.Builder<Builder, S200> {


                    @Override
                    public S200 build() {
                        return new S200();
                    }

                    void apply(ServerResponse serverResponse) {
                        build().apply(serverResponse);
                    }
                }

                /**
                 * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
                 * HTTP status, any response headers, and any response entity.
                 *
                 * @param serverResponse the server response to which to apply these result values
                 * @return the updated server response
                 */
                ServerResponse apply(ServerResponse serverResponse) {
                    serverResponse.status(Status.create(200));
                    serverResponse.send();
                    return serverResponse;
                }
            }
        }
    }

    /**
     * Returns a new instance of the class which handles parameters to and responses from the testBodyWithBinary operation.
     * <p>
     *     Developers can override this method if they extend the FakeService class.
     * </p>
     *
     * @return new TestBodyWithBinary
     */
    protected TestBodyWithBinary testBodyWithBinary() {
        return new TestBodyWithBinary();
    }

    /**
     * Helper elements for the testBodyWithBinary operation.
     */
    static protected class TestBodyWithBinary {

        /**
         * Prepares the body parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return body parameter value
         */
        protected InputStream body(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.content().hasEntity()
                ? request.content().inputStream()
                : null;
        }

        /**
         * Responses for operation {@code testBodyWithBinary} organized by response status.
         * <p>
         *     Once your code determines which (if any) response to send it can use the static {@code create} method for that
         *     specific result, passing the required elements of the response as parameters, and then assign any optional
         *     response elements using the record fields or their setter methods.
         * <p>
         *     Finally, your code should invoke the {@code apply} method, passing the original {@link ServerResponse}. The
         *     generated method sets any headers you have assigned, sets the correct status in the response, and sends
         *     the response including any appropriate entity.
         * </p>
         */
        interface Result {

            /**
             * Result for HTTP status code {@code 200}.
             */
            record S200()     {

                /**
                 * Creates a result builder for the status {@code 200} result
                 * for the testBodyWithBinary operation; there are no required result values for this response.
                 *
                 * @return new builder for status 200
                 */
                static Builder builder() {
                    return new Builder();
                }

                static class Builder implements io.helidon.common.Builder<Builder, S200> {


                    @Override
                    public S200 build() {
                        return new S200();
                    }

                    void apply(ServerResponse serverResponse) {
                        build().apply(serverResponse);
                    }
                }

                /**
                 * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
                 * HTTP status, any response headers, and any response entity.
                 *
                 * @param serverResponse the server response to which to apply these result values
                 * @return the updated server response
                 */
                ServerResponse apply(ServerResponse serverResponse) {
                    serverResponse.status(Status.create(200));
                    serverResponse.send();
                    return serverResponse;
                }
            }
        }
    }

    /**
     * Returns a new instance of the class which handles parameters to and responses from the testBodyWithFileSchema operation.
     * <p>
     *     Developers can override this method if they extend the FakeService class.
     * </p>
     *
     * @return new TestBodyWithFileSchema
     */
    protected TestBodyWithFileSchema testBodyWithFileSchema() {
        return new TestBodyWithFileSchema();
    }

    /**
     * Helper elements for the testBodyWithFileSchema operation.
     */
    static protected class TestBodyWithFileSchema {

        /**
         * Prepares the fileSchemaTestClass parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return fileSchemaTestClass parameter value
         */
        protected FileSchemaTestClass fileSchemaTestClass(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.content().hasEntity()
                ? request.content().as(FileSchemaTestClass.class)
                : null;
        }

        /**
         * Responses for operation {@code testBodyWithFileSchema} organized by response status.
         * <p>
         *     Once your code determines which (if any) response to send it can use the static {@code create} method for that
         *     specific result, passing the required elements of the response as parameters, and then assign any optional
         *     response elements using the record fields or their setter methods.
         * <p>
         *     Finally, your code should invoke the {@code apply} method, passing the original {@link ServerResponse}. The
         *     generated method sets any headers you have assigned, sets the correct status in the response, and sends
         *     the response including any appropriate entity.
         * </p>
         */
        interface Result {

            /**
             * Result for HTTP status code {@code 200}.
             */
            record S200()     {

                /**
                 * Creates a result builder for the status {@code 200} result
                 * for the testBodyWithFileSchema operation; there are no required result values for this response.
                 *
                 * @return new builder for status 200
                 */
                static Builder builder() {
                    return new Builder();
                }

                static class Builder implements io.helidon.common.Builder<Builder, S200> {


                    @Override
                    public S200 build() {
                        return new S200();
                    }

                    void apply(ServerResponse serverResponse) {
                        build().apply(serverResponse);
                    }
                }

                /**
                 * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
                 * HTTP status, any response headers, and any response entity.
                 *
                 * @param serverResponse the server response to which to apply these result values
                 * @return the updated server response
                 */
                ServerResponse apply(ServerResponse serverResponse) {
                    serverResponse.status(Status.create(200));
                    serverResponse.send();
                    return serverResponse;
                }
            }
        }
    }

    /**
     * Returns a new instance of the class which handles parameters to and responses from the testBodyWithQueryParams operation.
     * <p>
     *     Developers can override this method if they extend the FakeService class.
     * </p>
     *
     * @return new TestBodyWithQueryParams
     */
    protected TestBodyWithQueryParams testBodyWithQueryParams() {
        return new TestBodyWithQueryParams();
    }

    /**
     * Helper elements for the testBodyWithQueryParams operation.
     */
    static protected class TestBodyWithQueryParams {

        /**
         * Prepares the query parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return query parameter value
         */
        protected String query(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.query()
                .first("query")
                .asOptional()                .orElse(null);
        }

        /**
         * Prepares the user parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return user parameter value
         */
        protected User user(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.content().hasEntity()
                ? request.content().as(User.class)
                : null;
        }

        /**
         * Responses for operation {@code testBodyWithQueryParams} organized by response status.
         * <p>
         *     Once your code determines which (if any) response to send it can use the static {@code create} method for that
         *     specific result, passing the required elements of the response as parameters, and then assign any optional
         *     response elements using the record fields or their setter methods.
         * <p>
         *     Finally, your code should invoke the {@code apply} method, passing the original {@link ServerResponse}. The
         *     generated method sets any headers you have assigned, sets the correct status in the response, and sends
         *     the response including any appropriate entity.
         * </p>
         */
        interface Result {

            /**
             * Result for HTTP status code {@code 200}.
             */
            record S200()     {

                /**
                 * Creates a result builder for the status {@code 200} result
                 * for the testBodyWithQueryParams operation; there are no required result values for this response.
                 *
                 * @return new builder for status 200
                 */
                static Builder builder() {
                    return new Builder();
                }

                static class Builder implements io.helidon.common.Builder<Builder, S200> {


                    @Override
                    public S200 build() {
                        return new S200();
                    }

                    void apply(ServerResponse serverResponse) {
                        build().apply(serverResponse);
                    }
                }

                /**
                 * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
                 * HTTP status, any response headers, and any response entity.
                 *
                 * @param serverResponse the server response to which to apply these result values
                 * @return the updated server response
                 */
                ServerResponse apply(ServerResponse serverResponse) {
                    serverResponse.status(Status.create(200));
                    serverResponse.send();
                    return serverResponse;
                }
            }
        }
    }

    /**
     * Returns a new instance of the class which handles parameters to and responses from the testClientModel operation.
     * <p>
     *     Developers can override this method if they extend the FakeService class.
     * </p>
     *
     * @return new TestClientModel
     */
    protected TestClientModel testClientModel() {
        return new TestClientModel();
    }

    /**
     * Helper elements for the testClientModel operation.
     */
    static protected class TestClientModel {

        /**
         * Prepares the client parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return client parameter value
         */
        protected Client client(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.content().hasEntity()
                ? request.content().as(Client.class)
                : null;
        }

        /**
         * Responses for operation {@code testClientModel} organized by response status.
         * <p>
         *     Once your code determines which (if any) response to send it can use the static {@code create} method for that
         *     specific result, passing the required elements of the response as parameters, and then assign any optional
         *     response elements using the record fields or their setter methods.
         * <p>
         *     Finally, your code should invoke the {@code apply} method, passing the original {@link ServerResponse}. The
         *     generated method sets any headers you have assigned, sets the correct status in the response, and sends
         *     the response including any appropriate entity.
         * </p>
         */
        interface Result {

            /**
             * Result for HTTP status code {@code 200}.
            *
             * @param response 
             */
            record S200(Client response)     {

                /**
                 * Creates a result builder for the status {@code 200} result
                 * for the testClientModel operation; there are no required result values for this response.
                 *
                 * @return new builder for status 200
                 */
                static Builder builder() {
                    return new Builder();
                }

                static class Builder implements io.helidon.common.Builder<Builder, S200> {

                    private Client response;

                    @Override
                    public S200 build() {
                        return new S200(response);
                    }

                    void apply(ServerResponse serverResponse) {
                        build().apply(serverResponse);
                    }

                    Builder response(Client response) {
                        this.response = response;
                        return this;
                    }
                }

                /**
                 * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
                 * HTTP status, any response headers, and any response entity.
                 *
                 * @param serverResponse the server response to which to apply these result values
                 * @return the updated server response
                 */
                ServerResponse apply(ServerResponse serverResponse) {
                    serverResponse.status(Status.create(200));
                    if (response != null) { 
                        serverResponse.send(response);
                    } else {
                        serverResponse.send();
                    }
                    return serverResponse;
                }
            }
        }
    }

    /**
     * Returns a new instance of the class which handles parameters to and responses from the testEndpointParameters operation.
     * <p>
     *     Developers can override this method if they extend the FakeService class.
     * </p>
     *
     * @return new TestEndpointParameters
     */
    protected TestEndpointParameters testEndpointParameters() {
        return new TestEndpointParameters();
    }

    /**
     * Helper elements for the testEndpointParameters operation.
     */
    static protected class TestEndpointParameters {

        /**
         * Prepares the number parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return number parameter value
         */
        protected BigDecimal number(ServerRequest request, Parameters formParams, ValidatorUtils.Validator validator) {
            return formParams
                .first("number")
                .asOptional()
                .map(BigDecimal::new)                .orElse(null);
        }

        /**
         * Prepares the _double parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return _double parameter value
         */
        protected Double _double(ServerRequest request, Parameters formParams, ValidatorUtils.Validator validator) {
            return formParams
                .first("double")
                .asOptional()
                .map(Double::valueOf)                .orElse(null);
        }

        /**
         * Prepares the patternWithoutDelimiter parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return patternWithoutDelimiter parameter value
         */
        protected String patternWithoutDelimiter(ServerRequest request, Parameters formParams, ValidatorUtils.Validator validator) {
            return formParams
                .first("pattern_without_delimiter")
                .asOptional()                .orElse(null);
        }

        /**
         * Prepares the _byte parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return _byte parameter value
         */
        protected byte[] _byte(ServerRequest request, Parameters formParams, ValidatorUtils.Validator validator) {
            return formParams
                .first("byte")
                .asOptional()
                .map(HexFormat.of()::parseHex)                .orElse(null);
        }

        /**
         * Prepares the integer parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return integer parameter value
         */
        protected Optional<Integer> integer(ServerRequest request, Parameters formParams, ValidatorUtils.Validator validator) {
            return formParams
                .first("integer")
                .asOptional()
                .map(Integer::valueOf);
        }

        /**
         * Prepares the int32 parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return int32 parameter value
         */
        protected Optional<Integer> int32(ServerRequest request, Parameters formParams, ValidatorUtils.Validator validator) {
            return formParams
                .first("int32")
                .asOptional()
                .map(Integer::valueOf);
        }

        /**
         * Prepares the int64 parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return int64 parameter value
         */
        protected Optional<Long> int64(ServerRequest request, Parameters formParams, ValidatorUtils.Validator validator) {
            return formParams
                .first("int64")
                .asOptional()
                .map(Long::valueOf);
        }

        /**
         * Prepares the _float parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return _float parameter value
         */
        protected Optional<Float> _float(ServerRequest request, Parameters formParams, ValidatorUtils.Validator validator) {
            return formParams
                .first("float")
                .asOptional()
                .map(Float::valueOf);
        }

        /**
         * Prepares the string parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return string parameter value
         */
        protected Optional<String> string(ServerRequest request, Parameters formParams, ValidatorUtils.Validator validator) {
            return formParams
                .first("string")
                .asOptional();
        }

        /**
         * Prepares the binary parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return binary parameter value
         */
        protected Optional<byte[]> binary(ServerRequest request, Parameters formParams, ValidatorUtils.Validator validator) {
            return formParams
                .first("binary")
                .asOptional()
                .map(HCollectors::decodeBinaryFormParam);
        }

        /**
         * Prepares the date parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return date parameter value
         */
        protected Optional<LocalDate> date(ServerRequest request, Parameters formParams, ValidatorUtils.Validator validator) {
            return formParams
                .first("date")
                .asOptional()
                .map(LocalDate::parse);
        }

        /**
         * Prepares the dateTime parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return dateTime parameter value
         */
        protected Optional<OffsetDateTime> dateTime(ServerRequest request, Parameters formParams, ValidatorUtils.Validator validator) {
            return formParams
                .first("dateTime")
                .asOptional()
                .map(OffsetDateTime::parse);
        }

        /**
         * Prepares the password parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return password parameter value
         */
        protected Optional<String> password(ServerRequest request, Parameters formParams, ValidatorUtils.Validator validator) {
            return formParams
                .first("password")
                .asOptional();
        }

        /**
         * Prepares the paramCallback parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return paramCallback parameter value
         */
        protected Optional<String> paramCallback(ServerRequest request, Parameters formParams, ValidatorUtils.Validator validator) {
            return formParams
                .first("callback")
                .asOptional();
        }

        /**
         * Responses for operation {@code testEndpointParameters} organized by response status.
         * <p>
         *     Once your code determines which (if any) response to send it can use the static {@code create} method for that
         *     specific result, passing the required elements of the response as parameters, and then assign any optional
         *     response elements using the record fields or their setter methods.
         * <p>
         *     Finally, your code should invoke the {@code apply} method, passing the original {@link ServerResponse}. The
         *     generated method sets any headers you have assigned, sets the correct status in the response, and sends
         *     the response including any appropriate entity.
         * </p>
         */
        interface Result {

            /**
             * Result for HTTP status code {@code 400}.
             */
            record S400()     {

                /**
                 * Creates a result builder for the status {@code 400} result
                 * for the testEndpointParameters operation; there are no required result values for this response.
                 *
                 * @return new builder for status 400
                 */
                static Builder builder() {
                    return new Builder();
                }

                static class Builder implements io.helidon.common.Builder<Builder, S400> {


                    @Override
                    public S400 build() {
                        return new S400();
                    }

                    void apply(ServerResponse serverResponse) {
                        build().apply(serverResponse);
                    }
                }

                /**
                 * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
                 * HTTP status, any response headers, and any response entity.
                 *
                 * @param serverResponse the server response to which to apply these result values
                 * @return the updated server response
                 */
                ServerResponse apply(ServerResponse serverResponse) {
                    serverResponse.status(Status.create(400));
                    serverResponse.send();
                    return serverResponse;
                }
            }

            /**
             * Result for HTTP status code {@code 404}.
             */
            record S404()     {

                /**
                 * Creates a result builder for the status {@code 404} result
                 * for the testEndpointParameters operation; there are no required result values for this response.
                 *
                 * @return new builder for status 404
                 */
                static Builder builder() {
                    return new Builder();
                }

                static class Builder implements io.helidon.common.Builder<Builder, S404> {


                    @Override
                    public S404 build() {
                        return new S404();
                    }

                    void apply(ServerResponse serverResponse) {
                        build().apply(serverResponse);
                    }
                }

                /**
                 * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
                 * HTTP status, any response headers, and any response entity.
                 *
                 * @param serverResponse the server response to which to apply these result values
                 * @return the updated server response
                 */
                ServerResponse apply(ServerResponse serverResponse) {
                    serverResponse.status(Status.create(404));
                    serverResponse.send();
                    return serverResponse;
                }
            }
        }
    }

    /**
     * Returns a new instance of the class which handles parameters to and responses from the testEnumParameters operation.
     * <p>
     *     Developers can override this method if they extend the FakeService class.
     * </p>
     *
     * @return new TestEnumParameters
     */
    protected TestEnumParameters testEnumParameters() {
        return new TestEnumParameters();
    }

    /**
     * Helper elements for the testEnumParameters operation.
     */
    static protected class TestEnumParameters {

        /**
         * Prepares the enumHeaderStringArray parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return enumHeaderStringArray parameter value
         */
        protected List<String> enumHeaderStringArray(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.headers()
                .values(HeaderNames.create("enum_header_string_array"))
                .stream()
                .map(String::valueOf)
                .map(v -> validator.check("enum_header_string_array",
                     v,
                     List.of(">",
                             "$")))
                .collect(Collectors.toList());
        }

        /**
         * Prepares the enumHeaderString parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return enumHeaderString parameter value
         */
        protected Optional<String> enumHeaderString(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.headers()
                .first(HeaderNames.create("enum_header_string"))
                .or(() -> Optional.of("-efg"))
                .map(v -> validator.check("enum_header_string",
                     v,
                     List.of("_abc",
                             "-efg",
                             "(xyz)")));
        }

        /**
         * Prepares the enumQueryStringArray parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return enumQueryStringArray parameter value
         */
        protected List<String> enumQueryStringArray(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.query()
                .all("enum_query_string_array")
                .stream()
                .map(String::valueOf)
                .map(v -> validator.check("enum_query_string_array",
                     v,
                     List.of(">",
                             "$")))
                .collect(Collectors.toList());
        }

        /**
         * Prepares the enumQueryString parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return enumQueryString parameter value
         */
        protected Optional<String> enumQueryString(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.query()
                .first("enum_query_string")
                .asOptional()
                .or(() -> Optional.of("-efg"))
                .map(v -> validator.check("enum_query_string",
                     v,
                     List.of("_abc",
                             "-efg",
                             "(xyz)")));
        }

        /**
         * Prepares the enumQueryInteger parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return enumQueryInteger parameter value
         */
        protected Optional<Integer> enumQueryInteger(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.query()
                .first("enum_query_integer")
                .asOptional()
                .map(Integer::valueOf)
                .map(v -> validator.check("enum_query_integer",
                     v,
                     List.of(1,
                             -2)));
        }

        /**
         * Prepares the enumQueryDouble parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return enumQueryDouble parameter value
         */
        protected Optional<Double> enumQueryDouble(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.query()
                .first("enum_query_double")
                .asOptional()
                .map(Double::valueOf)
                .map(v -> validator.check("enum_query_double",
                     v,
                     List.of(1.1,
                             -1.2)));
        }

        /**
         * Prepares the enumQueryModelArray parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return enumQueryModelArray parameter value
         */
        protected List<EnumClass> enumQueryModelArray(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.query()
                .all("enum_query_model_array")
                .stream()
                .map(EnumClass::fromValue)
                .collect(Collectors.toList());
        }

        /**
         * Prepares the enumFormStringArray parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return enumFormStringArray parameter value
         */
        protected List<String> enumFormStringArray(ServerRequest request, Parameters formParams, ValidatorUtils.Validator validator) {
            return formParams
                .all("enum_form_string_array")
                .stream()
                .map(String::valueOf)
                .map(v -> validator.check("enum_form_string_array",
                     v,
                     List.of(">",
                             "$")))
                .collect(HCollectors.toDefaultedList("$",
                                                    String::valueOf));
        }

        /**
         * Prepares the enumFormString parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return enumFormString parameter value
         */
        protected Optional<String> enumFormString(ServerRequest request, Parameters formParams, ValidatorUtils.Validator validator) {
            return formParams
                .first("enum_form_string")
                .asOptional()
                .or(() -> Optional.of("-efg"))
                .map(v -> validator.check("enum_form_string",
                     v,
                     List.of("_abc",
                             "-efg",
                             "(xyz)")));
        }

        /**
         * Responses for operation {@code testEnumParameters} organized by response status.
         * <p>
         *     Once your code determines which (if any) response to send it can use the static {@code create} method for that
         *     specific result, passing the required elements of the response as parameters, and then assign any optional
         *     response elements using the record fields or their setter methods.
         * <p>
         *     Finally, your code should invoke the {@code apply} method, passing the original {@link ServerResponse}. The
         *     generated method sets any headers you have assigned, sets the correct status in the response, and sends
         *     the response including any appropriate entity.
         * </p>
         */
        interface Result {

            /**
             * Result for HTTP status code {@code 400}.
             */
            record S400()     {

                /**
                 * Creates a result builder for the status {@code 400} result
                 * for the testEnumParameters operation; there are no required result values for this response.
                 *
                 * @return new builder for status 400
                 */
                static Builder builder() {
                    return new Builder();
                }

                static class Builder implements io.helidon.common.Builder<Builder, S400> {


                    @Override
                    public S400 build() {
                        return new S400();
                    }

                    void apply(ServerResponse serverResponse) {
                        build().apply(serverResponse);
                    }
                }

                /**
                 * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
                 * HTTP status, any response headers, and any response entity.
                 *
                 * @param serverResponse the server response to which to apply these result values
                 * @return the updated server response
                 */
                ServerResponse apply(ServerResponse serverResponse) {
                    serverResponse.status(Status.create(400));
                    serverResponse.send();
                    return serverResponse;
                }
            }

            /**
             * Result for HTTP status code {@code 404}.
             */
            record S404()     {

                /**
                 * Creates a result builder for the status {@code 404} result
                 * for the testEnumParameters operation; there are no required result values for this response.
                 *
                 * @return new builder for status 404
                 */
                static Builder builder() {
                    return new Builder();
                }

                static class Builder implements io.helidon.common.Builder<Builder, S404> {


                    @Override
                    public S404 build() {
                        return new S404();
                    }

                    void apply(ServerResponse serverResponse) {
                        build().apply(serverResponse);
                    }
                }

                /**
                 * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
                 * HTTP status, any response headers, and any response entity.
                 *
                 * @param serverResponse the server response to which to apply these result values
                 * @return the updated server response
                 */
                ServerResponse apply(ServerResponse serverResponse) {
                    serverResponse.status(Status.create(404));
                    serverResponse.send();
                    return serverResponse;
                }
            }
        }
    }

    /**
     * Returns a new instance of the class which handles parameters to and responses from the testGroupParameters operation.
     * <p>
     *     Developers can override this method if they extend the FakeService class.
     * </p>
     *
     * @return new TestGroupParameters
     */
    protected TestGroupParameters testGroupParameters() {
        return new TestGroupParameters();
    }

    /**
     * Helper elements for the testGroupParameters operation.
     */
    static protected class TestGroupParameters {

        /**
         * Prepares the requiredStringGroup parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return requiredStringGroup parameter value
         */
        protected Integer requiredStringGroup(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.query()
                .first("required_string_group")
                .asOptional()
                .map(Integer::valueOf)                .orElse(null);
        }

        /**
         * Prepares the requiredBooleanGroup parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return requiredBooleanGroup parameter value
         */
        protected Boolean requiredBooleanGroup(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.headers()
                .first(HeaderNames.create("required_boolean_group"))
                .map(Boolean::valueOf)                .orElse(null);
        }

        /**
         * Prepares the requiredInt64Group parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return requiredInt64Group parameter value
         */
        protected Long requiredInt64Group(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.query()
                .first("required_int64_group")
                .asOptional()
                .map(Long::valueOf)                .orElse(null);
        }

        /**
         * Prepares the stringGroup parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return stringGroup parameter value
         */
        protected Optional<Integer> stringGroup(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.query()
                .first("string_group")
                .asOptional()
                .map(Integer::valueOf);
        }

        /**
         * Prepares the booleanGroup parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return booleanGroup parameter value
         */
        protected Optional<Boolean> booleanGroup(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.headers()
                .first(HeaderNames.create("boolean_group"))
                .map(Boolean::valueOf);
        }

        /**
         * Prepares the int64Group parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return int64Group parameter value
         */
        protected Optional<Long> int64Group(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.query()
                .first("int64_group")
                .asOptional()
                .map(Long::valueOf);
        }

        /**
         * Responses for operation {@code testGroupParameters} organized by response status.
         * <p>
         *     Once your code determines which (if any) response to send it can use the static {@code create} method for that
         *     specific result, passing the required elements of the response as parameters, and then assign any optional
         *     response elements using the record fields or their setter methods.
         * <p>
         *     Finally, your code should invoke the {@code apply} method, passing the original {@link ServerResponse}. The
         *     generated method sets any headers you have assigned, sets the correct status in the response, and sends
         *     the response including any appropriate entity.
         * </p>
         */
        interface Result {

            /**
             * Result for HTTP status code {@code 400}.
             */
            record S400()     {

                /**
                 * Creates a result builder for the status {@code 400} result
                 * for the testGroupParameters operation; there are no required result values for this response.
                 *
                 * @return new builder for status 400
                 */
                static Builder builder() {
                    return new Builder();
                }

                static class Builder implements io.helidon.common.Builder<Builder, S400> {


                    @Override
                    public S400 build() {
                        return new S400();
                    }

                    void apply(ServerResponse serverResponse) {
                        build().apply(serverResponse);
                    }
                }

                /**
                 * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
                 * HTTP status, any response headers, and any response entity.
                 *
                 * @param serverResponse the server response to which to apply these result values
                 * @return the updated server response
                 */
                ServerResponse apply(ServerResponse serverResponse) {
                    serverResponse.status(Status.create(400));
                    serverResponse.send();
                    return serverResponse;
                }
            }
        }
    }

    /**
     * Returns a new instance of the class which handles parameters to and responses from the testInlineAdditionalProperties operation.
     * <p>
     *     Developers can override this method if they extend the FakeService class.
     * </p>
     *
     * @return new TestInlineAdditionalProperties
     */
    protected TestInlineAdditionalProperties testInlineAdditionalProperties() {
        return new TestInlineAdditionalProperties();
    }

    /**
     * Helper elements for the testInlineAdditionalProperties operation.
     */
    static protected class TestInlineAdditionalProperties {

        /**
         * Prepares the requestBody parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return requestBody parameter value
         */
        protected Map<String, String> requestBody(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.content().hasEntity()
                ? request.content().as(GenericTypes.TYPE__Map_String)
                : Map.of();
        }

        /**
         * Responses for operation {@code testInlineAdditionalProperties} organized by response status.
         * <p>
         *     Once your code determines which (if any) response to send it can use the static {@code create} method for that
         *     specific result, passing the required elements of the response as parameters, and then assign any optional
         *     response elements using the record fields or their setter methods.
         * <p>
         *     Finally, your code should invoke the {@code apply} method, passing the original {@link ServerResponse}. The
         *     generated method sets any headers you have assigned, sets the correct status in the response, and sends
         *     the response including any appropriate entity.
         * </p>
         */
        interface Result {

            /**
             * Result for HTTP status code {@code 200}.
             */
            record S200()     {

                /**
                 * Creates a result builder for the status {@code 200} result
                 * for the testInlineAdditionalProperties operation; there are no required result values for this response.
                 *
                 * @return new builder for status 200
                 */
                static Builder builder() {
                    return new Builder();
                }

                static class Builder implements io.helidon.common.Builder<Builder, S200> {


                    @Override
                    public S200 build() {
                        return new S200();
                    }

                    void apply(ServerResponse serverResponse) {
                        build().apply(serverResponse);
                    }
                }

                /**
                 * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
                 * HTTP status, any response headers, and any response entity.
                 *
                 * @param serverResponse the server response to which to apply these result values
                 * @return the updated server response
                 */
                ServerResponse apply(ServerResponse serverResponse) {
                    serverResponse.status(Status.create(200));
                    serverResponse.send();
                    return serverResponse;
                }
            }
        }
    }

    /**
     * Returns a new instance of the class which handles parameters to and responses from the testInlineFreeformAdditionalProperties operation.
     * <p>
     *     Developers can override this method if they extend the FakeService class.
     * </p>
     *
     * @return new TestInlineFreeformAdditionalProperties
     */
    protected TestInlineFreeformAdditionalProperties testInlineFreeformAdditionalProperties() {
        return new TestInlineFreeformAdditionalProperties();
    }

    /**
     * Helper elements for the testInlineFreeformAdditionalProperties operation.
     */
    static protected class TestInlineFreeformAdditionalProperties {

        /**
         * Prepares the testInlineFreeformAdditionalPropertiesRequest parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return testInlineFreeformAdditionalPropertiesRequest parameter value
         */
        protected TestInlineFreeformAdditionalPropertiesRequest testInlineFreeformAdditionalPropertiesRequest(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.content().hasEntity()
                ? request.content().as(TestInlineFreeformAdditionalPropertiesRequest.class)
                : null;
        }

        /**
         * Responses for operation {@code testInlineFreeformAdditionalProperties} organized by response status.
         * <p>
         *     Once your code determines which (if any) response to send it can use the static {@code create} method for that
         *     specific result, passing the required elements of the response as parameters, and then assign any optional
         *     response elements using the record fields or their setter methods.
         * <p>
         *     Finally, your code should invoke the {@code apply} method, passing the original {@link ServerResponse}. The
         *     generated method sets any headers you have assigned, sets the correct status in the response, and sends
         *     the response including any appropriate entity.
         * </p>
         */
        interface Result {

            /**
             * Result for HTTP status code {@code 200}.
             */
            record S200()     {

                /**
                 * Creates a result builder for the status {@code 200} result
                 * for the testInlineFreeformAdditionalProperties operation; there are no required result values for this response.
                 *
                 * @return new builder for status 200
                 */
                static Builder builder() {
                    return new Builder();
                }

                static class Builder implements io.helidon.common.Builder<Builder, S200> {


                    @Override
                    public S200 build() {
                        return new S200();
                    }

                    void apply(ServerResponse serverResponse) {
                        build().apply(serverResponse);
                    }
                }

                /**
                 * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
                 * HTTP status, any response headers, and any response entity.
                 *
                 * @param serverResponse the server response to which to apply these result values
                 * @return the updated server response
                 */
                ServerResponse apply(ServerResponse serverResponse) {
                    serverResponse.status(Status.create(200));
                    serverResponse.send();
                    return serverResponse;
                }
            }
        }
    }

    /**
     * Returns a new instance of the class which handles parameters to and responses from the testJsonFormData operation.
     * <p>
     *     Developers can override this method if they extend the FakeService class.
     * </p>
     *
     * @return new TestJsonFormData
     */
    protected TestJsonFormData testJsonFormData() {
        return new TestJsonFormData();
    }

    /**
     * Helper elements for the testJsonFormData operation.
     */
    static protected class TestJsonFormData {

        /**
         * Prepares the param parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return param parameter value
         */
        protected String param(ServerRequest request, Parameters formParams, ValidatorUtils.Validator validator) {
            return formParams
                .first("param")
                .asOptional()                .orElse(null);
        }

        /**
         * Prepares the param2 parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return param2 parameter value
         */
        protected String param2(ServerRequest request, Parameters formParams, ValidatorUtils.Validator validator) {
            return formParams
                .first("param2")
                .asOptional()                .orElse(null);
        }

        /**
         * Responses for operation {@code testJsonFormData} organized by response status.
         * <p>
         *     Once your code determines which (if any) response to send it can use the static {@code create} method for that
         *     specific result, passing the required elements of the response as parameters, and then assign any optional
         *     response elements using the record fields or their setter methods.
         * <p>
         *     Finally, your code should invoke the {@code apply} method, passing the original {@link ServerResponse}. The
         *     generated method sets any headers you have assigned, sets the correct status in the response, and sends
         *     the response including any appropriate entity.
         * </p>
         */
        interface Result {

            /**
             * Result for HTTP status code {@code 200}.
             */
            record S200()     {

                /**
                 * Creates a result builder for the status {@code 200} result
                 * for the testJsonFormData operation; there are no required result values for this response.
                 *
                 * @return new builder for status 200
                 */
                static Builder builder() {
                    return new Builder();
                }

                static class Builder implements io.helidon.common.Builder<Builder, S200> {


                    @Override
                    public S200 build() {
                        return new S200();
                    }

                    void apply(ServerResponse serverResponse) {
                        build().apply(serverResponse);
                    }
                }

                /**
                 * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
                 * HTTP status, any response headers, and any response entity.
                 *
                 * @param serverResponse the server response to which to apply these result values
                 * @return the updated server response
                 */
                ServerResponse apply(ServerResponse serverResponse) {
                    serverResponse.status(Status.create(200));
                    serverResponse.send();
                    return serverResponse;
                }
            }
        }
    }

    /**
     * Returns a new instance of the class which handles parameters to and responses from the testNullable operation.
     * <p>
     *     Developers can override this method if they extend the FakeService class.
     * </p>
     *
     * @return new TestNullable
     */
    protected TestNullable testNullable() {
        return new TestNullable();
    }

    /**
     * Helper elements for the testNullable operation.
     */
    static protected class TestNullable {

        /**
         * Prepares the childWithNullable parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return childWithNullable parameter value
         */
        protected ChildWithNullable childWithNullable(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.content().hasEntity()
                ? request.content().as(ChildWithNullable.class)
                : null;
        }

        /**
         * Responses for operation {@code testNullable} organized by response status.
         * <p>
         *     Once your code determines which (if any) response to send it can use the static {@code create} method for that
         *     specific result, passing the required elements of the response as parameters, and then assign any optional
         *     response elements using the record fields or their setter methods.
         * <p>
         *     Finally, your code should invoke the {@code apply} method, passing the original {@link ServerResponse}. The
         *     generated method sets any headers you have assigned, sets the correct status in the response, and sends
         *     the response including any appropriate entity.
         * </p>
         */
        interface Result {

            /**
             * Result for HTTP status code {@code 200}.
             */
            record S200()     {

                /**
                 * Creates a result builder for the status {@code 200} result
                 * for the testNullable operation; there are no required result values for this response.
                 *
                 * @return new builder for status 200
                 */
                static Builder builder() {
                    return new Builder();
                }

                static class Builder implements io.helidon.common.Builder<Builder, S200> {


                    @Override
                    public S200 build() {
                        return new S200();
                    }

                    void apply(ServerResponse serverResponse) {
                        build().apply(serverResponse);
                    }
                }

                /**
                 * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
                 * HTTP status, any response headers, and any response entity.
                 *
                 * @param serverResponse the server response to which to apply these result values
                 * @return the updated server response
                 */
                ServerResponse apply(ServerResponse serverResponse) {
                    serverResponse.status(Status.create(200));
                    serverResponse.send();
                    return serverResponse;
                }
            }
        }
    }

    /**
     * Returns a new instance of the class which handles parameters to and responses from the testQueryParameterCollectionFormat operation.
     * <p>
     *     Developers can override this method if they extend the FakeService class.
     * </p>
     *
     * @return new TestQueryParameterCollectionFormat
     */
    protected TestQueryParameterCollectionFormat testQueryParameterCollectionFormat() {
        return new TestQueryParameterCollectionFormat();
    }

    /**
     * Helper elements for the testQueryParameterCollectionFormat operation.
     */
    static protected class TestQueryParameterCollectionFormat {

        /**
         * Prepares the pipe parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return pipe parameter value
         */
        protected List<String> pipe(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.query()
                .all("pipe")
                .stream()
                .map(String::valueOf)
                .collect(Collectors.toList());
        }

        /**
         * Prepares the ioutil parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return ioutil parameter value
         */
        protected List<String> ioutil(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.query()
                .all("ioutil")
                .stream()
                .map(String::valueOf)
                .collect(Collectors.toList());
        }

        /**
         * Prepares the http parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return http parameter value
         */
        protected List<String> http(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.query()
                .all("http")
                .stream()
                .map(String::valueOf)
                .collect(Collectors.toList());
        }

        /**
         * Prepares the url parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return url parameter value
         */
        protected List<String> url(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.query()
                .all("url")
                .stream()
                .map(String::valueOf)
                .collect(Collectors.toList());
        }

        /**
         * Prepares the context parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return context parameter value
         */
        protected List<String> context(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.query()
                .all("context")
                .stream()
                .map(String::valueOf)
                .collect(Collectors.toList());
        }

        /**
         * Prepares the allowEmpty parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return allowEmpty parameter value
         */
        protected String allowEmpty(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.query()
                .first("allowEmpty")
                .asOptional()                .orElse(null);
        }

        /**
         * Prepares the language parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return language parameter value
         */
        protected Map<String, String> language(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.query()
                .all("language")
                .stream()
                .map(String::valueOf)
                // TODO - Developer must override this method and provide the correct mapping.
                .collect(HCollectors.noOpMap());
        }

        /**
         * Responses for operation {@code testQueryParameterCollectionFormat} organized by response status.
         * <p>
         *     Once your code determines which (if any) response to send it can use the static {@code create} method for that
         *     specific result, passing the required elements of the response as parameters, and then assign any optional
         *     response elements using the record fields or their setter methods.
         * <p>
         *     Finally, your code should invoke the {@code apply} method, passing the original {@link ServerResponse}. The
         *     generated method sets any headers you have assigned, sets the correct status in the response, and sends
         *     the response including any appropriate entity.
         * </p>
         */
        interface Result {

            /**
             * Result for HTTP status code {@code 200}.
             */
            record S200()     {

                /**
                 * Creates a result builder for the status {@code 200} result
                 * for the testQueryParameterCollectionFormat operation; there are no required result values for this response.
                 *
                 * @return new builder for status 200
                 */
                static Builder builder() {
                    return new Builder();
                }

                static class Builder implements io.helidon.common.Builder<Builder, S200> {


                    @Override
                    public S200 build() {
                        return new S200();
                    }

                    void apply(ServerResponse serverResponse) {
                        build().apply(serverResponse);
                    }
                }

                /**
                 * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
                 * HTTP status, any response headers, and any response entity.
                 *
                 * @param serverResponse the server response to which to apply these result values
                 * @return the updated server response
                 */
                ServerResponse apply(ServerResponse serverResponse) {
                    serverResponse.status(Status.create(200));
                    serverResponse.send();
                    return serverResponse;
                }
            }
        }
    }

    /**
     * Returns a new instance of the class which handles parameters to and responses from the testStringMapReference operation.
     * <p>
     *     Developers can override this method if they extend the FakeService class.
     * </p>
     *
     * @return new TestStringMapReference
     */
    protected TestStringMapReference testStringMapReference() {
        return new TestStringMapReference();
    }

    /**
     * Helper elements for the testStringMapReference operation.
     */
    static protected class TestStringMapReference {

        /**
         * Prepares the requestBody parameter.
         *
         * @param request {@link io.helidon.webserver.http.ServerRequest} containing the parameter
         * @param validator {@link org.openapitools.server.api.ValidatorUtils.Validator} for validating all parameters to the operation
         * @return requestBody parameter value
         */
        protected Map<String, String> requestBody(ServerRequest request, ValidatorUtils.Validator validator) {
            return request.content().hasEntity()
                ? request.content().as(GenericTypes.TYPE__Map_String)
                : Map.of();
        }

        /**
         * Responses for operation {@code testStringMapReference} organized by response status.
         * <p>
         *     Once your code determines which (if any) response to send it can use the static {@code create} method for that
         *     specific result, passing the required elements of the response as parameters, and then assign any optional
         *     response elements using the record fields or their setter methods.
         * <p>
         *     Finally, your code should invoke the {@code apply} method, passing the original {@link ServerResponse}. The
         *     generated method sets any headers you have assigned, sets the correct status in the response, and sends
         *     the response including any appropriate entity.
         * </p>
         */
        interface Result {

            /**
             * Result for HTTP status code {@code 200}.
             */
            record S200()     {

                /**
                 * Creates a result builder for the status {@code 200} result
                 * for the testStringMapReference operation; there are no required result values for this response.
                 *
                 * @return new builder for status 200
                 */
                static Builder builder() {
                    return new Builder();
                }

                static class Builder implements io.helidon.common.Builder<Builder, S200> {


                    @Override
                    public S200 build() {
                        return new S200();
                    }

                    void apply(ServerResponse serverResponse) {
                        build().apply(serverResponse);
                    }
                }

                /**
                 * Applies this result data to the specified {@link io.helidon.webserver.http.ServerResponse}, assigning the
                 * HTTP status, any response headers, and any response entity.
                 *
                 * @param serverResponse the server response to which to apply these result values
                 * @return the updated server response
                 */
                ServerResponse apply(ServerResponse serverResponse) {
                    serverResponse.status(Status.create(200));
                    serverResponse.send();
                    return serverResponse;
                }
            }
        }
    }


    @Override
    public void afterStop() {
    System.out.println("Service FakeService is down. Goodbye!");
    }


}
