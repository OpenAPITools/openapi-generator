package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import java.math.BigDecimal;
import org.openapitools.client.model.Client;
import java.io.File;
import org.openapitools.client.model.FileSchemaTestClass;
import org.openapitools.client.model.HealthCheckResult;
import org.threeten.bp.LocalDate;
import org.threeten.bp.OffsetDateTime;
import org.openapitools.client.model.OuterComposite;
import org.openapitools.client.model.OuterObjectWithEnumProperty;
import org.openapitools.client.model.Pet;
import org.openapitools.client.model.User;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * API tests for FakeApi
 */
class FakeApiTest {

    private FakeApi api;

    @BeforeEach
    public void setup() {
        api = new ApiClient().buildClient(FakeApi.class);
    }

    
    /**
     * Health check endpoint
     *
     * 
     */
    @Test
    void fakeHealthGetTest() {
        // HealthCheckResult response = api.fakeHealthGet();

        // TODO: test validations
    }

    
    /**
     * test http signature authentication
     *
     * 
     */
    @Test
    void fakeHttpSignatureTestTest() {
        Pet pet = null;
        String query1 = null;
        String header1 = null;
        // api.fakeHttpSignatureTest(pet, query1, header1);

        // TODO: test validations
    }

    /**
     * test http signature authentication
     *
     * 
     *
     * This tests the overload of the method that uses a Map for query parameters instead of
     * listing them out individually.
     */
    @Test
    void fakeHttpSignatureTestTestQueryMap() {
        Pet pet = null;
        String header1 = null;
        FakeApi.FakeHttpSignatureTestQueryParams queryParams = new FakeApi.FakeHttpSignatureTestQueryParams()
            .query1(null);
        // api.fakeHttpSignatureTest(pet, header1, queryParams);

    // TODO: test validations
    }
    
    /**
     * 
     *
     * Test serialization of outer boolean types
     */
    @Test
    void fakeOuterBooleanSerializeTest() {
        Boolean body = null;
        // Boolean response = api.fakeOuterBooleanSerialize(body);

        // TODO: test validations
    }

    
    /**
     * 
     *
     * Test serialization of object with outer number type
     */
    @Test
    void fakeOuterCompositeSerializeTest() {
        OuterComposite outerComposite = null;
        // OuterComposite response = api.fakeOuterCompositeSerialize(outerComposite);

        // TODO: test validations
    }

    
    /**
     * 
     *
     * Test serialization of outer number types
     */
    @Test
    void fakeOuterNumberSerializeTest() {
        BigDecimal body = null;
        // BigDecimal response = api.fakeOuterNumberSerialize(body);

        // TODO: test validations
    }

    
    /**
     * 
     *
     * Test serialization of outer string types
     */
    @Test
    void fakeOuterStringSerializeTest() {
        String body = null;
        // String response = api.fakeOuterStringSerialize(body);

        // TODO: test validations
    }

    
    /**
     * 
     *
     * Test serialization of enum (int) properties with examples
     */
    @Test
    void fakePropertyEnumIntegerSerializeTest() {
        OuterObjectWithEnumProperty outerObjectWithEnumProperty = null;
        // OuterObjectWithEnumProperty response = api.fakePropertyEnumIntegerSerialize(outerObjectWithEnumProperty);

        // TODO: test validations
    }

    
    /**
     * 
     *
     * For this test, the body has to be a binary file.
     */
    @Test
    void testBodyWithBinaryTest() {
        File body = null;
        // api.testBodyWithBinary(body);

        // TODO: test validations
    }

    
    /**
     * 
     *
     * For this test, the body for this request must reference a schema named &#x60;File&#x60;.
     */
    @Test
    void testBodyWithFileSchemaTest() {
        FileSchemaTestClass fileSchemaTestClass = null;
        // api.testBodyWithFileSchema(fileSchemaTestClass);

        // TODO: test validations
    }

    
    /**
     * 
     *
     * 
     */
    @Test
    void testBodyWithQueryParamsTest() {
        String query = null;
        User user = null;
        // api.testBodyWithQueryParams(query, user);

        // TODO: test validations
    }

    /**
     * 
     *
     * 
     *
     * This tests the overload of the method that uses a Map for query parameters instead of
     * listing them out individually.
     */
    @Test
    void testBodyWithQueryParamsTestQueryMap() {
        User user = null;
        FakeApi.TestBodyWithQueryParamsQueryParams queryParams = new FakeApi.TestBodyWithQueryParamsQueryParams()
            .query(null);
        // api.testBodyWithQueryParams(user, queryParams);

    // TODO: test validations
    }
    
    /**
     * To test \&quot;client\&quot; model
     *
     * To test \&quot;client\&quot; model
     */
    @Test
    void testClientModelTest() {
        Client client = null;
        // Client response = api.testClientModel(client);

        // TODO: test validations
    }

    
    /**
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     *
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     */
    @Test
    void testEndpointParametersTest() {
        BigDecimal number = null;
        Double _double = null;
        String patternWithoutDelimiter = null;
        byte[] _byte = null;
        Integer integer = null;
        Integer int32 = null;
        Long int64 = null;
        Float _float = null;
        String string = null;
        File binary = null;
        LocalDate date = null;
        OffsetDateTime dateTime = null;
        String password = null;
        String paramCallback = null;
        // api.testEndpointParameters(number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, string, binary, date, dateTime, password, paramCallback);

        // TODO: test validations
    }

    
    /**
     * To test enum parameters
     *
     * To test enum parameters
     */
    @Test
    void testEnumParametersTest() {
        List<String> enumHeaderStringArray = null;
        String enumHeaderString = null;
        List<String> enumQueryStringArray = null;
        String enumQueryString = null;
        Integer enumQueryInteger = null;
        Double enumQueryDouble = null;
        List<String> enumFormStringArray = null;
        String enumFormString = null;
        // api.testEnumParameters(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumFormStringArray, enumFormString);

        // TODO: test validations
    }

    /**
     * To test enum parameters
     *
     * To test enum parameters
     *
     * This tests the overload of the method that uses a Map for query parameters instead of
     * listing them out individually.
     */
    @Test
    void testEnumParametersTestQueryMap() {
        List<String> enumHeaderStringArray = null;
        String enumHeaderString = null;
        List<String> enumFormStringArray = null;
        String enumFormString = null;
        FakeApi.TestEnumParametersQueryParams queryParams = new FakeApi.TestEnumParametersQueryParams()
            .enumQueryStringArray(null)
            .enumQueryString(null)
            .enumQueryInteger(null)
            .enumQueryDouble(null);
        // api.testEnumParameters(enumHeaderStringArray, enumHeaderString, enumFormStringArray, enumFormString, queryParams);

    // TODO: test validations
    }
    
    /**
     * Fake endpoint to test group parameters (optional)
     *
     * Fake endpoint to test group parameters (optional)
     */
    @Test
    void testGroupParametersTest() {
        Integer requiredStringGroup = null;
        Boolean requiredBooleanGroup = null;
        Long requiredInt64Group = null;
        Integer stringGroup = null;
        Boolean booleanGroup = null;
        Long int64Group = null;
        // api.testGroupParameters(requiredStringGroup, requiredBooleanGroup, requiredInt64Group, stringGroup, booleanGroup, int64Group);

        // TODO: test validations
    }

    /**
     * Fake endpoint to test group parameters (optional)
     *
     * Fake endpoint to test group parameters (optional)
     *
     * This tests the overload of the method that uses a Map for query parameters instead of
     * listing them out individually.
     */
    @Test
    void testGroupParametersTestQueryMap() {
        Boolean requiredBooleanGroup = null;
        Boolean booleanGroup = null;
        FakeApi.TestGroupParametersQueryParams queryParams = new FakeApi.TestGroupParametersQueryParams()
            .requiredStringGroup(null)
            .requiredInt64Group(null)
            .stringGroup(null)
            .int64Group(null);
        // api.testGroupParameters(requiredBooleanGroup, booleanGroup, queryParams);

    // TODO: test validations
    }
    
    /**
     * test inline additionalProperties
     *
     * 
     */
    @Test
    void testInlineAdditionalPropertiesTest() {
        Map<String, String> requestBody = null;
        // api.testInlineAdditionalProperties(requestBody);

        // TODO: test validations
    }

    
    /**
     * test json serialization of form data
     *
     * 
     */
    @Test
    void testJsonFormDataTest() {
        String param = null;
        String param2 = null;
        // api.testJsonFormData(param, param2);

        // TODO: test validations
    }

    
    /**
     * 
     *
     * To test the collection format in query parameters
     */
    @Test
    void testQueryParameterCollectionFormatTest() {
        List<String> pipe = null;
        List<String> ioutil = null;
        List<String> http = null;
        List<String> url = null;
        List<String> context = null;
        // api.testQueryParameterCollectionFormat(pipe, ioutil, http, url, context);

        // TODO: test validations
    }

    /**
     * 
     *
     * To test the collection format in query parameters
     *
     * This tests the overload of the method that uses a Map for query parameters instead of
     * listing them out individually.
     */
    @Test
    void testQueryParameterCollectionFormatTestQueryMap() {
        FakeApi.TestQueryParameterCollectionFormatQueryParams queryParams = new FakeApi.TestQueryParameterCollectionFormatQueryParams()
            .pipe(null)
            .ioutil(null)
            .http(null)
            .url(null)
            .context(null);
        // api.testQueryParameterCollectionFormat(queryParams);

    // TODO: test validations
    }
    
}
