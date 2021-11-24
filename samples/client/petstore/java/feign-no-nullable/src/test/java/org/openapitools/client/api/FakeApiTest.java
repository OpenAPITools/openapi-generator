package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import java.math.BigDecimal;
import org.openapitools.client.model.Client;
import java.io.File;
import org.openapitools.client.model.FileSchemaTestClass;
import org.threeten.bp.LocalDate;
import org.threeten.bp.OffsetDateTime;
import org.openapitools.client.model.OuterComposite;
import org.openapitools.client.model.User;
import org.openapitools.client.model.XmlItem;
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
     * creates an XmlItem
     *
     * this route creates an XmlItem
     */
    @Test
    void createXmlItemTest() {
        XmlItem xmlItem = null;
        // api.createXmlItem(xmlItem);

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
        OuterComposite body = null;
        // OuterComposite response = api.fakeOuterCompositeSerialize(body);

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
     * For this test, the body for this request much reference a schema named &#x60;File&#x60;.
     */
    @Test
    void testBodyWithFileSchemaTest() {
        FileSchemaTestClass body = null;
        // api.testBodyWithFileSchema(body);

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
        User body = null;
        // api.testBodyWithQueryParams(query, body);

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
        User body = null;
        FakeApi.TestBodyWithQueryParamsQueryParams queryParams = new FakeApi.TestBodyWithQueryParamsQueryParams()
            .query(null);
        // api.testBodyWithQueryParams(body, queryParams);

    // TODO: test validations
    }
    
    /**
     * To test \&quot;client\&quot; model
     *
     * To test \&quot;client\&quot; model
     */
    @Test
    void testClientModelTest() {
        Client body = null;
        // Client response = api.testClientModel(body);

        // TODO: test validations
    }

    
    /**
     * Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
     *
     * Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
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
        Map<String, String> param = null;
        // api.testInlineAdditionalProperties(param);

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
