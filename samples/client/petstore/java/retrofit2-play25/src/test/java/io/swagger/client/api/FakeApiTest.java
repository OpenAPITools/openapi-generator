package io.swagger.client.api;

import io.swagger.client.ApiClient;
import java.math.BigDecimal;
import io.swagger.client.model.Client;
import org.joda.time.DateTime;
import org.joda.time.LocalDate;
import io.swagger.client.model.OuterComposite;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * API tests for FakeApi
 */
public class FakeApiTest {

    private FakeApi api;

    @Before
    public void setup() {
        api = new ApiClient().createService(FakeApi.class);
    }

    
    /**
     * 
     *
     * Test serialization of outer boolean types
     */
    @Test
    public void fakeOuterBooleanSerializeTest() {
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
    public void fakeOuterCompositeSerializeTest() {
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
    public void fakeOuterNumberSerializeTest() {
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
    public void fakeOuterStringSerializeTest() {
        String body = null;
        // String response = api.fakeOuterStringSerialize(body);

        // TODO: test validations
    }
    
    /**
     * To test \&quot;client\&quot; model
     *
     * To test \&quot;client\&quot; model
     */
    @Test
    public void testClientModelTest() {
        Client body = null;
        // Client response = api.testClientModel(body);

        // TODO: test validations
    }
    
    /**
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     *
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     */
    @Test
    public void testEndpointParametersTest() {
        BigDecimal number = null;
        Double _double = null;
        String patternWithoutDelimiter = null;
        byte[] _byte = null;
        Integer integer = null;
        Integer int32 = null;
        Long int64 = null;
        Float _float = null;
        String string = null;
        byte[] binary = null;
        LocalDate date = null;
        DateTime dateTime = null;
        String password = null;
        String paramCallback = null;
        // Void response = api.testEndpointParameters(number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, string, binary, date, dateTime, password, paramCallback);

        // TODO: test validations
    }
    
    /**
     * To test enum parameters
     *
     * To test enum parameters
     */
    @Test
    public void testEnumParametersTest() {
        List<String> enumFormStringArray = null;
        String enumFormString = null;
        List<String> enumHeaderStringArray = null;
        String enumHeaderString = null;
        List<String> enumQueryStringArray = null;
        String enumQueryString = null;
        Integer enumQueryInteger = null;
        Double enumQueryDouble = null;
        // Void response = api.testEnumParameters(enumFormStringArray, enumFormString, enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble);

        // TODO: test validations
    }
    
}
