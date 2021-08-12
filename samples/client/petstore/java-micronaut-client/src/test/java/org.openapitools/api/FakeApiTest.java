package org.openapitools.api;

import java.math.BigDecimal;
import java.io.File;
import org.openapitools.model.FileSchemaTestClass;
import java.time.LocalDate;
import java.time.LocalDateTime;
import org.openapitools.model.ModelClient;
import org.openapitools.model.OuterComposite;
import org.openapitools.model.User;
import org.openapitools.model.XmlItem;
import io.micronaut.test.extensions.junit5.annotation.MicronautTest;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;
import jakarta.inject.Inject;
import reactor.core.publisher.Mono;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


/**
 * API tests for FakeApi
 */
@MicronautTest
public class FakeApiTest {

    @Inject
    FakeApi api;

    
    /**
     * creates an XmlItem
     *
     * this route creates an XmlItem
     */
    @Test
    public void createXmlItemTest() {
        XmlItem xmlItem = null;
        // api.createXmlItem(xmlItem).block();
        // Mono<Void> asyncResponse = api.createXmlItem(xmlItem);
        // TODO: test validations
    }

    
    /**
     * 
     *
     * Test serialization of outer boolean types
     */
    @Test
    public void fakeOuterBooleanSerializeTest() {
        Boolean _body = null;
        // Boolean response = api.fakeOuterBooleanSerialize(_body).block();
        // Mono<Boolean> asyncResponse = api.fakeOuterBooleanSerialize(_body);
        // TODO: test validations
    }

    
    /**
     * 
     *
     * Test serialization of object with outer number type
     */
    @Test
    public void fakeOuterCompositeSerializeTest() {
        OuterComposite _body = null;
        // OuterComposite response = api.fakeOuterCompositeSerialize(_body).block();
        // Mono<OuterComposite> asyncResponse = api.fakeOuterCompositeSerialize(_body);
        // TODO: test validations
    }

    
    /**
     * 
     *
     * Test serialization of outer number types
     */
    @Test
    public void fakeOuterNumberSerializeTest() {
        BigDecimal _body = null;
        // BigDecimal response = api.fakeOuterNumberSerialize(_body).block();
        // Mono<BigDecimal> asyncResponse = api.fakeOuterNumberSerialize(_body);
        // TODO: test validations
    }

    
    /**
     * 
     *
     * Test serialization of outer string types
     */
    @Test
    public void fakeOuterStringSerializeTest() {
        String _body = null;
        // String response = api.fakeOuterStringSerialize(_body).block();
        // Mono<String> asyncResponse = api.fakeOuterStringSerialize(_body);
        // TODO: test validations
    }

    
    /**
     * 
     *
     * For this test, the body for this request much reference a schema named &#x60;File&#x60;.
     */
    @Test
    public void testBodyWithFileSchemaTest() {
        FileSchemaTestClass _body = null;
        // api.testBodyWithFileSchema(_body).block();
        // Mono<Void> asyncResponse = api.testBodyWithFileSchema(_body);
        // TODO: test validations
    }

    
    /**
     * 
     */
    @Test
    public void testBodyWithQueryParamsTest() {
        String query = null;
        User _body = null;
        // api.testBodyWithQueryParams(query, _body).block();
        // Mono<Void> asyncResponse = api.testBodyWithQueryParams(query, _body);
        // TODO: test validations
    }

    
    /**
     * To test \&quot;client\&quot; model
     *
     * To test \&quot;client\&quot; model
     */
    @Test
    public void testClientModelTest() {
        ModelClient _body = null;
        // ModelClient response = api.testClientModel(_body).block();
        // Mono<ModelClient> asyncResponse = api.testClientModel(_body);
        // TODO: test validations
    }

    
    /**
     * Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
     *
     * Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
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
        File binary = null;
        LocalDate date = null;
        LocalDateTime dateTime = null;
        String password = null;
        String paramCallback = null;
        // api.testEndpointParameters(number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, string, binary, date, dateTime, password, paramCallback).block();
        // Mono<Void> asyncResponse = api.testEndpointParameters(number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, string, binary, date, dateTime, password, paramCallback);
        // TODO: test validations
    }

    
    /**
     * To test enum parameters
     *
     * To test enum parameters
     */
    @Test
    public void testEnumParametersTest() {
        List<String> enumHeaderStringArray = null;
        String enumHeaderString = null;
        List<String> enumQueryStringArray = null;
        String enumQueryString = null;
        Integer enumQueryInteger = null;
        Double enumQueryDouble = null;
        List<String> enumFormStringArray = null;
        String enumFormString = null;
        // api.testEnumParameters(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumFormStringArray, enumFormString).block();
        // Mono<Void> asyncResponse = api.testEnumParameters(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumFormStringArray, enumFormString);
        // TODO: test validations
    }

    
    /**
     * Fake endpoint to test group parameters (optional)
     *
     * Fake endpoint to test group parameters (optional)
     */
    @Test
    public void testGroupParametersTest() {
        Integer requiredStringGroup = null;
        Boolean requiredBooleanGroup = null;
        Long requiredInt64Group = null;
        Integer stringGroup = null;
        Boolean booleanGroup = null;
        Long int64Group = null;
        // api.testGroupParameters(requiredStringGroup, requiredBooleanGroup, requiredInt64Group, stringGroup, booleanGroup, int64Group).block();
        // Mono<Void> asyncResponse = api.testGroupParameters(requiredStringGroup, requiredBooleanGroup, requiredInt64Group, stringGroup, booleanGroup, int64Group);
        // TODO: test validations
    }

    
    /**
     * test inline additionalProperties
     */
    @Test
    public void testInlineAdditionalPropertiesTest() {
        Map<String, String> param = null;
        // api.testInlineAdditionalProperties(param).block();
        // Mono<Void> asyncResponse = api.testInlineAdditionalProperties(param);
        // TODO: test validations
    }

    
    /**
     * test json serialization of form data
     */
    @Test
    public void testJsonFormDataTest() {
        String param = null;
        String param2 = null;
        // api.testJsonFormData(param, param2).block();
        // Mono<Void> asyncResponse = api.testJsonFormData(param, param2);
        // TODO: test validations
    }

    
    /**
     * 
     *
     * To test the collection format in query parameters
     */
    @Test
    public void testQueryParameterCollectionFormatTest() {
        List<String> pipe = null;
        List<String> ioutil = null;
        List<String> http = null;
        List<String> url = null;
        List<String> context = null;
        // api.testQueryParameterCollectionFormat(pipe, ioutil, http, url, context).block();
        // Mono<Void> asyncResponse = api.testQueryParameterCollectionFormat(pipe, ioutil, http, url, context);
        // TODO: test validations
    }

    
}
