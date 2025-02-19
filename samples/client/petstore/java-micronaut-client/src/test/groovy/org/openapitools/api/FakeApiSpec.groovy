package org.openapitools.api

import java.math.BigDecimal
import java.io.File
import org.openapitools.model.FileSchemaTestClass
import java.time.LocalDate
import java.time.LocalDateTime
import org.openapitools.model.ModelClient
import org.openapitools.model.OuterComposite
import org.openapitools.model.User
import org.openapitools.model.XmlItem
import io.micronaut.test.extensions.spock.annotation.MicronautTest
import spock.lang.Specification
import jakarta.inject.Inject
import reactor.core.publisher.Mono
import java.util.ArrayList
import java.util.HashMap
import java.util.List
import java.util.Map


/**
 * API tests for FakeApi
 */
@MicronautTest
class FakeApiSpec extends Specification {

    @Inject
    FakeApi api

    
    /**
     * creates an XmlItem
     *
     * this route creates an XmlItem
     */
    void "createXmlItem() test"() {
        given:
        XmlItem xmlItem = null
        // api.createXmlItem(xmlItem).block()
        // Mono<Void> asyncResponse = api.createXmlItem(xmlItem)

        expect:
        true
        // TODO: test validations
    }

    
    /**
     * 
     *
     * Test serialization of outer boolean types
     */
    void "fakeOuterBooleanSerialize() test"() {
        given:
        Boolean _body = null
        // Boolean response = api.fakeOuterBooleanSerialize(_body).block()
        // Mono<Boolean> asyncResponse = api.fakeOuterBooleanSerialize(_body)

        expect:
        true
        // TODO: test validations
    }

    
    /**
     * 
     *
     * Test serialization of object with outer number type
     */
    void "fakeOuterCompositeSerialize() test"() {
        given:
        OuterComposite _body = null
        // OuterComposite response = api.fakeOuterCompositeSerialize(_body).block()
        // Mono<OuterComposite> asyncResponse = api.fakeOuterCompositeSerialize(_body)

        expect:
        true
        // TODO: test validations
    }

    
    /**
     * 
     *
     * Test serialization of outer number types
     */
    void "fakeOuterNumberSerialize() test"() {
        given:
        BigDecimal _body = null
        // BigDecimal response = api.fakeOuterNumberSerialize(_body).block()
        // Mono<BigDecimal> asyncResponse = api.fakeOuterNumberSerialize(_body)

        expect:
        true
        // TODO: test validations
    }

    
    /**
     * 
     *
     * Test serialization of outer string types
     */
    void "fakeOuterStringSerialize() test"() {
        given:
        String _body = null
        // String response = api.fakeOuterStringSerialize(_body).block()
        // Mono<String> asyncResponse = api.fakeOuterStringSerialize(_body)

        expect:
        true
        // TODO: test validations
    }

    
    /**
     * 
     *
     * For this test, the body for this request much reference a schema named &#x60;File&#x60;.
     */
    void "testBodyWithFileSchema() test"() {
        given:
        FileSchemaTestClass _body = null
        // api.testBodyWithFileSchema(_body).block()
        // Mono<Void> asyncResponse = api.testBodyWithFileSchema(_body)

        expect:
        true
        // TODO: test validations
    }

    
    /**
     * 
     */
    void "testBodyWithQueryParams() test"() {
        given:
        String query = null
        User _body = null
        // api.testBodyWithQueryParams(query, _body).block()
        // Mono<Void> asyncResponse = api.testBodyWithQueryParams(query, _body)

        expect:
        true
        // TODO: test validations
    }

    
    /**
     * To test \&quot;client\&quot; model
     *
     * To test \&quot;client\&quot; model
     */
    void "testClientModel() test"() {
        given:
        ModelClient _body = null
        // ModelClient response = api.testClientModel(_body).block()
        // Mono<ModelClient> asyncResponse = api.testClientModel(_body)

        expect:
        true
        // TODO: test validations
    }

    
    /**
     * Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
     *
     * Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
     */
    void "testEndpointParameters() test"() {
        given:
        BigDecimal number = null
        Double _double = null
        String patternWithoutDelimiter = null
        byte[] _byte = null
        Integer integer = null
        Integer int32 = null
        Long int64 = null
        Float _float = null
        String string = null
        File binary = null
        LocalDate date = null
        LocalDateTime dateTime = null
        String password = null
        String paramCallback = null
        // api.testEndpointParameters(number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, string, binary, date, dateTime, password, paramCallback).block()
        // Mono<Void> asyncResponse = api.testEndpointParameters(number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, string, binary, date, dateTime, password, paramCallback)

        expect:
        true
        // TODO: test validations
    }

    
    /**
     * To test enum parameters
     *
     * To test enum parameters
     */
    void "testEnumParameters() test"() {
        given:
        List<String> enumHeaderStringArray = null
        String enumHeaderString = null
        List<String> enumQueryStringArray = null
        String enumQueryString = null
        Integer enumQueryInteger = null
        Double enumQueryDouble = null
        List<String> enumFormStringArray = null
        String enumFormString = null
        // api.testEnumParameters(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumFormStringArray, enumFormString).block()
        // Mono<Void> asyncResponse = api.testEnumParameters(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumFormStringArray, enumFormString)

        expect:
        true
        // TODO: test validations
    }

    
    /**
     * Fake endpoint to test group parameters (optional)
     *
     * Fake endpoint to test group parameters (optional)
     */
    void "testGroupParameters() test"() {
        given:
        Integer requiredStringGroup = null
        Boolean requiredBooleanGroup = null
        Long requiredInt64Group = null
        Integer stringGroup = null
        Boolean booleanGroup = null
        Long int64Group = null
        // api.testGroupParameters(requiredStringGroup, requiredBooleanGroup, requiredInt64Group, stringGroup, booleanGroup, int64Group).block()
        // Mono<Void> asyncResponse = api.testGroupParameters(requiredStringGroup, requiredBooleanGroup, requiredInt64Group, stringGroup, booleanGroup, int64Group)

        expect:
        true
        // TODO: test validations
    }

    
    /**
     * test inline additionalProperties
     */
    void "testInlineAdditionalProperties() test"() {
        given:
        Map<String, String> param = null
        // api.testInlineAdditionalProperties(param).block()
        // Mono<Void> asyncResponse = api.testInlineAdditionalProperties(param)

        expect:
        true
        // TODO: test validations
    }

    
    /**
     * test json serialization of form data
     */
    void "testJsonFormData() test"() {
        given:
        String param = null
        String param2 = null
        // api.testJsonFormData(param, param2).block()
        // Mono<Void> asyncResponse = api.testJsonFormData(param, param2)

        expect:
        true
        // TODO: test validations
    }

    
    /**
     * 
     *
     * To test the collection format in query parameters
     */
    void "testQueryParameterCollectionFormat() test"() {
        given:
        List<String> pipe = null
        List<String> ioutil = null
        List<String> http = null
        List<String> url = null
        List<String> context = null
        // api.testQueryParameterCollectionFormat(pipe, ioutil, http, url, context).block()
        // Mono<Void> asyncResponse = api.testQueryParameterCollectionFormat(pipe, ioutil, http, url, context)

        expect:
        true
        // TODO: test validations
    }

    
}
