package org.openapitools.api

import java.math.BigDecimal
import java.io.File
import org.openapitools.model.FileSchemaTestClass
import java.time.LocalDate
import org.openapitools.model.ModelClient
import java.time.OffsetDateTime
import org.openapitools.model.OuterComposite
import org.openapitools.model.User
import org.openapitools.model.XmlItem
import io.micronaut.test.extensions.spock.annotation.MicronautTest
import spock.lang.Specification
import jakarta.inject.Inject
import spock.lang.Ignore
import java.util.Arrays
import java.util.ArrayList
import java.util.HashMap
import java.util.List
import java.util.Map
import java.util.HashSet


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
    @Ignore("Not Implemented")
    void 'createXmlItem() test'() {
        given:
        XmlItem xmlItem = new XmlItem()

        when:
        api.createXmlItem(xmlItem).block()

        then:
        true
        // TODO: test validations
    }

    
    /**
     * 
     *
     * Test serialization of outer boolean types
     */
    @Ignore("Not Implemented")
    void 'fakeOuterBooleanSerialize() test'() {
        given:
        Boolean _body = false

        when:
        Boolean body = api.fakeOuterBooleanSerialize(_body).block()

        then:
        true
        // TODO: test validations
    }

    
    /**
     * 
     *
     * Test serialization of object with outer number type
     */
    @Ignore("Not Implemented")
    void 'fakeOuterCompositeSerialize() test'() {
        given:
        OuterComposite _body = new OuterComposite()

        when:
        OuterComposite body = api.fakeOuterCompositeSerialize(_body).block()

        then:
        true
        // TODO: test validations
    }

    
    /**
     * 
     *
     * Test serialization of outer number types
     */
    @Ignore("Not Implemented")
    void 'fakeOuterNumberSerialize() test'() {
        given:
        BigDecimal _body = new BigDecimal(78)

        when:
        BigDecimal body = api.fakeOuterNumberSerialize(_body).block()

        then:
        true
        // TODO: test validations
    }

    
    /**
     * 
     *
     * Test serialization of outer string types
     */
    @Ignore("Not Implemented")
    void 'fakeOuterStringSerialize() test'() {
        given:
        String _body = 'example'

        when:
        String body = api.fakeOuterStringSerialize(_body).block()

        then:
        true
        // TODO: test validations
    }

    
    /**
     * 
     *
     * For this test, the body for this request much reference a schema named &#x60;File&#x60;.
     */
    @Ignore("Not Implemented")
    void 'testBodyWithFileSchema() test'() {
        given:
        FileSchemaTestClass _body = new FileSchemaTestClass()

        when:
        api.testBodyWithFileSchema(_body).block()

        then:
        true
        // TODO: test validations
    }

    
    /**
     * 
     */
    @Ignore("Not Implemented")
    void 'testBodyWithQueryParams() test'() {
        given:
        String query = 'example'
        User _body = new User()

        when:
        api.testBodyWithQueryParams(query, _body).block()

        then:
        true
        // TODO: test validations
    }

    
    /**
     * To test \&quot;client\&quot; model
     *
     * To test \&quot;client\&quot; model
     */
    @Ignore("Not Implemented")
    void 'testClientModel() test'() {
        given:
        ModelClient _body = new ModelClient()

        when:
        ModelClient body = api.testClientModel(_body).block()

        then:
        true
        // TODO: test validations
    }

    
    /**
     * Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
     *
     * Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
     */
    @Ignore("Not Implemented")
    void 'testEndpointParameters() test'() {
        given:
        BigDecimal number = new BigDecimal(78)
        Double _double = 3.4D
        String patternWithoutDelimiter = 'example'
        byte[] _byte = null
        Integer integer = 56
        Integer int32 = 56
        Long int64 = 56L
        Float _float = 3.4F
        String string = 'example'
        File binary = null
        LocalDate date = LocalDate.of(2001, 2, 3)
        OffsetDateTime dateTime = OffsetDateTime.of(2001, 2, 3, 12, 0, 0, 0, java.time.ZoneOffset.of("+02:00"))
        String password = 'example'
        String paramCallback = 'example'

        when:
        api.testEndpointParameters(number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, string, binary, date, dateTime, password, paramCallback).block()

        then:
        true
        // TODO: test validations
    }

    
    /**
     * To test enum parameters
     *
     * To test enum parameters
     */
    @Ignore("Not Implemented")
    void 'testEnumParameters() test'() {
        given:
        List<String> enumHeaderStringArray = ['$']
        String enumHeaderString = '-efg'
        List<String> enumQueryStringArray = ['$']
        String enumQueryString = '-efg'
        Integer enumQueryInteger = 56
        Double enumQueryDouble = 3.4D
        List<String> enumFormStringArray = ['$']
        String enumFormString = '-efg'

        when:
        api.testEnumParameters(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumFormStringArray, enumFormString).block()

        then:
        true
        // TODO: test validations
    }

    
    /**
     * Fake endpoint to test group parameters (optional)
     *
     * Fake endpoint to test group parameters (optional)
     */
    @Ignore("Not Implemented")
    void 'testGroupParameters() test'() {
        given:
        Integer requiredStringGroup = 56
        Boolean requiredBooleanGroup = false
        Long requiredInt64Group = 56L
        Integer stringGroup = 56
        Boolean booleanGroup = false
        Long int64Group = 56L

        when:
        api.testGroupParameters(requiredStringGroup, requiredBooleanGroup, requiredInt64Group, stringGroup, booleanGroup, int64Group).block()

        then:
        true
        // TODO: test validations
    }

    
    /**
     * test inline additionalProperties
     */
    @Ignore("Not Implemented")
    void 'testInlineAdditionalProperties() test'() {
        given:
        Map<String, String> param = [:]

        when:
        api.testInlineAdditionalProperties(param).block()

        then:
        true
        // TODO: test validations
    }

    
    /**
     * test json serialization of form data
     */
    @Ignore("Not Implemented")
    void 'testJsonFormData() test'() {
        given:
        String param = 'example'
        String param2 = 'example'

        when:
        api.testJsonFormData(param, param2).block()

        then:
        true
        // TODO: test validations
    }

    
    /**
     * 
     *
     * To test the collection format in query parameters
     */
    @Ignore("Not Implemented")
    void 'testQueryParameterCollectionFormat() test'() {
        given:
        List<String> pipe = ['example']
        List<String> ioutil = ['example']
        List<String> http = ['example']
        List<String> url = ['example']
        List<String> context = ['example']

        when:
        api.testQueryParameterCollectionFormat(pipe, ioutil, http, url, context).block()

        then:
        true
        // TODO: test validations
    }

    
}
