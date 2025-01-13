package org.openapitools.api

import org.openapitools.model.ModelClient
import java.util.UUID
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
 * API tests for AnotherFakeApi
 */
@MicronautTest
class AnotherFakeApiSpec extends Specification {

    @Inject
    AnotherFakeApi api

    
    /**
     * To test special tags
     *
     * To test special tags and operation ID starting with number
     */
    @Ignore("Not Implemented")
    void 'call123testSpecialTags() test'() {
        given:
        UUID uuidTest = null
        ModelClient _body = new ModelClient()

        when:
        ModelClient body = api.call123testSpecialTags(uuidTest, _body).block()

        then:
        true
        // TODO: test validations
    }

    
}
