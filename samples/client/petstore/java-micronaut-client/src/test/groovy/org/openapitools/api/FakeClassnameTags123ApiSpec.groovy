package org.openapitools.api

import org.openapitools.model.ModelClient
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
 * API tests for FakeClassnameTags123Api
 */
@MicronautTest
class FakeClassnameTags123ApiSpec extends Specification {

    @Inject
    FakeClassnameTags123Api api

    
    /**
     * To test class name in snake case
     *
     * To test class name in snake case
     */
    @Ignore("Not Implemented")
    void 'testClassname() test'() {
        given:
        ModelClient _body = new ModelClient()

        when:
        ModelClient body = api.testClassname(_body).block()

        then:
        true
        // TODO: test validations
    }

    
}
