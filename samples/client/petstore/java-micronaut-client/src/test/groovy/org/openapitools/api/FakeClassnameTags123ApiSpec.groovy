package org.openapitools.api

import org.openapitools.model.ModelClient
import io.micronaut.test.extensions.spock.annotation.MicronautTest
import spock.lang.Specification
import jakarta.inject.Inject
import reactor.core.publisher.Mono
import java.util.ArrayList
import java.util.HashMap
import java.util.List
import java.util.Map


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
    void "testClassname() test"() {
        given:
        ModelClient _body = null
        // ModelClient response = api.testClassname(_body).block()
        // Mono<ModelClient> asyncResponse = api.testClassname(_body)

        expect:
        true
        // TODO: test validations
    }

    
}
