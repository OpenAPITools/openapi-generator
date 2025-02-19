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
    void "call123testSpecialTags() test"() {
        given:
        ModelClient _body = null
        // ModelClient response = api.call123testSpecialTags(_body).block()
        // Mono<ModelClient> asyncResponse = api.call123testSpecialTags(_body)

        expect:
        true
        // TODO: test validations
    }

    
}
