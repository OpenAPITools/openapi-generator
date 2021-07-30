package org.openapitools.api;

import org.openapitools.model.ModelClient;
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
 * API tests for FakeClassnameTags123Api
 */
@MicronautTest
public class FakeClassnameTags123ApiTest {

    @Inject
    FakeClassnameTags123Api api;

    
    /**
     * To test class name in snake case
     *
     * To test class name in snake case
     */
    @Test
    public void testClassnameTest() {
        ModelClient _body = null;
        // ModelClient response = api.testClassname(_body).block();
        // Mono<ModelClient> asyncResponse = api.testClassname(_body);
        // TODO: test validations
    }

    
}
