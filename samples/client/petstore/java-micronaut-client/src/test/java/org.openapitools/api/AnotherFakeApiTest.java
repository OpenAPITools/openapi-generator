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
 * API tests for AnotherFakeApi
 */
@MicronautTest
public class AnotherFakeApiTest {

    @Inject
    AnotherFakeApi api;

    
    /**
     * To test special tags
     *
     * To test special tags and operation ID starting with number
     */
    @Test
    public void call123testSpecialTagsTest() {
        ModelClient _body = null;
        // ModelClient response = api.call123testSpecialTags(_body).block();
        // Mono<ModelClient> asyncResponse = api.call123testSpecialTags(_body);
        // TODO: test validations
    }

    
}
