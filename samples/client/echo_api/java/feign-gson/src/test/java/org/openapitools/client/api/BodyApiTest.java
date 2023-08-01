package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import org.openapitools.client.model.Pet;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;

import java.time.LocalDate;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * API tests for BodyApi
 */
class BodyApiTest {

    private BodyApi api;

    @BeforeEach
    public void setup() {
        api = new ApiClient().buildClient(BodyApi.class);
    }

    
    /**
     * Test body parameter(s)
     *
     * Test body parameter(s)
     */
    @Test
    void testEchoBodyPetTest() {
        Pet pet = null;
        // Pet response = api.testEchoBodyPet(pet);

        // TODO: test validations
    }

    
}
