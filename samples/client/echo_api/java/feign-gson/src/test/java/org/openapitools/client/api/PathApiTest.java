package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;

import java.time.LocalDate;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * API tests for PathApi
 */
class PathApiTest {

    private PathApi api;

    @BeforeEach
    public void setup() {
        api = new ApiClient().buildClient(PathApi.class);
    }

    
    /**
     * Test path parameter(s)
     *
     * Test path parameter(s)
     */
    @Test
    void testsPathStringPathStringIntegerPathIntegerTest() {
        String pathString = null;
        Integer pathInteger = null;
        // String response = api.testsPathStringPathStringIntegerPathInteger(pathString, pathInteger);

        // TODO: test validations
    }

    
}
