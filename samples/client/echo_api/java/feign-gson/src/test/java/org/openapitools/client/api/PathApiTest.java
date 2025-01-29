package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import org.openapitools.client.model.StringEnumRef;
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
    void testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPathTest() {
        String pathString = null;
        Integer pathInteger = null;
        String enumNonrefStringPath = null;
        StringEnumRef enumRefStringPath = null;
        // String response = api.testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath(pathString, pathInteger, enumNonrefStringPath, enumRefStringPath);

        // TODO: test validations
    }

    
}
