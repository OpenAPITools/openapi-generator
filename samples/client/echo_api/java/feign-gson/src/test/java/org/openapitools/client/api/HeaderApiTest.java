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
 * API tests for HeaderApi
 */
class HeaderApiTest {

    private HeaderApi api;

    @BeforeEach
    public void setup() {
        api = new ApiClient().buildClient(HeaderApi.class);
    }

    
    /**
     * Test header parameter(s)
     *
     * Test header parameter(s)
     */
    @Test
    void testHeaderIntegerBooleanStringEnumsTest() {
        Integer integerHeader = null;
        Boolean booleanHeader = null;
        String stringHeader = null;
        String enumNonrefStringHeader = null;
        StringEnumRef enumRefStringHeader = null;
        // String response = api.testHeaderIntegerBooleanStringEnums(integerHeader, booleanHeader, stringHeader, enumNonrefStringHeader, enumRefStringHeader);

        // TODO: test validations
    }

    
}
