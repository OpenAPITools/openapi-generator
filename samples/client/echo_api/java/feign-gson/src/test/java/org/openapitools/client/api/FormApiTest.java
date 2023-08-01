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
 * API tests for FormApi
 */
class FormApiTest {

    private FormApi api;

    @BeforeEach
    public void setup() {
        api = new ApiClient().buildClient(FormApi.class);
    }

    
    /**
     * Test form parameter(s)
     *
     * Test form parameter(s)
     */
    @Test
    void testFormIntegerBooleanStringTest() {
        Integer integerForm = null;
        Boolean booleanForm = null;
        String stringForm = null;
        // String response = api.testFormIntegerBooleanString(integerForm, booleanForm, stringForm);

        // TODO: test validations
    }

    
}
