package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import org.openapitools.client.model.TestFormObjectMultipartRequestMarker;
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

    
    /**
     * Test form parameter(s) for multipart schema
     *
     * Test form parameter(s) for multipart schema
     */
    @Test
    void testFormObjectMultipartTest() {
        TestFormObjectMultipartRequestMarker marker = null;
        // String response = api.testFormObjectMultipart(marker);

        // TODO: test validations
    }

    
    /**
     * Test form parameter(s) for oneOf schema
     *
     * Test form parameter(s) for oneOf schema
     */
    @Test
    void testFormOneofTest() {
        String form1 = null;
        Integer form2 = null;
        String form3 = null;
        Boolean form4 = null;
        Long id = null;
        String name = null;
        // String response = api.testFormOneof(form1, form2, form3, form4, id, name);

        // TODO: test validations
    }

    
}
