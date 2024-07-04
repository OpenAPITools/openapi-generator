package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import org.openapitools.client.model.Pet;
import org.openapitools.client.model.TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;

import java.time.LocalDate;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * API tests for QueryApi
 */
class QueryApiTest {

    private QueryApi api;

    @BeforeEach
    public void setup() {
        api = new ApiClient().buildClient(QueryApi.class);
    }

    
    /**
     * Test query parameter(s)
     *
     * Test query parameter(s)
     */
    @Test
    void testQueryIntegerBooleanStringTest() {
        Integer integerQuery = null;
        Boolean booleanQuery = null;
        String stringQuery = null;
        // String response = api.testQueryIntegerBooleanString(integerQuery, booleanQuery, stringQuery);

        // TODO: test validations
    }

    /**
     * Test query parameter(s)
     *
     * Test query parameter(s)
     *
     * This tests the overload of the method that uses a Map for query parameters instead of
     * listing them out individually.
     */
    @Test
    void testQueryIntegerBooleanStringTestQueryMap() {
        QueryApi.TestQueryIntegerBooleanStringQueryParams queryParams = new QueryApi.TestQueryIntegerBooleanStringQueryParams()
            .integerQuery(null)
            .booleanQuery(null)
            .stringQuery(null);
        // String response = api.testQueryIntegerBooleanString(queryParams);

    // TODO: test validations
    }
    
    /**
     * Test query parameter(s)
     *
     * Test query parameter(s)
     */
    @Test
    void testQueryStyleFormExplodeTrueArrayStringTest() {
        TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter queryObject = null;
        // String response = api.testQueryStyleFormExplodeTrueArrayString(queryObject);

        // TODO: test validations
    }

    /**
     * Test query parameter(s)
     *
     * Test query parameter(s)
     *
     * This tests the overload of the method that uses a Map for query parameters instead of
     * listing them out individually.
     */
    @Test
    void testQueryStyleFormExplodeTrueArrayStringTestQueryMap() {
        QueryApi.TestQueryStyleFormExplodeTrueArrayStringQueryParams queryParams = new QueryApi.TestQueryStyleFormExplodeTrueArrayStringQueryParams()
            .queryObject(null);
        // String response = api.testQueryStyleFormExplodeTrueArrayString(queryParams);

    // TODO: test validations
    }
    
    /**
     * Test query parameter(s)
     *
     * Test query parameter(s)
     */
    @Test
    void testQueryStyleFormExplodeTrueObjectTest() {
        Pet queryObject = null;
        // String response = api.testQueryStyleFormExplodeTrueObject(queryObject);

        // TODO: test validations
    }

    /**
     * Test query parameter(s)
     *
     * Test query parameter(s)
     *
     * This tests the overload of the method that uses a Map for query parameters instead of
     * listing them out individually.
     */
    @Test
    void testQueryStyleFormExplodeTrueObjectTestQueryMap() {
        QueryApi.TestQueryStyleFormExplodeTrueObjectQueryParams queryParams = new QueryApi.TestQueryStyleFormExplodeTrueObjectQueryParams()
            .queryObject(null);
        // String response = api.testQueryStyleFormExplodeTrueObject(queryParams);

    // TODO: test validations
    }
    
}
