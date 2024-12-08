package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import org.openapitools.client.model.DataQuery;
import java.time.LocalDate;
import java.time.OffsetDateTime;
import org.openapitools.client.model.Pet;
import org.openapitools.client.model.StringEnumRef;
import org.openapitools.client.model.TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter;
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
    void testEnumRefStringTest() {
        String enumNonrefStringQuery = null;
        StringEnumRef enumRefStringQuery = null;
        // String response = api.testEnumRefString(enumNonrefStringQuery, enumRefStringQuery);

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
    void testEnumRefStringTestQueryMap() {
        QueryApi.TestEnumRefStringQueryParams queryParams = new QueryApi.TestEnumRefStringQueryParams()
            .enumNonrefStringQuery(null)
            .enumRefStringQuery(null);
        // String response = api.testEnumRefString(queryParams);

    // TODO: test validations
    }
    
    /**
     * Test query parameter(s)
     *
     * Test query parameter(s)
     */
    @Test
    void testQueryDatetimeDateStringTest() {
        OffsetDateTime datetimeQuery = null;
        LocalDate dateQuery = null;
        String stringQuery = null;
        // String response = api.testQueryDatetimeDateString(datetimeQuery, dateQuery, stringQuery);

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
    void testQueryDatetimeDateStringTestQueryMap() {
        QueryApi.TestQueryDatetimeDateStringQueryParams queryParams = new QueryApi.TestQueryDatetimeDateStringQueryParams()
            .datetimeQuery(null)
            .dateQuery(null)
            .stringQuery(null);
        // String response = api.testQueryDatetimeDateString(queryParams);

    // TODO: test validations
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
    void testQueryStyleDeepObjectExplodeTrueObjectTest() {
        Pet queryObject = null;
        // String response = api.testQueryStyleDeepObjectExplodeTrueObject(queryObject);

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
    void testQueryStyleDeepObjectExplodeTrueObjectTestQueryMap() {
        QueryApi.TestQueryStyleDeepObjectExplodeTrueObjectQueryParams queryParams = new QueryApi.TestQueryStyleDeepObjectExplodeTrueObjectQueryParams()
            .queryObject(null);
        // String response = api.testQueryStyleDeepObjectExplodeTrueObject(queryParams);

    // TODO: test validations
    }
    
    /**
     * Test query parameter(s)
     *
     * Test query parameter(s)
     */
    @Test
    void testQueryStyleDeepObjectExplodeTrueObjectAllOfTest() {
        TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter queryObject = null;
        // String response = api.testQueryStyleDeepObjectExplodeTrueObjectAllOf(queryObject);

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
    void testQueryStyleDeepObjectExplodeTrueObjectAllOfTestQueryMap() {
        QueryApi.TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryParams queryParams = new QueryApi.TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryParams()
            .queryObject(null);
        // String response = api.testQueryStyleDeepObjectExplodeTrueObjectAllOf(queryParams);

    // TODO: test validations
    }
    
    /**
     * Test query parameter(s)
     *
     * Test query parameter(s)
     */
    @Test
    void testQueryStyleFormExplodeFalseArrayIntegerTest() {
        List<Integer> queryObject = null;
        // String response = api.testQueryStyleFormExplodeFalseArrayInteger(queryObject);

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
    void testQueryStyleFormExplodeFalseArrayIntegerTestQueryMap() {
        QueryApi.TestQueryStyleFormExplodeFalseArrayIntegerQueryParams queryParams = new QueryApi.TestQueryStyleFormExplodeFalseArrayIntegerQueryParams()
            .queryObject(null);
        // String response = api.testQueryStyleFormExplodeFalseArrayInteger(queryParams);

    // TODO: test validations
    }
    
    /**
     * Test query parameter(s)
     *
     * Test query parameter(s)
     */
    @Test
    void testQueryStyleFormExplodeFalseArrayStringTest() {
        List<String> queryObject = null;
        // String response = api.testQueryStyleFormExplodeFalseArrayString(queryObject);

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
    void testQueryStyleFormExplodeFalseArrayStringTestQueryMap() {
        QueryApi.TestQueryStyleFormExplodeFalseArrayStringQueryParams queryParams = new QueryApi.TestQueryStyleFormExplodeFalseArrayStringQueryParams()
            .queryObject(null);
        // String response = api.testQueryStyleFormExplodeFalseArrayString(queryParams);

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
    
    /**
     * Test query parameter(s)
     *
     * Test query parameter(s)
     */
    @Test
    void testQueryStyleFormExplodeTrueObjectAllOfTest() {
        DataQuery queryObject = null;
        // String response = api.testQueryStyleFormExplodeTrueObjectAllOf(queryObject);

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
    void testQueryStyleFormExplodeTrueObjectAllOfTestQueryMap() {
        QueryApi.TestQueryStyleFormExplodeTrueObjectAllOfQueryParams queryParams = new QueryApi.TestQueryStyleFormExplodeTrueObjectAllOfQueryParams()
            .queryObject(null);
        // String response = api.testQueryStyleFormExplodeTrueObjectAllOf(queryParams);

    // TODO: test validations
    }
    
}
