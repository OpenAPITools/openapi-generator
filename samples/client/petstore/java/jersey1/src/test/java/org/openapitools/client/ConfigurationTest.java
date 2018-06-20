package org.openapitools.client;

import org.junit.*;
import static org.junit.Assert.*;


public class ConfigurationTest {
    @Test
    public void testDefaultApiClient() {
        ApiClient apiClient = Configuration.getDefaultApiClient();
        assertNotNull(apiClient);
        assertEquals("http://petstore.swagger.io:80/v2", apiClient.getBasePath());
        assertFalse(apiClient.isDebugging());
    }
}
