package org.openapitools.client;

import static org.junit.Assert.*;

import org.junit.*;

public class ConfigurationTest {
    @Test
    public void testDefaultApiClient() {
        ApiClient apiClient = Configuration.getDefaultApiClient();
        assertNotNull(apiClient);
        assertEquals("http://petstore.swagger.io:80/v2", apiClient.getBasePath());
        assertFalse(apiClient.isDebugging());
    }
}
