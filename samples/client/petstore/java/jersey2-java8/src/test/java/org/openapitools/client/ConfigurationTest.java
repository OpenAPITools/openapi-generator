package org.openapitools.client;

import org.junit.jupiter.api.*;

import static org.junit.jupiter.api.Assertions.*;


public class ConfigurationTest {
    @Test
    public void testDefaultApiClient() {
        ApiClient apiClient = Configuration.getDefaultApiClient();
        assertNotNull(apiClient);
        assertEquals("http://petstore.swagger.io:80/v2", apiClient.getBasePath());
        assertFalse(apiClient.isDebugging());
    }
}
