package org.openapitools.client;

import org.junit.jupiter.api.*;

import static org.junit.jupiter.api.Assertions.*;


public class ConfigurationTest {
    @Test
    public void testDefaultApiClient() {
        ApiClient apiClient = Configuration.getDefaultApiClient();
        assertNotNull(apiClient);
        assertEquals("https://petstore.swagger.io/v2", apiClient.getBasePath());
        assertFalse(apiClient.isDebugging());
    }
}
