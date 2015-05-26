package io.swagger.client;

import static org.junit.Assert.*;
import org.junit.*;

public class ConfigurationTest {
  @Test
  public void testDefaultApiClient() {
    ApiClient apiClient = Configuration.getDefaultApiClient();
    assertNotNull(apiClient);
    assertEquals("http://petstore.swagger.io/v2", apiClient.getBasePath());
    assertFalse(apiClient.isDebug());
  }
}
