package io.swagger.client;

import static org.junit.Assert.*;
import org.junit.*;

public class ApiClientTest {
  ApiClient apiClient = null;

  @Before
  public void setup() {
    apiClient = new ApiClient();
  }
  @Test
  public void testSelectHeaderAccept() {
    String[] accepts = { "APPLICATION/JSON", "APPLICATION/XML" };
    assertEquals("application/json", apiClient.selectHeaderAccept(accepts));

    accepts = new String[] { "application/json", "application/xml" };
    assertEquals("application/json", apiClient.selectHeaderAccept(accepts));

    accepts = new String[] { "application/xml", "application/json" };
    assertEquals("application/json", apiClient.selectHeaderAccept(accepts));

    accepts = new String[] { "text/plain", "application/xml" };
    assertEquals("text/plain,application/xml", apiClient.selectHeaderAccept(accepts));

    accepts = new String[] { };
    assertNull(apiClient.selectHeaderAccept(accepts));
  }

  @Test
  public void testSelectHeaderContentType() {
    String[] contentTypes = { "APPLICATION/JSON", "APPLICATION/XML" };
    assertEquals("application/json", apiClient.selectHeaderContentType(contentTypes));

    contentTypes = new String[] { "application/json", "application/xml" };
    assertEquals("application/json", apiClient.selectHeaderContentType(contentTypes));

    contentTypes = new String[] { "application/xml", "application/json" };
    assertEquals("application/json", apiClient.selectHeaderContentType(contentTypes));

    contentTypes = new String[] { "text/plain", "application/xml" };
    assertEquals("text/plain", apiClient.selectHeaderContentType(contentTypes));

    contentTypes = new String[] { };
    assertEquals("application/json", apiClient.selectHeaderContentType(contentTypes));
  }
}
