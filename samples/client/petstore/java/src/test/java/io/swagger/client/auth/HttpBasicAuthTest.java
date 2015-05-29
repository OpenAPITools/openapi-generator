package io.swagger.client.auth;

import java.util.Map;
import java.util.HashMap;

import static org.junit.Assert.*;
import org.junit.*;

public class HttpBasicAuthTest {
  HttpBasicAuth auth = null;

  @Before
  public void setup() {
    auth = new HttpBasicAuth();
  }

  @Test
  public void testApplyToParams() {
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();

    auth.setUsername("my-username");
    auth.setPassword("my-password");
    auth.applyToParams(queryParams, headerParams);

    // no changes to query parameters
    assertEquals(0, queryParams.size());
    assertEquals(1, headerParams.size());
    // the string below is base64-encoded result of "my-username:my-password" with the "Basic " prefix
    final String expected = "Basic bXktdXNlcm5hbWU6bXktcGFzc3dvcmQ=";
    assertEquals(expected, headerParams.get("Authorization"));
  }
}
