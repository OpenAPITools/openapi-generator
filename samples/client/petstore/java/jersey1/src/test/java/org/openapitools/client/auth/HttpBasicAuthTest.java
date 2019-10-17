package org.openapitools.client.auth;

import java.util.HashMap;
import java.util.ArrayList;
import java.util.Map;
import java.util.List;

import org.openapitools.client.Pair;
import org.junit.*;
import static org.junit.Assert.*;


public class HttpBasicAuthTest {
    HttpBasicAuth auth = null;

    @Before
    public void setup() {
        auth = new HttpBasicAuth();
    }

    @Test
    public void testApplyToParams() {
        List<Pair> queryParams = new ArrayList<Pair>();
        Map<String, String> headerParams = new HashMap<String, String>();
        Map<String, String> cookieParams = new HashMap<String, String>();

        auth.setUsername("my-username");
        auth.setPassword("my-password");
        auth.applyToParams(queryParams, headerParams, cookieParams);

        // no changes to query or cookie parameters
        assertEquals(0, queryParams.size());
        assertEquals(0, cookieParams.size());
        assertEquals(1, headerParams.size());
        // the string below is base64-encoded result of "my-username:my-password" with the "Basic " prefix
        String expected = "Basic bXktdXNlcm5hbWU6bXktcGFzc3dvcmQ=";
        assertEquals(expected, headerParams.get("Authorization"));

        // null username should be treated as empty string
        auth.setUsername(null);
        auth.applyToParams(queryParams, headerParams, cookieParams);
        // the string below is base64-encoded result of ":my-password" with the "Basic " prefix
        expected = "Basic Om15LXBhc3N3b3Jk";
        assertEquals(expected, headerParams.get("Authorization"));

        // null password should be treated as empty string
        auth.setUsername("my-username");
        auth.setPassword(null);
        auth.applyToParams(queryParams, headerParams, cookieParams);
        // the string below is base64-encoded result of "my-username:" with the "Basic " prefix
        expected = "Basic bXktdXNlcm5hbWU6";
        assertEquals(expected, headerParams.get("Authorization"));
    }
}
