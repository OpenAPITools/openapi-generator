package io.swagger.client.auth;

import java.util.HashMap;
import java.util.ArrayList;
import java.util.Map;
import java.util.List;

import org.junit.*;
import org.springframework.http.HttpHeaders;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;

import static org.junit.Assert.*;

public class HttpBasicAuthTest {
    HttpBasicAuth auth = null;

    @Before
    public void setup() {
        auth = new HttpBasicAuth();
    }

    @Test
    public void testApplyToParams() {
        MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        HttpHeaders headerParams = new HttpHeaders();

        auth.setUsername("my-username");
        auth.setPassword("my-password");
        auth.applyToParams(queryParams, headerParams);

        // no changes to query parameters
        assertEquals(0, queryParams.size());
        assertEquals(1, headerParams.size());
        // the string below is base64-encoded result of "my-username:my-password" with the "Basic " prefix
        String expected = "Basic bXktdXNlcm5hbWU6bXktcGFzc3dvcmQ=";
        assertEquals(expected, headerParams.get("Authorization").get(0));

        // null username should be treated as empty string
        auth.setUsername(null);
        auth.applyToParams(queryParams, headerParams);
        // the string below is base64-encoded result of ":my-password" with the "Basic " prefix
        expected = "Basic Om15LXBhc3N3b3Jk";
        assertEquals(expected, headerParams.get("Authorization").get(1));

        // null password should be treated as empty string
        auth.setUsername("my-username");
        auth.setPassword(null);
        auth.applyToParams(queryParams, headerParams);
        // the string below is base64-encoded result of "my-username:" with the "Basic " prefix
        expected = "Basic bXktdXNlcm5hbWU6";
        assertEquals(expected, headerParams.get("Authorization").get(2));
    }
}
