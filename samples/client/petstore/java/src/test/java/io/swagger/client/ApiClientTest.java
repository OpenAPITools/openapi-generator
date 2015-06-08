package io.swagger.client;

import io.swagger.client.auth.*;

import java.util.Map;

import org.junit.*;
import static org.junit.Assert.*;


public class ApiClientTest {
    ApiClient apiClient = null;

    @Before
    public void setup() {
        apiClient = new ApiClient();
    }

    @Test
    public void testSelectHeaderAccept() {
        String[] accepts = {"APPLICATION/JSON", "APPLICATION/XML"};
        assertEquals("application/json", apiClient.selectHeaderAccept(accepts));

        accepts = new String[]{"application/json", "application/xml"};
        assertEquals("application/json", apiClient.selectHeaderAccept(accepts));

        accepts = new String[]{"application/xml", "application/json"};
        assertEquals("application/json", apiClient.selectHeaderAccept(accepts));

        accepts = new String[]{"text/plain", "application/xml"};
        assertEquals("text/plain,application/xml", apiClient.selectHeaderAccept(accepts));

        accepts = new String[]{};
        assertNull(apiClient.selectHeaderAccept(accepts));
    }

    @Test
    public void testSelectHeaderContentType() {
        String[] contentTypes = {"APPLICATION/JSON", "APPLICATION/XML"};
        assertEquals("application/json", apiClient.selectHeaderContentType(contentTypes));

        contentTypes = new String[]{"application/json", "application/xml"};
        assertEquals("application/json", apiClient.selectHeaderContentType(contentTypes));

        contentTypes = new String[]{"application/xml", "application/json"};
        assertEquals("application/json", apiClient.selectHeaderContentType(contentTypes));

        contentTypes = new String[]{"text/plain", "application/xml"};
        assertEquals("text/plain", apiClient.selectHeaderContentType(contentTypes));

        contentTypes = new String[]{};
        assertEquals("application/json", apiClient.selectHeaderContentType(contentTypes));
    }

    @Test
    public void testGetAuthentications() {
        Map<String, Authentication> auths = apiClient.getAuthentications();

        Authentication auth = auths.get("api_key");
        assertNotNull(auth);
        assertTrue(auth instanceof ApiKeyAuth);
        ApiKeyAuth apiKeyAuth = (ApiKeyAuth) auth;
        assertEquals("header", apiKeyAuth.getLocation());
        assertEquals("api_key", apiKeyAuth.getParamName());

        auth = auths.get("petstore_auth");
        assertTrue(auth instanceof OAuth);
        assertSame(auth, apiClient.getAuthentication("petstore_auth"));

        assertNull(auths.get("unknown"));

        try {
            auths.put("my_auth", new HttpBasicAuth());
            fail("the authentications returned should not be modifiable");
        } catch (UnsupportedOperationException e) {
        }
    }

    @Test
    public void testSetUsername() {
        try {
            apiClient.setUsername("my-username");
            fail("there should be no HTTP basic authentications");
        } catch (RuntimeException e) {
        }
    }

    @Test
    public void testSetPassword() {
        try {
            apiClient.setPassword("my-password");
            fail("there should be no HTTP basic authentications");
        } catch (RuntimeException e) {
        }
    }

    @Test
    public void testSetApiKeyAndPrefix() {
        ApiKeyAuth auth = (ApiKeyAuth) apiClient.getAuthentications().get("api_key");
        auth.setApiKey(null);
        auth.setApiKeyPrefix(null);

        apiClient.setApiKey("my-api-key");
        apiClient.setApiKeyPrefix("Token");
        assertEquals("my-api-key", auth.getApiKey());
        assertEquals("Token", auth.getApiKeyPrefix());

        // reset values
        auth.setApiKey(null);
        auth.setApiKeyPrefix(null);
    }
}
