package org.openapitools.client.auth;

import com.github.scribejava.core.model.OAuth2AccessToken;
import com.github.scribejava.core.oauth.OAuth20Service;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.junit.Before;
import org.junit.Test;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import org.openapitools.client.Pair;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutionException;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class OAuthPasswordGrantTest {

    private OAuthPasswordGrant oauth;

    @Before
    public void setUp() throws Exception {
        oauth = new OAuthPasswordGrant("_clientId", "_clientSecret", "https://token.example.com");
        oauth.setAccessToken("expired-access-token");
        FieldUtils.writeField(oauth, "service", mockOAuthService(), true);
    }

    @Test
    public void testApplyToParams() throws IOException, ExecutionException, InterruptedException {
        List<Pair> queryParams = new ArrayList<Pair>();
        Map<String, String> headerParams = new HashMap<String, String>();
        Map<String, String> cookieParams = new HashMap<String, String>();

        oauth.applyToParams(queryParams, headerParams, cookieParams);

        // no changes to query or cookie parameters
        assertEquals(0, queryParams.size());
        assertEquals(0, cookieParams.size());
        assertEquals(1, headerParams.size());
        String expected = "Bearer expired-access-token";
        assertEquals(expected, headerParams.get("Authorization"));

        // let's try to get a new access token
        oauth.obtainAccessToken("_username", "_password", "scope");
        oauth.applyToParams(queryParams, headerParams, cookieParams);
        assertEquals(0, queryParams.size());
        assertEquals(0, cookieParams.size());
        assertEquals(1, headerParams.size());
        expected = "Bearer new-access-token";
        assertEquals(expected, headerParams.get("Authorization"));
    }

    private OAuth20Service mockOAuthService() throws IOException, ExecutionException, InterruptedException {
        OAuth2AccessToken response = mock(OAuth2AccessToken.class);
        when(response.getAccessToken()).thenAnswer(new Answer<String>() {
            @Override
            public String answer(InvocationOnMock invocation) throws Throwable {
                // sleep ensures that the bug is triggered.
                Thread.sleep(1000);
                return "new-access-token";
            }
        });

        OAuth20Service service = mock(OAuth20Service.class);
        when(service.getAccessTokenPasswordGrant(eq("_username"), eq("_password"), eq("scope"))).thenReturn(response);
        return service;
    }
}
