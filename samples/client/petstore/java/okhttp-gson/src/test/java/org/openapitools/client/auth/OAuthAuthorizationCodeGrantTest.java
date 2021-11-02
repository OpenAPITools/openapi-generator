package org.openapitools.client.auth;

import com.github.scribejava.core.model.OAuth2AccessToken;
import com.github.scribejava.core.model.OAuth2Authorization;
import com.github.scribejava.core.oauth.AccessTokenRequestParams;
import com.github.scribejava.core.oauth.OAuth20Service;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.junit.Before;
import org.junit.Test;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import org.openapitools.client.Pair;

import java.io.IOException;
import java.util.*;
import java.util.concurrent.ExecutionException;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class OAuthAuthorizationCodeGrantTest {

    private OAuthAuthorizationCodeGrant oauth;

    @Before
    public void setUp() throws Exception {
        oauth = new OAuthAuthorizationCodeGrant("_clientId", "_clientSecret", "https://token.example.com", 
            "https://auth.example.com", "http://localhost:8080", Collections.<String, String>emptyMap());
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

        // let's simulate a new access token req
        // first, build the authorization url
        String state = "fake_state";
        String url = oauth.getAuthorizationUrl(state);
        assertEquals("https://auth.example.com?response_type=code&client_id=_clientId&redirect_uri=http%3A%2F%2Flocalhost:8080&scope=scope&state=" + state, url);

        // for the test purpose, skipping the redirect step and simulating the callback
        OAuth2Authorization authorization = oauth.extractAuthorization("http://localhost:8080?code=obtained_code&state=" + state);
        String authCode = authorization.getCode();
        assertEquals(state, authorization.getState());
        assertEquals("obtained_code", authCode);

        // then the access token request
        oauth.obtainAccessToken(authCode, "scope");
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
        when(service.getAuthorizationUrl(eq("fake_state")))
                .thenReturn("https://auth.example.com?response_type=code&client_id=_clientId&redirect_uri=http%3A%2F%2Flocalhost:8080&scope=scope&state=fake_state");
        OAuth2Authorization auth =  new OAuth2Authorization();
        auth.setCode("obtained_code");
        auth.setState("fake_state");
        when(service.extractAuthorization(eq("http://localhost:8080?code=obtained_code&state=fake_state")))
                .thenReturn(auth);
        when(service.getAccessToken(any(AccessTokenRequestParams.class))).thenReturn(response);
        return service;
    }
}
