package org.openapitools.client.auth;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.net.HttpURLConnection;
import java.util.Collections;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import okhttp3.*;
import okhttp3.Interceptor.Chain;
import okhttp3.Response.Builder;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.apache.oltu.oauth2.client.OAuthClient;
import org.apache.oltu.oauth2.client.request.OAuthClientRequest;
import org.apache.oltu.oauth2.client.response.OAuthJSONAccessTokenResponse;
import org.apache.oltu.oauth2.common.exception.OAuthProblemException;
import org.apache.oltu.oauth2.common.exception.OAuthSystemException;
import org.junit.Before;
import org.junit.Test;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;

public class RetryingOAuthTest {

    private RetryingOAuth oauth;

    @Before
    public void setUp() throws Exception {
        oauth =
                new RetryingOAuth(
                        "_clientId",
                        "_clientSecret",
                        OAuthFlow.ACCESS_CODE,
                        "https://token.example.com",
                        Collections.<String, String>emptyMap());
        oauth.setAccessToken("expired-access-token");
        FieldUtils.writeField(oauth, "oAuthClient", mockOAuthClient(), true);
    }

    @Test
    public void testSingleRequestUnauthorized() throws Exception {
        Response response = oauth.intercept(mockChain());
        assertEquals(HttpURLConnection.HTTP_OK, response.code());
    }

    @Test
    public void testTwoConcurrentRequestsUnauthorized() throws Exception {

        Callable<Response> callable =
                new Callable<Response>() {
                    @Override
                    public Response call() throws Exception {
                        return oauth.intercept(mockChain());
                    }
                };
        ExecutorService executor = Executors.newFixedThreadPool(2);
        try {
            Future<Response> response1 = executor.submit(callable);
            Future<Response> response2 = executor.submit(callable);

            assertEquals(HttpURLConnection.HTTP_OK, response1.get().code());
            assertEquals(HttpURLConnection.HTTP_OK, response2.get().code());
        } finally {
            executor.shutdown();
        }
    }

    private OAuthClient mockOAuthClient() throws OAuthProblemException, OAuthSystemException {
        OAuthJSONAccessTokenResponse response = mock(OAuthJSONAccessTokenResponse.class);
        when(response.getAccessToken())
                .thenAnswer(
                        new Answer<String>() {
                            @Override
                            public String answer(InvocationOnMock invocation) throws Throwable {
                                // sleep ensures that the bug is triggered.
                                Thread.sleep(1000);
                                return "new-access-token";
                            }
                        });

        OAuthClient client = mock(OAuthClient.class);
        when(client.accessToken(any(OAuthClientRequest.class))).thenReturn(response);
        return client;
    }

    private Chain mockChain() throws IOException {
        Chain chain = mock(Chain.class);

        final Request request = new Request.Builder().url("https://api.example.com").build();
        when(chain.request()).thenReturn(request);

        when(chain.proceed(any(Request.class)))
                .thenAnswer(
                        new Answer<Response>() {
                            @Override
                            public Response answer(InvocationOnMock inv) {
                                Request r = inv.getArgument(0);
                                int responseCode =
                                        "Bearer new-access-token".equals(r.header("Authorization"))
                                                ? HttpURLConnection.HTTP_OK
                                                : HttpURLConnection.HTTP_UNAUTHORIZED;
                                return new Builder()
                                        .protocol(Protocol.HTTP_1_0)
                                        .message("sup")
                                        .request(request)
                                        .body(
                                                ResponseBody.create(
                                                        new byte[0],
                                                        MediaType.get("application/test")))
                                        .code(responseCode)
                                        .build();
                            }
                        });

        return chain;
    }
}
