package org.openapitools.client.auth;

import static org.junit.jupiter.api.Assertions.*;

import com.sun.net.httpserver.HttpServer;

import java.io.IOException;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import okhttp3.FormBody;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.Response;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * The okhttp library's RetryingOAuth was rewritten without Apache Oltu: it now builds its own
 * token request via the {@link RetryingOAuth.TokenRequestBuilder} inner class and no longer
 * exposes an {@code OAuthClient}/{@code OAuthOkHttpClient} seam to mock. Since this sample has
 * no mocking library available, this test exercises the interceptor's retry-once behavior (and
 * the token request it builds) against a real, local HTTP server instead of a mocked chain.
 */
public class RetryingOAuthTest {

    private HttpServer server;
    private String baseUrl;
    private RetryingOAuth oauth;
    private OkHttpClient httpClient;

    @BeforeEach
    public void setUp() throws Exception {
        server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        server.setExecutor(Executors.newCachedThreadPool());

        // Token endpoint: always issues "new-access-token".
        server.createContext("/token", exchange -> {
            try {
                // small delay so two concurrent 401s are likely to overlap on the
                // synchronized updateAccessToken() critical section
                Thread.sleep(300);
            } catch (InterruptedException ignored) {
                Thread.currentThread().interrupt();
            }
            byte[] body = "{\"access_token\":\"new-access-token\",\"token_type\":\"Bearer\"}"
                    .getBytes(StandardCharsets.UTF_8);
            exchange.getResponseHeaders().add("Content-Type", "application/json");
            exchange.sendResponseHeaders(HttpURLConnection.HTTP_OK, body.length);
            try (OutputStream os = exchange.getResponseBody()) {
                os.write(body);
            }
        });

        // Protected resource: 200 only when called with the fresh access token, 401 otherwise.
        server.createContext("/api", exchange -> {
            String authHeader = exchange.getRequestHeaders().getFirst("Authorization");
            int code = "Bearer new-access-token".equals(authHeader)
                    ? HttpURLConnection.HTTP_OK
                    : HttpURLConnection.HTTP_UNAUTHORIZED;
            exchange.sendResponseHeaders(code, -1);
            exchange.close();
        });

        server.start();
        int port = server.getAddress().getPort();
        baseUrl = "http://127.0.0.1:" + port;

        oauth = new RetryingOAuth(
                baseUrl + "/token",
                "_clientId",
                OAuthFlow.ACCESS_CODE,
                "_clientSecret",
                Collections.<String, String>emptyMap());
        oauth.setAccessToken("expired-access-token");

        httpClient = new OkHttpClient.Builder().addInterceptor(oauth).build();
    }

    @AfterEach
    public void tearDown() {
        server.stop(0);
    }

    @Test
    public void testSingleRequestUnauthorized() throws Exception {
        try (Response response = httpClient.newCall(new Request.Builder().url(baseUrl + "/api").build()).execute()) {
            assertEquals(HttpURLConnection.HTTP_OK, response.code());
        }
    }

    @Test
    public void testTwoConcurrentRequestsUnauthorized() throws Exception {
        Callable<Integer> callable = () -> {
            try (Response response = httpClient.newCall(new Request.Builder().url(baseUrl + "/api").build()).execute()) {
                return response.code();
            }
        };
        ExecutorService executor = Executors.newFixedThreadPool(2);
        try {
            Future<Integer> response1 = executor.submit(callable);
            Future<Integer> response2 = executor.submit(callable);

            assertEquals(HttpURLConnection.HTTP_OK, response1.get(10, TimeUnit.SECONDS).intValue());
            assertEquals(HttpURLConnection.HTTP_OK, response2.get(10, TimeUnit.SECONDS).intValue());
        } finally {
            executor.shutdown();
        }
    }

    @Test
    public void testTokenRequestBuilderIncludesGrantTypeAndCredentials() throws Exception {
        Request tokenRequest = oauth.getTokenRequestBuilder().build();
        assertEquals(baseUrl + "/token", tokenRequest.url().toString());
        assertEquals("POST", tokenRequest.method());

        okhttp3.RequestBody body = tokenRequest.body();
        assertNotNull(body);
        assertTrue(body instanceof FormBody);
        FormBody formBody = (FormBody) body;
        Map<String, String> formParams = new HashMap<>();
        for (int i = 0; i < formBody.size(); i++) {
            formParams.put(formBody.encodedName(i), formBody.encodedValue(i));
        }

        // setFlow(OAuthFlow.ACCESS_CODE) (via the constructor) maps to "authorization_code"
        assertEquals("authorization_code", formParams.get("grant_type"));
        assertEquals("_clientId", formParams.get("client_id"));
        assertEquals("_clientSecret", formParams.get("client_secret"));
    }
}
