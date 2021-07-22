package org.openapitools.client.api;

import com.github.tomakehurst.wiremock.WireMockServer;
import com.github.tomakehurst.wiremock.stubbing.Scenario;
import feign.RequestInterceptor;
import feign.RequestTemplate;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.openapitools.client.ApiClient;
import org.openapitools.client.auth.OauthClientCredentialsGrant;

import java.util.Map;

import static com.github.tomakehurst.wiremock.client.WireMock.*;
import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.options;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

public class OauthRetryAuthErrorsTest {

    private static StoreApi api;

    private static WireMockServer wm = new WireMockServer(options().dynamicPort());

    private static OauthClientCredentialsGrant oauthClientCredentialsGrant;

    @BeforeAll
    static void setup() {
        wm.start();

        ApiClient apiClient = new ApiClient();
        apiClient.setBasePath(wm.baseUrl());
        apiClient.getFeignBuilder().requestInterceptor(new TokenExpireMockInterceptor());

        oauthClientCredentialsGrant = new OauthClientCredentialsGrant(null, wm.baseUrl() + "/token", "read");
        oauthClientCredentialsGrant.configure("client_id", "client_secret");

        apiClient.addAuthorization("oauth", oauthClientCredentialsGrant);
        //Sets the access token to avoid mocking an extra invocation to the /token endpoint
        apiClient.setAccessToken("TOKEN_THAT_WILL_EXPIRE_BEFORE_THE_REQUEST_REACHES_THE_SERVER", 30);

        api = apiClient.buildClient(StoreApi.class);
    }

    @AfterAll
    static void shutdown() {
        wm.shutdown();
    }

    @Test
    void oauthClientCredentialsTest() {
        wm.stubFor(post(urlEqualTo("/token"))
                .withRequestBody(equalTo("client_id=client_id&client_secret=client_secret&scope=read&grant_type=client_credentials"))
                .willReturn(ok("{\n" +
                        "  \"access_token\":\"MTQ0NjJkZmQ5OTM2NDE1ZTZjNGZmZjI3\",\n" +
                        "  \"token_type\":\"bearer\",\n" +
                        "  \"expires_in\":3600,\n" +
                        "  \"refresh_token\":\"IwOGYzYTlmM2YxOTQ5MGE3YmNmMDFkNTVk\",\n" +
                        "  \"scope\":\"read\"\n" +
                        "}")));

        //First request will fail with a 401
        //Simulates a token that expired before reaching the server
        wm.stubFor(get(urlEqualTo("/store/inventory"))
                .inScenario("Retry token")
                .whenScenarioStateIs(Scenario.STARTED)
                .willSetStateTo("After fetching new token")
                .withHeader("Accept", equalTo("application/json"))
                .withHeader("Authorization", equalTo("Bearer TOKEN_THAT_WILL_EXPIRE_BEFORE_THE_REQUEST_REACHES_THE_SERVER"))
                .willReturn(aResponse().withStatus(401)));

        //The second request sends a newly generated token
        wm.stubFor(get(urlEqualTo("/store/inventory"))
                .inScenario("Retry token")
                .whenScenarioStateIs("After fetching new token")
                .withHeader("Accept", equalTo("application/json"))
                .withHeader("Authorization", equalTo("Bearer MTQ0NjJkZmQ5OTM2NDE1ZTZjNGZmZjI3"))
                .willReturn(ok("{\n" +
                        "  \"prop1\": 1,\n" +
                        "  \"prop2\": 2\n" +
                        "}")));

        Map<String, Integer> inventory = api.getInventory();

        assertThat(inventory.keySet().size(), is(2));
        assertThat(inventory.get("prop1"), is(1));
        assertThat(inventory.get("prop2"), is(2));

        wm.verify(exactly(2), getRequestedFor(urlEqualTo("/store/inventory")));
        wm.verify(exactly(1), postRequestedFor(urlEqualTo("/token")));
        wm.verify(exactly(1), postRequestedFor(urlEqualTo("/token")));
    }


    /**
     * Interceptor to simulate a token that expires before reaching the server
     * It marks the token as expired right after the first request, then the retryer will retry the request
     * and a new token should be fetched from the /token endpoint
     */
    private static class TokenExpireMockInterceptor implements RequestInterceptor {
        private boolean firstRequest = true;

        @Override
        public void apply(RequestTemplate template) {
            if (!firstRequest) {
                oauthClientCredentialsGrant.setAccessToken("TOKEN_THAT_WILL_EXPIRE_BEFORE_THE_REQUEST_REACHES_THE_SERVER", 0);
            }
            firstRequest = false;
        }
    }
}
