package org.openapitools.client.api;

import com.github.tomakehurst.wiremock.WireMockServer;
import com.github.tomakehurst.wiremock.stubbing.Scenario;
import feign.FeignException;
import feign.RetryableException;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.openapitools.client.ApiClient;
import org.openapitools.client.auth.OauthClientCredentialsGrant;

import java.util.Map;

import static com.github.tomakehurst.wiremock.client.WireMock.*;
import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.options;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class OauthRetryAuthErrorsTest {

    private static StoreApi api;

    private WireMockServer wm = new WireMockServer(options().dynamicPort());

    @BeforeEach
    void setup() {
        wm.start();

        ApiClient apiClient = new ApiClient();
        apiClient.setBasePath(wm.baseUrl());

        OauthClientCredentialsGrant oauthClientCredentialsGrant = new OauthClientCredentialsGrant(null, wm.baseUrl() + "/token", "read");
        oauthClientCredentialsGrant.configure("client_id", "client_secret");
        apiClient.addAuthorization("oauth", oauthClientCredentialsGrant);

        api = apiClient.buildClient(StoreApi.class);
    }

    @AfterEach
    void shutdown() {
        wm.shutdown();
    }

    @Test
    void retryableAuthenticationException() {
        //First request to fetch the token returns an already expired token
        //Just to mock the scenario where already have a token in memory but it expires before the request reaches the server
        wm.stubFor(post(urlEqualTo("/token"))
                .withRequestBody(equalTo("client_id=client_id&client_secret=client_secret&scope=read&grant_type=client_credentials"))
                .inScenario("Retry token")
                .whenScenarioStateIs(Scenario.STARTED)
                .willReturn(ok("{\n" +
                        "  \"access_token\":\"EXPIRED_TOKEN\",\n" +
                        "  \"token_type\":\"bearer\",\n" +
                        "  \"expires_in\":0,\n" +
                        "  \"refresh_token\":\"IwOGYzYTlmM2YxOTQ5MGE3YmNmMDFkNTVk\",\n" +
                        "  \"scope\":\"read\"\n" +
                        "}")));

        //This token request will be triggered by the RetryableException
        wm.stubFor(post(urlEqualTo("/token"))
                .withRequestBody(equalTo("client_id=client_id&client_secret=client_secret&scope=read&grant_type=client_credentials"))
                .inScenario("Retry token")
                .whenScenarioStateIs("After fetching new token")
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
                .withHeader("Authorization", equalTo("Bearer EXPIRED_TOKEN"))
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
        wm.verify(exactly(2), postRequestedFor(urlEqualTo("/token")));
    }

    @Test
    void retryableAuthenticationExhaustedRetries() {
        //First request to fetch the token returns an already expired token
        //Just to mock the scenario where already have a token in memory but it expires before the request reaches the server
        wm.stubFor(post(urlEqualTo("/token"))
                .withRequestBody(equalTo("client_id=client_id&client_secret=client_secret&scope=read&grant_type=client_credentials"))
                .inScenario("Retry token")
                .whenScenarioStateIs(Scenario.STARTED)
                .willReturn(ok("{\n" +
                        "  \"access_token\":\"EXPIRED_TOKEN\",\n" +
                        "  \"token_type\":\"bearer\",\n" +
                        "  \"expires_in\":0,\n" +
                        "  \"refresh_token\":\"IwOGYzYTlmM2YxOTQ5MGE3YmNmMDFkNTVk\",\n" +
                        "  \"scope\":\"read\"\n" +
                        "}")));

        //This token request will be triggered by the RetryableException
        wm.stubFor(post(urlEqualTo("/token"))
                .withRequestBody(equalTo("client_id=client_id&client_secret=client_secret&scope=read&grant_type=client_credentials"))
                .inScenario("Retry token")
                .whenScenarioStateIs("After fetching new token")
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
                .withHeader("Authorization", equalTo("Bearer EXPIRED_TOKEN"))
                .willReturn(aResponse().withStatus(401)));

        //Second request also fails with a 401, in this case the 401 is not related with an expired token
        wm.stubFor(get(urlEqualTo("/store/inventory"))
                .inScenario("Retry token")
                .whenScenarioStateIs("After fetching new token")
                .withHeader("Accept", equalTo("application/json"))
                .withHeader("Authorization", equalTo("Bearer MTQ0NjJkZmQ5OTM2NDE1ZTZjNGZmZjI3"))
                .willReturn(aResponse().withStatus(401)));

        RetryableException retryableException = assertThrows(RetryableException.class, () -> api.getInventory());
        assertThat(retryableException.getCause(), is(instanceOf(FeignException.Unauthorized.class)));

        wm.verify(exactly(2), getRequestedFor(urlEqualTo("/store/inventory")));
        wm.verify(exactly(2), postRequestedFor(urlEqualTo("/token")));
    }
}
