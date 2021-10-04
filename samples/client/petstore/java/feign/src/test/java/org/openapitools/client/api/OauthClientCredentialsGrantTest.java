package org.openapitools.client.api;

import com.github.tomakehurst.wiremock.WireMockServer;
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

public class OauthClientCredentialsGrantTest {

    private static StoreApi api;

    private static WireMockServer wm = new WireMockServer(options().dynamicPort());

    @BeforeAll
    static void setup() {
        wm.start();

        ApiClient apiClient = new ApiClient();
        apiClient.setBasePath(wm.baseUrl());

        OauthClientCredentialsGrant oauthClientCredentialsGrant = new OauthClientCredentialsGrant(wm.baseUrl() + "/auth", wm.baseUrl() + "/token", "read");
        oauthClientCredentialsGrant.configure("client_id", "client_secret");
        apiClient.addAuthorization("oauth", oauthClientCredentialsGrant);

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

        wm.stubFor(get(urlEqualTo("/store/inventory"))
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

        wm.verify(exactly(1), getRequestedFor(urlEqualTo("/store/inventory")));
        wm.verify(exactly(1), postRequestedFor(urlEqualTo("/token")));
        assertThat(wm.getAllServeEvents().size(), is(2));
    }
}
