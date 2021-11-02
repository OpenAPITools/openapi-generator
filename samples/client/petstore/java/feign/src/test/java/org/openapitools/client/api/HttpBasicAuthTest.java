package org.openapitools.client.api;

import com.github.tomakehurst.wiremock.WireMockServer;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.openapitools.client.ApiClient;
import org.openapitools.client.auth.HttpBasicAuth;

import java.util.Map;

import static com.github.tomakehurst.wiremock.client.WireMock.*;
import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.options;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

public class HttpBasicAuthTest {

    private static StoreApi api;

    private static WireMockServer wm = new WireMockServer(options().dynamicPort());

    @BeforeAll
    static void setup() {
        wm.start();
    }

    @AfterAll
    static void shutdown() {
        wm.shutdown();
    }

    @Test
    void httpBasicAuth() {
        ApiClient apiClient = new ApiClient();
        apiClient.setBasePath(wm.baseUrl());

        HttpBasicAuth httpBasicAuth = new HttpBasicAuth();
        apiClient.addAuthorization("basic", httpBasicAuth);
        apiClient.setCredentials("username", "password");

        api = apiClient.buildClient(StoreApi.class);

        wm.stubFor(get(urlPathEqualTo("/store/inventory"))
                .withHeader("Authorization", equalTo("Basic dXNlcm5hbWU6cGFzc3dvcmQ="))
                .withHeader("Accept", equalTo("application/json"))
                .willReturn(ok("{\n" +
                        "  \"prop1\": 1,\n" +
                        "  \"prop2\": 2\n" +
                        "}")));

        Map<String, Integer> inventory = api.getInventory();

        assertThat(inventory.keySet().size(), is(2));
        assertThat(inventory.get("prop1"), is(1));
        assertThat(inventory.get("prop2"), is(2));
    }
}
