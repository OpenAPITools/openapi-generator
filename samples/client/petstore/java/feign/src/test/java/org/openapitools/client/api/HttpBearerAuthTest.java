package org.openapitools.client.api;

import com.github.tomakehurst.wiremock.WireMockServer;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.openapitools.client.ApiClient;
import org.openapitools.client.auth.HttpBearerAuth;

import java.util.Map;

import static com.github.tomakehurst.wiremock.client.WireMock.*;
import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.options;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

public class HttpBearerAuthTest {

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
    void httpBearer() {
        ApiClient apiClient = new ApiClient();
        apiClient.setBasePath(wm.baseUrl());

        HttpBearerAuth httpBearerAuth = new HttpBearerAuth("Bearer");
        apiClient.addAuthorization("bearer", httpBearerAuth);
        apiClient.setBearerToken("MTQ0NjJkZmQ5OTM2NDE1ZTZjNGZmZjI3");

        api = apiClient.buildClient(StoreApi.class);

        wm.stubFor(get(urlPathEqualTo("/store/inventory"))
                .withHeader("Authorization", equalTo("Bearer MTQ0NjJkZmQ5OTM2NDE1ZTZjNGZmZjI3"))
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
