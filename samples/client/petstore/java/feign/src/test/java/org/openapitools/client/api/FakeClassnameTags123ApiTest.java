package org.openapitools.client.api;

import com.github.tomakehurst.wiremock.WireMockServer;
import com.github.tomakehurst.wiremock.common.Slf4jNotifier;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.openapitools.client.ApiClient;
import org.openapitools.client.model.Client;

import static com.github.tomakehurst.wiremock.client.WireMock.*;
import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.options;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

/**
 * API tests for FakeClassnameTags123Api
 */
class FakeClassnameTags123ApiTest {

  private static WireMockServer wm = new WireMockServer(options().dynamicPort().notifier(new Slf4jNotifier(true)));

  private FakeClassnameTags123Api api;

  @BeforeAll
  static void startWireMock() {
    wm.start();
  }

  @AfterAll
  static void stopWireMock() {
    wm.shutdown();
  }

  @BeforeEach
  void setUp() {
    ApiClient apiClient = new ApiClient();
    apiClient.setBasePath(wm.baseUrl());
    api = apiClient.buildClient(FakeClassnameTags123Api.class);
  }

  /**
   * To test class name in snake case
   * <p>
   * To test class name in snake case
   */
  @Test
  void testClassnameTest() {
    String responseBody = "{\n" +
            "   \"client\":\"Bruce\"\n" +
            "}";
    wm.stubFor(patch(urlEqualTo("/fake_classname_test"))
            .withHeader("Content-Type", equalTo("application/json"))
            .withHeader("Accept", equalTo("application/json"))
            .willReturn(ok(responseBody)));

    Client client = new Client();
    client.setClient("Bruce");

    Client response = api.testClassname(client);
    assertThat(response, is(client));
  }


}
