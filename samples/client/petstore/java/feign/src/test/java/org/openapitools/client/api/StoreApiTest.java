package org.openapitools.client.api;

import com.github.tomakehurst.wiremock.WireMockServer;
import feign.FeignException;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.openapitools.client.ApiClient;
import org.openapitools.client.model.Order;
import org.threeten.bp.OffsetDateTime;
import org.threeten.bp.ZoneOffset;

import java.util.Map;

import static com.github.tomakehurst.wiremock.client.WireMock.*;
import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.options;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * API tests for StoreApi
 */
public class StoreApiTest {

  private static StoreApi api;

  private static WireMockServer wm = new WireMockServer(options().dynamicPort());

  @BeforeAll
  static void setup() {
    wm.start();

    ApiClient apiClient = new ApiClient();
    apiClient.setBasePath(wm.baseUrl());
    api = apiClient.buildClient(StoreApi.class);

  }

  @AfterAll
  static void shutdown() {
    wm.shutdown();
  }


  /**
   * Delete purchase order by ID
   * <p>
   * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
   */
  @Test
  void deleteOrderTest() {
    wm.stubFor(delete(urlEqualTo("/store/order/1234"))
            .withHeader("Accept", equalTo("application/json"))
            .willReturn(ok()));

    api.deleteOrder("1234");
  }

  @Test
  void deleteOrderTestInvalid() {
    wm.stubFor(delete(urlEqualTo("/store/order/abc"))
            .withHeader("Accept", equalTo("application/json"))
            .willReturn(aResponse().withStatus(400)));

    assertThrows(FeignException.BadRequest.class, () -> api.deleteOrder("abc"));
  }

  /**
   * Returns pet inventories by status
   * <p>
   * Returns a map of status codes to quantities
   */
  @Test
  void getInventoryTest() {
    wm.stubFor(get(urlEqualTo("/store/inventory"))
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


  /**
   * Find purchase order by ID
   * <p>
   * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
   */
  @Test
  void getOrderByIdTest() {
    String responseBody = "{\n" +
            "  \"id\": 1,\n" +
            "  \"petId\": 1,\n" +
            "  \"quantity\": 10,\n" +
            "  \"shipDate\": \"2120-03-23T01:23:44.000000009+0000\",\n" +
            "  \"status\": \"placed\",\n" +
            "  \"complete\": true\n" +
            "}";

    wm.stubFor(get(urlEqualTo("/store/order/123"))
            .withHeader("Accept", equalTo("application/json"))
            .willReturn(ok(responseBody)));

    Order order = api.getOrderById(123L);

    assertThat(order.getId(), is(1L));
    assertThat(order.getPetId(), is(1L));
    assertThat(order.getQuantity(), is(10));
    assertThat(order.getShipDate(), is(OffsetDateTime.of(2120, 03, 23, 01, 23, 44, 9, ZoneOffset.UTC)));
    assertThat(order.getStatus(), is(Order.StatusEnum.PLACED));
    assertThat(order.isComplete(), is(true));
  }


  /**
   * Place an order for a pet
   */
  @Test
  void placeOrderTest() {
    String responseBody = "{\n" +
            "  \"id\": 1,\n" +
            "  \"petId\": 1,\n" +
            "  \"quantity\": 10,\n" +
            "  \"shipDate\": \"2120-03-23T01:23:44.000000009+0000\",\n" +
            "  \"status\": \"placed\",\n" +
            "  \"complete\": true\n" +
            "}";

    Order newOrder = new Order();
    newOrder.setId(1L);
    newOrder.setPetId(1L);
    newOrder.setQuantity(10);
    newOrder.shipDate(OffsetDateTime.of(2120, 03, 23, 01, 23, 44, 9, ZoneOffset.UTC));
    newOrder.setStatus(Order.StatusEnum.PLACED);
    newOrder.setComplete(Boolean.TRUE);

    wm.stubFor(post(urlEqualTo("/store/order"))
            .withHeader("Accept", equalTo("application/json"))
            .willReturn(ok(responseBody)));

    Order order = api.placeOrder(newOrder);

    assertThat(order, is(newOrder));
  }

}
