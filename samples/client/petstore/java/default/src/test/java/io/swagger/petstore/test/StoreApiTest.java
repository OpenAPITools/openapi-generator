package io.swagger.petstore.test;

import io.swagger.TestUtils;

import io.swagger.client.ApiException;

import io.swagger.client.*;
import io.swagger.client.api.*;
import io.swagger.client.auth.*;
import io.swagger.client.model.*;

import java.util.Map;
import java.text.SimpleDateFormat;

import org.junit.*;
import static org.junit.Assert.*;

public class StoreApiTest {
    StoreApi api = null;

    @Before
    public void setup() {
        api = new StoreApi();
        // setup authentication
        ApiKeyAuth apiKeyAuth = (ApiKeyAuth) api.getApiClient().getAuthentication("api_key");
        apiKeyAuth.setApiKey("special-key");
        // set custom date format that is used by the petstore server
        api.getApiClient().setDateFormat(new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ"));
    }

    @Test
    public void testGetInventory() throws Exception {
        Map<String, Integer> inventory = api.getInventory();
        assertTrue(inventory.keySet().size() > 0);
    }

    @Test
    public void testPlaceOrder() throws Exception {
        Order order = createOrder();
        api.placeOrder(order);

        Order fetched = api.getOrderById(String.valueOf(order.getId()));
        assertEquals(order.getId(), fetched.getId());
        assertEquals(order.getPetId(), fetched.getPetId());
        assertEquals(order.getQuantity(), fetched.getQuantity());
    }

    @Test
    public void testDeleteOrder() throws Exception {
        Order order = createOrder();
        api.placeOrder(order);

        Order fetched = api.getOrderById(String.valueOf(order.getId()));
        assertEquals(fetched.getId(), order.getId());

        api.deleteOrder(String.valueOf(order.getId()));

        try {
            api.getOrderById(String.valueOf(order.getId()));
            // fail("expected an error");
        } catch (ApiException e) {
            // ok
        }
    }

    private Order createOrder() {
        Order order = new Order();
        order.setId(TestUtils.nextId());
        order.setPetId(new Long(200));
        order.setQuantity(new Integer(13));
        order.setShipDate(new java.util.Date());
        order.setStatus(Order.StatusEnum.PLACED);
        order.setComplete(true);

        return order;
    }
}
