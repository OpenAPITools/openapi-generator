package io.swagger.petstore.test;

import feign.FeignException;

import io.swagger.TestUtils;

import io.swagger.client.*;
import io.swagger.client.api.*;
import io.swagger.client.model.*;

import java.util.Map;

import org.junit.*;
import static org.junit.Assert.*;

public class StoreApiTest {
    ApiClient apiClient;
    StoreApi api;

    @Before
    public void setup() {
        apiClient = new ApiClient();
        api = apiClient.buildClient(StoreApi.class);
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

        Order fetched = api.getOrderById(order.getId().toString());
        assertEquals(order.getId(), fetched.getId());
        assertEquals(order.getPetId(), fetched.getPetId());
        assertEquals(order.getQuantity(), fetched.getQuantity());
    }

    @Test
    public void testDeleteOrder() throws Exception {
        Order order = createOrder();
        api.placeOrder(order);

        Order fetched = api.getOrderById(order.getId().toString());
        assertEquals(fetched.getId(), order.getId());

        api.deleteOrder(order.getId().toString());

        try {
            api.getOrderById(order.getId().toString());
            fail("expected an error");
        } catch (FeignException e) {
            assertTrue(e.getMessage().startsWith("status 404 "));
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
