package io.swagger.petstore.test;

import io.swagger.TestUtils;

import io.swagger.client.ApiClient;
import io.swagger.client.api.*;
import io.swagger.client.model.*;

import java.util.Map;

import org.junit.*;

import retrofit2.Response;
import static org.junit.Assert.*;

public class StoreApiTest {
    StoreApi api = null;

    @Before
    public void setup() {
        api = new ApiClient().createService(StoreApi.class);
    }

    @Test
    public void testGetInventory() throws Exception {
        Map<String, Integer> inventory = api.getInventory().execute().body();
        assertTrue(inventory.keySet().size() > 0);
    }

    @Test
    public void testPlaceOrder() throws Exception {
        Order order = createOrder();
        api.placeOrder(order).execute();

        Order fetched = api.getOrderById(String.valueOf(order.getId())).execute().body();
        assertEquals(order.getId(), fetched.getId());
        assertEquals(order.getPetId(), fetched.getPetId());
        assertEquals(order.getQuantity(), fetched.getQuantity());
    }

    @Test
    public void testDeleteOrder() throws Exception {
        Order order = createOrder();
        Response<Order> aa = api.placeOrder(order).execute();

        Order fetched = api.getOrderById(String.valueOf(order.getId())).execute().body();
        assertEquals(fetched.getId(), order.getId());

        api.deleteOrder(String.valueOf(order.getId())).execute();

        api.getOrderById(String.valueOf(order.getId())).execute();
        //also in retrofit 1 should return an error but don't, check server api impl.
    }

    private Order createOrder() {
        Order order = new Order();
        order.setId(new Long(TestUtils.nextId()));
        order.setPetId(new Long(200));
        order.setQuantity(new Integer(13));
        order.setShipDate(new java.util.Date());
        order.setStatus(Order.StatusEnum.PLACED);
        order.setComplete(true);

        return order;
    }
}
