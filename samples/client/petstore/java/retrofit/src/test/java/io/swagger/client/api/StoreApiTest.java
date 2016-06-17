package io.swagger.client.api;

import io.swagger.TestUtils;

import io.swagger.client.ApiClient;
import io.swagger.client.model.*;

import org.joda.time.DateTime;
import org.joda.time.DateTimeZone;
import retrofit.RetrofitError;

import java.lang.reflect.Field;
import java.util.Map;

import org.junit.*;
import static org.junit.Assert.*;

public class StoreApiTest {
    StoreApi api = null;

    @Before
    public void setup() {
        api = new ApiClient().createService(StoreApi.class);
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

        Order fetched = api.getOrderById(order.getId());
        assertEquals(order.getId(), fetched.getId());
        assertEquals(order.getPetId(), fetched.getPetId());
        assertEquals(order.getQuantity(), fetched.getQuantity());
        assertEquals(order.getShipDate().withZone(DateTimeZone.UTC), fetched.getShipDate().withZone(DateTimeZone.UTC));
    }

    @Test
    public void testDeleteOrder() throws Exception {
        Order order = createOrder();
        api.placeOrder(order);

        Order fetched = api.getOrderById(order.getId());
        assertEquals(fetched.getId(), order.getId());

        api.deleteOrder(String.valueOf(order.getId()));

        try {
            api.getOrderById(order.getId());
            // fail("expected an error");
        } catch (RetrofitError e) {
            // ok
        }
    }

    private Order createOrder() {
        Order order = new Order();
        order.setPetId(new Long(200));
        order.setQuantity(new Integer(13));
        order.setShipDate(DateTime.now());
        order.setStatus(Order.StatusEnum.PLACED);
        order.setComplete(true);

        try {
          Field idField = Order.class.getDeclaredField("id");
          idField.setAccessible(true);
          idField.set(order, TestUtils.nextId());
        } catch (Exception e) {
          throw new RuntimeException(e);
        }

        return order;
    }
}
