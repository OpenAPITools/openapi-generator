package io.swagger.petstore.test;

import io.swagger.client.ApiClient;
import io.swagger.client.api.*;
import io.swagger.client.model.*;

import java.util.Map;

import org.junit.*;

import retrofit.Response;
import static org.junit.Assert.*;

public class StoreApiTest {
    StoreApi api = null;

    @Before
    public void setup() {
        api = new ApiClient().createService(StoreApi.class);
    }

    @Test
    public void testGetInventory() throws Exception {
        api.getInventory().subscribe(inventory -> {
            assertTrue(inventory.keySet().size() > 0);
        });
    }

    @Test
    public void testPlaceOrder() throws Exception {
        Order order = createOrder();
        api.placeOrder(order).subscribe(placed -> {});
        api.getOrderById(String.valueOf(order.getId())).subscribe(fetched -> {
            assertEquals(order.getId(), fetched.getId());
            assertEquals(order.getPetId(), fetched.getPetId());
            assertEquals(order.getQuantity(), fetched.getQuantity());
        });
    }

    @Test
    public void testDeleteOrder() throws Exception {
        Order order = createOrder();
        api.placeOrder(order).subscribe(order1 -> {
        });

        api.getOrderById(String.valueOf(order.getId())).subscribe(fetched -> {
            assertEquals(fetched.getId(), order.getId());
        });

        api.deleteOrder(String.valueOf(order.getId())).subscribe(aVoid -> {
        });
        api.getOrderById(String.valueOf(order.getId())).subscribe(
                order1 -> {
                    throw new RuntimeException("Should not have found deleted order.");
                },
                error -> {}
        );
    }

    private Order createOrder() {
        Order order = new Order();
        order.setId(new Long(System.currentTimeMillis()));
        order.setPetId(new Long(200));
        order.setQuantity(new Integer(13));
        order.setShipDate(new java.util.Date());
        order.setStatus(Order.StatusEnum.PLACED);
        order.setComplete(true);

        return order;
    }
}