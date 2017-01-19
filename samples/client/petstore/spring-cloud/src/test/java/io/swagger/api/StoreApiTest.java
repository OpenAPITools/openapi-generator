package io.swagger.api;

import com.netflix.hystrix.exception.HystrixRuntimeException;
import io.swagger.Application;
import io.swagger.TestUtils;
import io.swagger.model.Order;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.lang.reflect.Field;
import java.util.Map;

import static org.junit.Assert.*;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringBootTest(classes = Application.class)
public class StoreApiTest {

    @Autowired
    private StoreApiClient client;

    @Test
    public void testGetInventory() {
        Map<String, Integer> inventory = client.getInventory().execute().getBody();
        assertTrue(inventory.keySet().size() > 0);
    }

    @Test
    public void testPlaceOrder() {
        Order order = createOrder();
        client.placeOrder(order).execute();

        Order fetched = client.getOrderById(order.getId()).execute().getBody();
        assertEquals(order.getId(), fetched.getId());
        assertEquals(order.getPetId(), fetched.getPetId());
        assertEquals(order.getQuantity(), fetched.getQuantity());
        assertEquals(order.getShipDate().toInstant(), fetched.getShipDate().toInstant());
    }

    @Test
    public void testDeleteOrder() {
        Order order = createOrder();
        client.placeOrder(order).execute();

        Order fetched = client.getOrderById(order.getId()).execute().getBody();
        assertEquals(fetched.getId(), order.getId());

        client.deleteOrder(String.valueOf(order.getId())).execute();

        try {
            client.getOrderById(order.getId()).execute();
            fail("expected an error");
        } catch (HystrixRuntimeException e) {
            assertTrue(e.getCause().getMessage().startsWith("status 404 "));
        }
    }

    private Order createOrder() {
        Order order = new Order();
        order.setPetId(200L);
        order.setQuantity(13);
        order.setShipDate(org.joda.time.DateTime.now());
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
