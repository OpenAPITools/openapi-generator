package io.swagger.api;

import feign.FeignException;
import io.swagger.TestUtils;
import io.swagger.model.Order;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.builder.SpringApplicationBuilder;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.cloud.netflix.feign.EnableFeignClients;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.lang.reflect.Field;
import java.util.Map;

import static org.junit.Assert.*;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(classes = StoreApiTest.Application.class)
public class StoreApiTest {

    @Autowired
    private StoreApiClient client;

    @Test
    public void testGetInventory() {
        Map<String, Integer> inventory = client.getInventory().getBody();
        assertTrue(inventory.keySet().size() > 0);
    }

    @Test
    public void testPlaceOrder() {
        Order order = createOrder();
        client.placeOrder(order);

        Order fetched = client.getOrderById(order.getId()).getBody();
        assertEquals(order.getId(), fetched.getId());
        assertEquals(order.getPetId(), fetched.getPetId());
        assertEquals(order.getQuantity(), fetched.getQuantity());
        assertEquals(order.getShipDate().toInstant(), fetched.getShipDate().toInstant());
    }

    @Test
    public void testDeleteOrder() {
        Order order = createOrder();
        client.placeOrder(order);

        Order fetched = client.getOrderById(order.getId()).getBody();
        assertEquals(fetched.getId(), order.getId());

        client.deleteOrder(String.valueOf(order.getId()));

        try {
            client.getOrderById(order.getId());
            fail("expected an error");
        } catch (FeignException e) {
            assertTrue(e.getMessage().startsWith("status 404 "));
        }
    }

    private Order createOrder() {
        Order order = new Order();
        order.setPetId(new Long(200));
        order.setQuantity(new Integer(13));
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

    @SpringBootApplication
    @EnableFeignClients
    protected static class Application {
        public static void main(String[] args) {
            new SpringApplicationBuilder(StoreApiTest.Application.class).run(args);
        }
    }
}
