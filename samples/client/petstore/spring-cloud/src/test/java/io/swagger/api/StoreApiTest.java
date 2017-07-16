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
import org.threeten.bp.OffsetDateTime;

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
        return new Order()
                .id(TestUtils.nextId())
                .petId(200L)
                .quantity(13)
                //Ensure 3 fractional digits because of a bug in the petstore server
                .shipDate(OffsetDateTime.now().withNano(123000000))
                .status(Order.StatusEnum.PLACED)
                .complete(true);
    }

}
