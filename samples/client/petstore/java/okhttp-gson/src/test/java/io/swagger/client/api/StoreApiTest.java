package io.swagger.client.api;

import io.swagger.TestUtils;
import io.swagger.client.ApiException;

import io.swagger.client.auth.*;
import io.swagger.client.model.*;

import java.lang.reflect.Field;
import java.util.Map;

import org.junit.*;
import org.threeten.bp.OffsetDateTime;

import static org.junit.Assert.*;

public class StoreApiTest {
    StoreApi api = null;

    @Before
    public void setup() {
        api = new StoreApi();
        // setup authentication
        ApiKeyAuth apiKeyAuth = (ApiKeyAuth) api.getApiClient().getAuthentication("api_key");
        apiKeyAuth.setApiKey("special-key");
    }

    @Test
    public void testGetInventory() throws Exception {
        Map<String, Integer> inventory = api.getInventory();
        assertTrue(inventory.keySet().size() > 0);
    }

    /*
    @Test
    public void testGetInventoryInObject() throws Exception {
        Object inventoryObj = api.getInventoryInObject();
        assertTrue(inventoryObj instanceof Map);

        Map inventoryMap = (Map) inventoryObj;
        assertTrue(inventoryMap.keySet().size() > 0);

        Map.Entry firstEntry = (Map.Entry) inventoryMap.entrySet().iterator().next();
        assertTrue(firstEntry.getKey() instanceof String);
        // NOTE: Gson parses integer value to double.
        assertTrue(firstEntry.getValue() instanceof Double);
    }
    */

    @Test
    public void testPlaceOrder() throws Exception {
        Order order = createOrder();
        api.placeOrder(order);

        Order fetched = api.getOrderById(order.getId());
        assertEquals(order.getId(), fetched.getId());
        assertEquals(order.getPetId(), fetched.getPetId());
        assertEquals(order.getQuantity(), fetched.getQuantity());
        assertTrue(order.getShipDate().isEqual(fetched.getShipDate()));
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
        } catch (ApiException e) {
            // ok
        }
    }

    private Order createOrder() {
        Order order = new Order();
        order.setPetId(200L);
        order.setQuantity(13);
        //Ensure 3 fractional digits because of a bug in the petstore server
        order.setShipDate(OffsetDateTime.now().withNano(123000000));
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
