package test_simple_server_stubs;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.openapi.example.api.StoreApi;
import org.openapi.example.model.Order;

import java.util.Map;

import static helper.ApiClientFactories.setUpStoreApi;
import static helper.TestingHelper.approveResponseAsJson;
import static org.junit.jupiter.api.Assertions.*;

class StoreApiTest {

    StoreApi apiInstance;

    @BeforeEach
    void setUp() {
        apiInstance = setUpStoreApi(1235);
    }

    @Test
    void deleteOrder() {
        apiInstance.deleteOrder("myOrderId").block();
    }

    @Test
    void getInventory() {
        Map<String, Integer> inventory = apiInstance.getInventory().block();
        approveResponseAsJson(inventory);
    }

    @Test
    void getOrderById() {
        Order order = apiInstance.getOrderById(123L).block();
        approveResponseAsJson(order);
    }

    @Test
    void placeOrder() {
        Order order = new Order();
        order.setId(9876L);
        order.setStatus(Order.StatusEnum.PLACED);
        Order response = apiInstance.placeOrder(order).block();
        approveResponseAsJson(response);
    }
}