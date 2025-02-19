package test_default_unimplemented_server_stubs;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.openapi.example.api.StoreApi;
import org.openapi.example.model.Order;
import org.openapi.example.invoker.ApiClient;
import org.springframework.web.reactive.function.client.WebClientResponseException;

import static helper.ApiClientFactories.setUpStoreApi;
import static org.junit.jupiter.api.Assertions.*;
import static helper.TestingHelper.approveException;

class StoreApiTest {

    StoreApi apiInstance;

    @BeforeEach
    void setUp() {
        apiInstance = setUpStoreApi(1234);
    }

    @Test
    void deleteOrder() {
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.deleteOrder("myOrderId").block();
                });

        approveException(exception);
    }

    @Test
    void getInventory() {
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.getInventory().block();
                });

        approveException(exception);
    }

    @Test
    void getOrderById() {
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.getOrderById(1L).block();
                });

        approveException(exception);
    }

    @Test
    void placeOrder() {
        Order order = new Order();
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.placeOrder(order).block();
                });

        approveException(exception);
    }
}