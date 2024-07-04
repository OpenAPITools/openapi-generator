package test_error_handling_server_stubs;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.openapi.example.api.StoreApi;
import org.openapi.example.model.Order;
import org.springframework.core.codec.DecodingException;
import org.springframework.web.reactive.function.client.WebClientResponseException;

import java.util.Map;

import static helper.ApiClientFactories.setUpStoreApi;
import static helper.TestingHelper.*;
import static org.junit.jupiter.api.Assertions.*;

class StoreApiTest {

    StoreApi apiInstance;

    @BeforeEach
    void setUp() {
        apiInstance = setUpStoreApi(1236);
    }

    @Test
    void deleteOrderThrowsApiException() {
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.deleteOrder("ThrowsApiException").block();
                });
        approveException(exception);
    }

    @Test
    void deleteOrderThrowsStdExceptionDerivedException() {
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.deleteOrder("ThrowsStdExceptionDerivedException").block();
                });
        approveException(exception);
    }

    @Test
    void deleteOrderThrowsInt() {
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.deleteOrder("ThrowsInt").block();
                });
        approveException(exception);
    }

    @Test
    void deleteOrderReturnsStatus200() {
        apiInstance.deleteOrder("ReturnsStatus200").block();
    }

    @Test
    void deleteOrderReturnsStatus400() {
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.deleteOrder("ReturnsStatus400").block();
                });

        approveException(exception);
    }

    @Test
    void deleteOrderReturnsStatus404() {
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.deleteOrder("ReturnsStatus404").block();
                });

        approveException(exception);
    }
    @Test
    void deleteOrderReturnsStatus405() {
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.deleteOrder("ReturnsStatus405").block();
                });

        approveException(exception);
    }

    @Test
    void getInventory() {
        Map<String, Integer> ret = apiInstance.getInventory().block();
        assertNull(ret);
    }

    @Test
    void getOrderByIdThrowsApiException() {
        long id = errorRaisingStringToInt("ThrowsApiException");
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.getOrderById(id).block();
                });
        approveException(exception);
    }

    @Test
    void getOrderByIdThrowsStdExceptionDerivedException() {
        long id = errorRaisingStringToInt("ThrowsStdExceptionDerivedException");
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.getOrderById(id).block();
                });
        approveException(exception);
    }

    @Test
    void getOrderByIdThrowsInt() {
        long id = errorRaisingStringToInt("ThrowsInt");
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.getOrderById(id).block();
                });
        approveException(exception);
    }

    @Test
    void getOrderByIdReturnsStatus200WithInvalidEnumValue() {
        long id = errorRaisingStringToInt("ReturnsStatus200");
        DecodingException exception = assertThrows(DecodingException.class, () -> {
                apiInstance.getOrderById(id).block();
        });
    }

    @Test
    void getOrderByIdReturnsStatus300() {
        long id = errorRaisingStringToInt("ReturnsStatus300");
        Order resp = apiInstance.getOrderById(id).block();
        assertNull(resp);
    }

    @Test
    void getOrderByIdReturnsStatus400() {
        long id = errorRaisingStringToInt("ReturnsStatus400");
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.getOrderById(id).block();
                });
        approveException(exception);
    }

    @Test
    void getOrderByIdReturnsStatus404() {
        long id = errorRaisingStringToInt("ReturnsStatus404");
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.getOrderById(id).block();
                });
        approveException(exception);
    }

    @Test
    void placeOrderThrowsApiException() {
        long id = errorRaisingStringToInt("ThrowsApiException");

        Order order = new Order();
        order.setId(id);
        order.setStatus(Order.StatusEnum.PLACED);

        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.placeOrder(order).block();
                });
        approveException(exception);

    }

    @Test
    void placeOrderThrowsStdExceptionDerivedException() {
        long id = errorRaisingStringToInt("ThrowsStdExceptionDerivedException");

        Order order = new Order();
        order.setId(id);
        order.setStatus(Order.StatusEnum.PLACED);

        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.placeOrder(order).block();
                });
        approveException(exception);

    }

    @Test
    void placeOrderThrowsInt() {
        long id = errorRaisingStringToInt("ThrowsInt");

        Order order = new Order();
        order.setId(id);
        order.setStatus(Order.StatusEnum.PLACED);

        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.placeOrder(order).block();
                });
        approveException(exception);

    }

    @Test
    void placeOrderReturnsStatus300() {
        long id = errorRaisingStringToInt("ReturnsStatus300");

        Order order = new Order();
        order.setId(id);
        order.setStatus(Order.StatusEnum.PLACED);

        Order resp = apiInstance.placeOrder(order).block();

        assertNull(resp);
    }

    @Test
    void placeOrderReturnsStatus400() {
        long id = errorRaisingStringToInt("ReturnsStatus400");

        Order order = new Order();
        order.setId(id);
        order.setStatus(Order.StatusEnum.PLACED);

        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.placeOrder(order).block();
                });
        approveException(exception);
    }
}