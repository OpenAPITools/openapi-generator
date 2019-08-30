package org.openapitools.api;
import java.util.Map;
import org.openapitools.model.Order;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.*;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.http.MediaType;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.web.servlet.MockMvc;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest
class StoreApiTest {

    @Autowired
    private MockMvc mockmvc;

    @Test
    void getInventory200Test() throws Exception {
        mockmvc.perform(
                get("/store/inventory")
            )
            .andExpect(status().is(200));
    }

    @Test
    void getOrderById200Test() throws Exception {
        Long orderId = 56L;
        mockmvc.perform(
                get("/store/order/{order_id}", orderId)
            )
            .andExpect(status().is(200));
    }

    @Test
    void getOrderById200Test() throws Exception {
        Long orderId = 56L;
        mockmvc.perform(
                get("/store/order/{order_id}", orderId)
            )
            .andExpect(status().is(200));
    }

    @Test
    void getOrderById400Test() throws Exception {
        Long orderId = 56L;
        mockmvc.perform(
                get("/store/order/{order_id}", orderId)
            )
            .andExpect(status().is(400));
    }

    @Test
    void getOrderById400Test() throws Exception {
        Long orderId = 56L;
        mockmvc.perform(
                get("/store/order/{order_id}", orderId)
            )
            .andExpect(status().is(400));
    }

    @Test
    void getOrderById404Test() throws Exception {
        Long orderId = 56L;
        mockmvc.perform(
                get("/store/order/{order_id}", orderId)
            )
            .andExpect(status().is(404));
    }

    @Test
    void getOrderById404Test() throws Exception {
        Long orderId = 56L;
        mockmvc.perform(
                get("/store/order/{order_id}", orderId)
            )
            .andExpect(status().is(404));
    }

    @Test
    void placeOrder200Test() throws Exception {
        Order body = new Order();
        mockmvc.perform(
                post("/store/order")
                    .contentType(MediaType.valueOf("application/xml"))
                    .content(new ObjectMapper().writeValueAsString(body))
            )
            .andExpect(status().is(200));
    }

    @Test
    void placeOrder200Test() throws Exception {
        Order body = new Order();
        mockmvc.perform(
                post("/store/order")
                    .contentType(MediaType.valueOf("application/json"))
                    .content(new ObjectMapper().writeValueAsString(body))
            )
            .andExpect(status().is(200));
    }

    @Test
    void placeOrder400Test() throws Exception {
        Order body = new Order();
        mockmvc.perform(
                post("/store/order")
                    .contentType(MediaType.valueOf("application/xml"))
                    .content(new ObjectMapper().writeValueAsString(body))
            )
            .andExpect(status().is(400));
    }

    @Test
    void placeOrder400Test() throws Exception {
        Order body = new Order();
        mockmvc.perform(
                post("/store/order")
                    .contentType(MediaType.valueOf("application/json"))
                    .content(new ObjectMapper().writeValueAsString(body))
            )
            .andExpect(status().is(400));
    }
}