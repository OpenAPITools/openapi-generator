package org.openapitools.api;
import java.util.Map;
import org.openapitools.model.Order;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;

import java.util.*;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.web.WebAppConfiguration;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;

import org.openapitools.configuration.OpenAPIUiConfiguration;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@RunWith(SpringJUnit4ClassRunner.class)
@WebAppConfiguration
@ContextConfiguration(classes = OpenAPIUiConfiguration.class)
public class StoreApiTest {

    @Autowired
    private WebApplicationContext webAppContext;
    private MockMvc mockmvc;

    @Before
    public void setup() {
        mockmvc = MockMvcBuilders.webAppContextSetup(webAppContext).build();
    }

    @Test
    public void getInventory200Test() throws Exception {
        mockmvc.perform(
                get("/store/inventory")
            )
            .andExpect(status().is(200));
    }

    @Test
    public void getOrderById200Test() throws Exception {
        Long orderId = 56L;
        mockmvc.perform(
                get("/store/order/{order_id}", orderId)
            )
            .andExpect(status().is(200));
    }

    @Test
    public void getOrderById200Test() throws Exception {
        Long orderId = 56L;
        mockmvc.perform(
                get("/store/order/{order_id}", orderId)
            )
            .andExpect(status().is(200));
    }

    @Test
    public void getOrderById400Test() throws Exception {
        Long orderId = 56L;
        mockmvc.perform(
                get("/store/order/{order_id}", orderId)
            )
            .andExpect(status().is(400));
    }

    @Test
    public void getOrderById400Test() throws Exception {
        Long orderId = 56L;
        mockmvc.perform(
                get("/store/order/{order_id}", orderId)
            )
            .andExpect(status().is(400));
    }

    @Test
    public void getOrderById404Test() throws Exception {
        Long orderId = 56L;
        mockmvc.perform(
                get("/store/order/{order_id}", orderId)
            )
            .andExpect(status().is(404));
    }

    @Test
    public void getOrderById404Test() throws Exception {
        Long orderId = 56L;
        mockmvc.perform(
                get("/store/order/{order_id}", orderId)
            )
            .andExpect(status().is(404));
    }

    @Test
    public void placeOrder200Test() throws Exception {
        Order body = new Order();
        mockmvc.perform(
                post("/store/order")
                    .contentType(MediaType.valueOf("application/xml"))
                    .content(new ObjectMapper().writeValueAsString(body))
            )
            .andExpect(status().is(200));
    }

    @Test
    public void placeOrder200Test() throws Exception {
        Order body = new Order();
        mockmvc.perform(
                post("/store/order")
                    .contentType(MediaType.valueOf("application/json"))
                    .content(new ObjectMapper().writeValueAsString(body))
            )
            .andExpect(status().is(200));
    }

    @Test
    public void placeOrder400Test() throws Exception {
        Order body = new Order();
        mockmvc.perform(
                post("/store/order")
                    .contentType(MediaType.valueOf("application/xml"))
                    .content(new ObjectMapper().writeValueAsString(body))
            )
            .andExpect(status().is(400));
    }

    @Test
    public void placeOrder400Test() throws Exception {
        Order body = new Order();
        mockmvc.perform(
                post("/store/order")
                    .contentType(MediaType.valueOf("application/json"))
                    .content(new ObjectMapper().writeValueAsString(body))
            )
            .andExpect(status().is(400));
    }
}