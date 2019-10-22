package org.openapitools.api;
import java.util.List;
import org.openapitools.model.User;
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
public class UserApiTest {

    @Autowired
    private WebApplicationContext webAppContext;
    private MockMvc mockmvc;

    @Before
    public void setup() {
        mockmvc = MockMvcBuilders.webAppContextSetup(webAppContext).build();
    }

    @Test
    public void getUserByName200Test() throws Exception {
        String username = "username_example";
        mockmvc.perform(
                get("/user/{username}", username)
            )
            .andExpect(status().is(200));
    }

    @Test
    public void getUserByName200Test() throws Exception {
        String username = "username_example";
        mockmvc.perform(
                get("/user/{username}", username)
            )
            .andExpect(status().is(200));
    }

    @Test
    public void getUserByName400Test() throws Exception {
        String username = "username_example";
        mockmvc.perform(
                get("/user/{username}", username)
            )
            .andExpect(status().is(400));
    }

    @Test
    public void getUserByName400Test() throws Exception {
        String username = "username_example";
        mockmvc.perform(
                get("/user/{username}", username)
            )
            .andExpect(status().is(400));
    }

    @Test
    public void getUserByName404Test() throws Exception {
        String username = "username_example";
        mockmvc.perform(
                get("/user/{username}", username)
            )
            .andExpect(status().is(404));
    }

    @Test
    public void getUserByName404Test() throws Exception {
        String username = "username_example";
        mockmvc.perform(
                get("/user/{username}", username)
            )
            .andExpect(status().is(404));
    }

    @Test
    public void loginUser200Test() throws Exception {
        String username = "username_example";
        String password = "password_example";
        mockmvc.perform(
                get("/user/login", username, password)
            )
            .andExpect(status().is(200));
    }

    @Test
    public void loginUser200Test() throws Exception {
        String username = "username_example";
        String password = "password_example";
        mockmvc.perform(
                get("/user/login", username, password)
            )
            .andExpect(status().is(200));
    }

    @Test
    public void loginUser400Test() throws Exception {
        String username = "username_example";
        String password = "password_example";
        mockmvc.perform(
                get("/user/login", username, password)
            )
            .andExpect(status().is(400));
    }

    @Test
    public void loginUser400Test() throws Exception {
        String username = "username_example";
        String password = "password_example";
        mockmvc.perform(
                get("/user/login", username, password)
            )
            .andExpect(status().is(400));
    }
}