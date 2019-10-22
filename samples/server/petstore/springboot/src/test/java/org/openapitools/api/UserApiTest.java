package org.openapitools.api;
import java.util.List;
import org.openapitools.model.User;
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
class UserApiTest {

    @Autowired
    private MockMvc mockmvc;

    @Test
    void getUserByName200Test() throws Exception {
        String username = "username_example";
        mockmvc.perform(
                get("/user/{username}", username)
            )
            .andExpect(status().is(200));
    }

    @Test
    void getUserByName200Test() throws Exception {
        String username = "username_example";
        mockmvc.perform(
                get("/user/{username}", username)
            )
            .andExpect(status().is(200));
    }

    @Test
    void getUserByName400Test() throws Exception {
        String username = "username_example";
        mockmvc.perform(
                get("/user/{username}", username)
            )
            .andExpect(status().is(400));
    }

    @Test
    void getUserByName400Test() throws Exception {
        String username = "username_example";
        mockmvc.perform(
                get("/user/{username}", username)
            )
            .andExpect(status().is(400));
    }

    @Test
    void getUserByName404Test() throws Exception {
        String username = "username_example";
        mockmvc.perform(
                get("/user/{username}", username)
            )
            .andExpect(status().is(404));
    }

    @Test
    void getUserByName404Test() throws Exception {
        String username = "username_example";
        mockmvc.perform(
                get("/user/{username}", username)
            )
            .andExpect(status().is(404));
    }

    @Test
    void loginUser200Test() throws Exception {
        String username = "username_example";
        String password = "password_example";
        mockmvc.perform(
                get("/user/login", username, password)
            )
            .andExpect(status().is(200));
    }

    @Test
    void loginUser200Test() throws Exception {
        String username = "username_example";
        String password = "password_example";
        mockmvc.perform(
                get("/user/login", username, password)
            )
            .andExpect(status().is(200));
    }

    @Test
    void loginUser400Test() throws Exception {
        String username = "username_example";
        String password = "password_example";
        mockmvc.perform(
                get("/user/login", username, password)
            )
            .andExpect(status().is(400));
    }

    @Test
    void loginUser400Test() throws Exception {
        String username = "username_example";
        String password = "password_example";
        mockmvc.perform(
                get("/user/login", username, password)
            )
            .andExpect(status().is(400));
    }
}