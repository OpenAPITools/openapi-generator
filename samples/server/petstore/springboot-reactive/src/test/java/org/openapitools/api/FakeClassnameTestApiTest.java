package org.openapitools.api;
import org.openapitools.model.Client;
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
class FakeClassnameTestApiTest {

    @Autowired
    private MockMvc mockmvc;

    @Test
    void testClassname200Test() throws Exception {
        Client body = new Client();
        mockmvc.perform(
                patch("/fake_classname_test")
                    .contentType(MediaType.valueOf("application/json"))
                    .content(new ObjectMapper().writeValueAsString(body))
            )
            .andExpect(status().is(200));
    }
}