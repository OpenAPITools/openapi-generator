package org.openapitools.api;
import org.openapitools.model.Client;
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
public class FakeClassnameTestApiTest {

    @Autowired
    private WebApplicationContext webAppContext;
    private MockMvc mockmvc;

    @Before
    public void setup() {
        mockmvc = MockMvcBuilders.webAppContextSetup(webAppContext).build();
    }

    @Test
    public void testClassname200Test() throws Exception {
        Client body = new Client();
        mockmvc.perform(
                patch("/fake_classname_test")
                    .contentType(MediaType.valueOf("application/json"))
                    .content(new ObjectMapper().writeValueAsString(body))
            )
            .andExpect(status().is(200));
    }
}