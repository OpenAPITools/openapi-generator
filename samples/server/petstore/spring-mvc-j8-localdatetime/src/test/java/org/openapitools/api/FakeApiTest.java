package org.openapitools.api;
import java.math.BigDecimal;
import org.openapitools.model.Client;
import org.openapitools.model.FileSchemaTestClass;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Map;
import org.openapitools.model.ModelApiResponse;
import org.openapitools.model.OuterComposite;
import org.springframework.core.io.Resource;
import org.openapitools.model.User;
import org.openapitools.model.XmlItem;
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
public class FakeApiTest {

    @Autowired
    private WebApplicationContext webAppContext;
    private MockMvc mockmvc;

    @Before
    public void setup() {
        mockmvc = MockMvcBuilders.webAppContextSetup(webAppContext).build();
    }

    @Test
    public void fakeOuterBooleanSerialize200Test() throws Exception {
        Boolean body = true;
        mockmvc.perform(
                post("/fake/outer/boolean")
                    .contentType(MediaType.valueOf("*/*"))
                    .content(new ObjectMapper().writeValueAsString(body))
            )
            .andExpect(status().is(200));
    }

    @Test
    public void fakeOuterCompositeSerialize200Test() throws Exception {
        OuterComposite body = new OuterComposite();
        mockmvc.perform(
                post("/fake/outer/composite")
                    .contentType(MediaType.valueOf("*/*"))
                    .content(new ObjectMapper().writeValueAsString(body))
            )
            .andExpect(status().is(200));
    }

    @Test
    public void fakeOuterNumberSerialize200Test() throws Exception {
        BigDecimal body = new BigDecimal();
        mockmvc.perform(
                post("/fake/outer/number")
                    .contentType(MediaType.valueOf("*/*"))
                    .content(new ObjectMapper().writeValueAsString(body))
            )
            .andExpect(status().is(200));
    }

    @Test
    public void fakeOuterStringSerialize200Test() throws Exception {
        String body = "body_example";
        mockmvc.perform(
                post("/fake/outer/string")
                    .contentType(MediaType.valueOf("*/*"))
                    .content(new ObjectMapper().writeValueAsString(body))
            )
            .andExpect(status().is(200));
    }

    @Test
    public void testClientModel200Test() throws Exception {
        Client body = new Client();
        mockmvc.perform(
                patch("/fake")
                    .contentType(MediaType.valueOf("application/json"))
                    .content(new ObjectMapper().writeValueAsString(body))
            )
            .andExpect(status().is(200));
    }

    @Test
    public void uploadFileWithRequiredFile200Test() throws Exception {
        Long petId = 56L;
        org.springframework.web.multipart.MultipartFile requiredFile = null;
        String additionalMetadata = "additionalMetadata_example";
        mockmvc.perform(
                post("/fake/{petId}/uploadImageWithRequiredFile", petId, requiredFile, additionalMetadata)
            )
            .andExpect(status().is(200));
    }
}