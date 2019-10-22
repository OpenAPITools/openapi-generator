package org.openapitools.api;
import org.openapitools.model.ModelApiResponse;
import org.openapitools.model.Pet;
import org.springframework.core.io.Resource;
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
public class PetApiTest {

    @Autowired
    private WebApplicationContext webAppContext;
    private MockMvc mockmvc;

    @Before
    public void setup() {
        mockmvc = MockMvcBuilders.webAppContextSetup(webAppContext).build();
    }

    @Test
    public void findPetsByStatus200Test() throws Exception {
        List<String> status = Arrays.asList("available");
        mockmvc.perform(
                get("/pet/findByStatus", status)
            )
            .andExpect(status().is(200));
    }

    @Test
    public void findPetsByStatus200Test() throws Exception {
        List<String> status = Arrays.asList("available");
        mockmvc.perform(
                get("/pet/findByStatus", status)
            )
            .andExpect(status().is(200));
    }

    @Test
    public void findPetsByStatus400Test() throws Exception {
        List<String> status = Arrays.asList("available");
        mockmvc.perform(
                get("/pet/findByStatus", status)
            )
            .andExpect(status().is(400));
    }

    @Test
    public void findPetsByStatus400Test() throws Exception {
        List<String> status = Arrays.asList("available");
        mockmvc.perform(
                get("/pet/findByStatus", status)
            )
            .andExpect(status().is(400));
    }

    @Test
    public void findPetsByTags200Test() throws Exception {
        List<String> tags = Arrays.asList();
        mockmvc.perform(
                get("/pet/findByTags", tags)
            )
            .andExpect(status().is(200));
    }

    @Test
    public void findPetsByTags200Test() throws Exception {
        List<String> tags = Arrays.asList();
        mockmvc.perform(
                get("/pet/findByTags", tags)
            )
            .andExpect(status().is(200));
    }

    @Test
    public void findPetsByTags400Test() throws Exception {
        List<String> tags = Arrays.asList();
        mockmvc.perform(
                get("/pet/findByTags", tags)
            )
            .andExpect(status().is(400));
    }

    @Test
    public void findPetsByTags400Test() throws Exception {
        List<String> tags = Arrays.asList();
        mockmvc.perform(
                get("/pet/findByTags", tags)
            )
            .andExpect(status().is(400));
    }

    @Test
    public void getPetById200Test() throws Exception {
        Long petId = 56L;
        mockmvc.perform(
                get("/pet/{petId}", petId)
            )
            .andExpect(status().is(200));
    }

    @Test
    public void getPetById200Test() throws Exception {
        Long petId = 56L;
        mockmvc.perform(
                get("/pet/{petId}", petId)
            )
            .andExpect(status().is(200));
    }

    @Test
    public void getPetById400Test() throws Exception {
        Long petId = 56L;
        mockmvc.perform(
                get("/pet/{petId}", petId)
            )
            .andExpect(status().is(400));
    }

    @Test
    public void getPetById400Test() throws Exception {
        Long petId = 56L;
        mockmvc.perform(
                get("/pet/{petId}", petId)
            )
            .andExpect(status().is(400));
    }

    @Test
    public void getPetById404Test() throws Exception {
        Long petId = 56L;
        mockmvc.perform(
                get("/pet/{petId}", petId)
            )
            .andExpect(status().is(404));
    }

    @Test
    public void getPetById404Test() throws Exception {
        Long petId = 56L;
        mockmvc.perform(
                get("/pet/{petId}", petId)
            )
            .andExpect(status().is(404));
    }

    @Test
    public void uploadFile200Test() throws Exception {
        Long petId = 56L;
        String additionalMetadata = "additionalMetadata_example";
        org.springframework.web.multipart.MultipartFile file = null;
        mockmvc.perform(
                post("/pet/{petId}/uploadImage", petId, additionalMetadata, file)
            )
            .andExpect(status().is(200));
    }
}