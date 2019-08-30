package org.openapitools.virtualan.api;
import org.openapitools.virtualan.model.ModelApiResponse;
import org.openapitools.virtualan.model.Pet;
import org.springframework.core.io.Resource;
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
class PetApiTest {

    @Autowired
    private MockMvc mockmvc;

    @Test
    void findPetsByStatus200Test() throws Exception {
        List<String> status = Arrays.asList("available");
        mockmvc.perform(
                get("/pet/findByStatus", status)
            )
            .andExpect(status().is(200));
    }

    @Test
    void findPetsByStatus200Test() throws Exception {
        List<String> status = Arrays.asList("available");
        mockmvc.perform(
                get("/pet/findByStatus", status)
            )
            .andExpect(status().is(200));
    }

    @Test
    void findPetsByStatus400Test() throws Exception {
        List<String> status = Arrays.asList("available");
        mockmvc.perform(
                get("/pet/findByStatus", status)
            )
            .andExpect(status().is(400));
    }

    @Test
    void findPetsByStatus400Test() throws Exception {
        List<String> status = Arrays.asList("available");
        mockmvc.perform(
                get("/pet/findByStatus", status)
            )
            .andExpect(status().is(400));
    }

    @Test
    void findPetsByTags200Test() throws Exception {
        List<String> tags = Arrays.asList();
        mockmvc.perform(
                get("/pet/findByTags", tags)
            )
            .andExpect(status().is(200));
    }

    @Test
    void findPetsByTags200Test() throws Exception {
        List<String> tags = Arrays.asList();
        mockmvc.perform(
                get("/pet/findByTags", tags)
            )
            .andExpect(status().is(200));
    }

    @Test
    void findPetsByTags400Test() throws Exception {
        List<String> tags = Arrays.asList();
        mockmvc.perform(
                get("/pet/findByTags", tags)
            )
            .andExpect(status().is(400));
    }

    @Test
    void findPetsByTags400Test() throws Exception {
        List<String> tags = Arrays.asList();
        mockmvc.perform(
                get("/pet/findByTags", tags)
            )
            .andExpect(status().is(400));
    }

    @Test
    void getPetById200Test() throws Exception {
        Long petId = 56L;
        mockmvc.perform(
                get("/pet/{petId}", petId)
            )
            .andExpect(status().is(200));
    }

    @Test
    void getPetById200Test() throws Exception {
        Long petId = 56L;
        mockmvc.perform(
                get("/pet/{petId}", petId)
            )
            .andExpect(status().is(200));
    }

    @Test
    void getPetById400Test() throws Exception {
        Long petId = 56L;
        mockmvc.perform(
                get("/pet/{petId}", petId)
            )
            .andExpect(status().is(400));
    }

    @Test
    void getPetById400Test() throws Exception {
        Long petId = 56L;
        mockmvc.perform(
                get("/pet/{petId}", petId)
            )
            .andExpect(status().is(400));
    }

    @Test
    void getPetById404Test() throws Exception {
        Long petId = 56L;
        mockmvc.perform(
                get("/pet/{petId}", petId)
            )
            .andExpect(status().is(404));
    }

    @Test
    void getPetById404Test() throws Exception {
        Long petId = 56L;
        mockmvc.perform(
                get("/pet/{petId}", petId)
            )
            .andExpect(status().is(404));
    }

    @Test
    void uploadFile200Test() throws Exception {
        Long petId = 56L;
        String additionalMetadata = "additionalMetadata_example";
        org.springframework.web.multipart.MultipartFile file = null;
        mockmvc.perform(
                post("/pet/{petId}/uploadImage", petId, additionalMetadata, file)
            )
            .andExpect(status().is(200));
    }
}