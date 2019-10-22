package org.openapitools.api;
import java.math.BigDecimal;
import org.openapitools.model.Client;
import org.openapitools.model.FileSchemaTestClass;
import org.threeten.bp.LocalDate;
import java.util.Map;
import org.openapitools.model.ModelApiResponse;
import org.threeten.bp.OffsetDateTime;
import org.openapitools.model.OuterComposite;
import org.springframework.core.io.Resource;
import org.openapitools.model.User;
import org.openapitools.model.XmlItem;
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
class FakeApiTest {

    @Autowired
    private MockMvc mockmvc;

    @Test
    void fakeOuterBooleanSerialize200Test() throws Exception {
        Boolean body = true;
        mockmvc.perform(
                post("/fake/outer/boolean")
                    .contentType(MediaType.valueOf("*/*"))
                    .content(new ObjectMapper().writeValueAsString(body))
            )
            .andExpect(status().is(200));
    }

    @Test
    void fakeOuterCompositeSerialize200Test() throws Exception {
        OuterComposite body = new OuterComposite();
        mockmvc.perform(
                post("/fake/outer/composite")
                    .contentType(MediaType.valueOf("*/*"))
                    .content(new ObjectMapper().writeValueAsString(body))
            )
            .andExpect(status().is(200));
    }

    @Test
    void fakeOuterNumberSerialize200Test() throws Exception {
        BigDecimal body = new BigDecimal();
        mockmvc.perform(
                post("/fake/outer/number")
                    .contentType(MediaType.valueOf("*/*"))
                    .content(new ObjectMapper().writeValueAsString(body))
            )
            .andExpect(status().is(200));
    }

    @Test
    void fakeOuterStringSerialize200Test() throws Exception {
        String body = "body_example";
        mockmvc.perform(
                post("/fake/outer/string")
                    .contentType(MediaType.valueOf("*/*"))
                    .content(new ObjectMapper().writeValueAsString(body))
            )
            .andExpect(status().is(200));
    }

    @Test
    void testClientModel200Test() throws Exception {
        Client body = new Client();
        mockmvc.perform(
                patch("/fake")
                    .contentType(MediaType.valueOf("application/json"))
                    .content(new ObjectMapper().writeValueAsString(body))
            )
            .andExpect(status().is(200));
    }

    @Test
    void uploadFileWithRequiredFile200Test() throws Exception {
        Long petId = 56L;
        org.springframework.web.multipart.MultipartFile requiredFile = null;
        String additionalMetadata = "additionalMetadata_example";
        mockmvc.perform(
                post("/fake/{petId}/uploadImageWithRequiredFile", petId, requiredFile, additionalMetadata)
            )
            .andExpect(status().is(200));
    }
}