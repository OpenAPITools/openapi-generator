package org.openapitools.codegen.online.configuration;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.web.servlet.MockMvc;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@SpringBootTest
@AutoConfigureMockMvc
public class OpenAPIDocumentationConfigTest {

    @Autowired
    private MockMvc mockMvc;

    @Test
    public void apiDocsDescribeTheGeneratorApi() throws Exception {
        // MockMvc requests come from http://localhost, unless GENERATOR_HOST sets the address
        String configured = OpenAPIDocumentationConfig.serverUrl(System.getenv("GENERATOR_HOST"));
        String body = mockMvc.perform(get("/api-docs"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.openapi").value("3.0.1"))
                .andExpect(jsonPath("$.info.title").value("OpenAPI Generator Online"))
                .andExpect(jsonPath("$.paths['/api/gen/clients'].get.operationId")
                        .value("clientOptions"))
                .andExpect(jsonPath("$.paths['/api/gen/clients/{language}'].get.operationId")
                        .value("getClientOptions"))
                .andExpect(jsonPath("$.paths['/api/gen/clients/{language}'].post.operationId")
                        .value("generateClient"))
                .andExpect(jsonPath("$.paths['/api/gen/servers'].get.operationId")
                        .value("serverOptions"))
                .andExpect(jsonPath("$.paths['/api/gen/servers/{framework}'].get.operationId")
                        .value("getServerOptions"))
                .andExpect(jsonPath("$.paths['/api/gen/servers/{framework}'].post.operationId")
                        .value("generateServerForLanguage"))
                .andExpect(jsonPath("$.paths['/api/gen/download/{fileId}'].get.operationId")
                        .value("downloadFile"))
                .andExpect(jsonPath("$.components.schemas.GeneratorInput").exists())
                .andExpect(jsonPath("$.servers[0].url")
                        .value(configured != null ? configured : "//localhost"))
                .andReturn().getResponse().getContentAsString();

        List<String> paths = new ArrayList<>();
        new ObjectMapper().readTree(body).get("paths").fieldNames().forEachRemaining(paths::add);
        assertEquals(paths.stream().sorted().collect(Collectors.toList()), paths);
    }

    @Test
    public void serverUrlDropsTrailingSlashes() {
        assertNull(OpenAPIDocumentationConfig.serverUrl(null));
        assertNull(OpenAPIDocumentationConfig.serverUrl(""));
        assertEquals("http://example.com:8888", OpenAPIDocumentationConfig.serverUrl("http://example.com:8888"));
        assertEquals("http://example.com:8888", OpenAPIDocumentationConfig.serverUrl("http://example.com:8888/"));
        assertEquals("https://example.com/prefix",
                OpenAPIDocumentationConfig.serverUrl("https://example.com/prefix//"));
        assertNull(OpenAPIDocumentationConfig.serverUrl("http://example.com:8888/ x"));
        assertNull(OpenAPIDocumentationConfig.serverUrl("http:///x"));
        assertNull(OpenAPIDocumentationConfig.serverUrl("example.com:8888"));
        assertEquals("/prefix", OpenAPIDocumentationConfig.serverUrl("/prefix/"));
    }
}
