package org.openapitools.codegen.online.api;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.online.model.ResponseCode;
import org.springframework.core.io.ByteArrayResource;
import org.springframework.http.ResponseEntity;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.test.context.bean.override.mockito.MockitoBean;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.servlet.MockMvc;
import static org.junit.jupiter.api.Assertions.assertTrue;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.not;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@ExtendWith(SpringExtension.class)
@WebMvcTest(GenApiController.class)
public class GenApiControllerTest {

    private static final String OPENAPI_URL = "https://raw.githubusercontent.com/OpenAPITools/openapi-generator/v4.3.1/modules/openapi-generator/src/test/resources/petstore.json";
    private static final String UUID_REGEX = "[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-4[a-fA-F0-9]{3}-[89aAbB][a-fA-F0-9]{3}-[a-fA-F0-9]{12}";

    @Autowired
    private MockMvc mockMvc;

    @MockitoBean
    private GenApiDelegate genApiDelegate;

    @Test
    public void clientLanguages() throws Exception {
        when(genApiDelegate.clientOptions()).thenReturn(ResponseEntity.ok(Arrays.asList("java", "python", "javascript")));
        getLanguages("clients", "java");
    }

    @Test
    public void serverFrameworks() throws Exception {
        when(genApiDelegate.serverOptions()).thenReturn(ResponseEntity.ok(Arrays.asList("spring", "nodejs", "flask")));
        getLanguages("servers", "spring");
    }


    public void getLanguages(String type, String expected) throws Exception {
        mockMvc.perform(get("/api/gen/" + type))
                .andExpect(status().isOk())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                .andExpect(jsonPath("$.[*]").value(hasItem(expected)));
    }

    @Test
    public void clientOptions() throws Exception {
        Map<String, CliOption> options = new HashMap<>();
        CliOption option = new CliOption("sortParamsByRequiredFlag", "Sort parameters by required flag");
        options.put("sortParamsByRequiredFlag", option);
        when(genApiDelegate.getClientOptions("java")).thenReturn(ResponseEntity.ok(options));
        getOptions("clients", "java");
    }

    @Test
    public void clientOptionsUnknown() throws Exception {
        when(genApiDelegate.getClientOptions("unknown")).thenReturn(ResponseEntity.notFound().build());
        mockMvc.perform(get("/api/gen/clients/unknown"))
                .andExpect(status().isNotFound());
    }

    @Test
    public void serverOptions() throws Exception {
        Map<String, CliOption> options = new HashMap<>();
        CliOption option = new CliOption("sortParamsByRequiredFlag", "Sort parameters by required flag");
        options.put("sortParamsByRequiredFlag", option);
        when(genApiDelegate.getServerOptions("spring")).thenReturn(ResponseEntity.ok(options));
        getOptions("servers", "spring");
    }

    @Test
    public void serverOptionsUnknown() throws Exception {
        when(genApiDelegate.getServerOptions("unknown")).thenReturn(ResponseEntity.notFound().build());
        mockMvc.perform(get("/api/gen/servers/unknown"))
                .andExpect(status().isNotFound());
    }

    private void getOptions(String type, String name) throws Exception {
        mockMvc.perform(get("/api/gen/" + type + "/" + name))
                .andExpect(status().isOk())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                .andExpect(jsonPath("$.sortParamsByRequiredFlag.opt").value("sortParamsByRequiredFlag"));
    }

    @Test
    public void generateClient() throws Exception {
        when(genApiDelegate.generateClient(anyString(), any())).thenReturn(ResponseEntity.ok(new ResponseCode("test-code", "http://test.com:1234/api/gen/download/test-code")));
        when(genApiDelegate.downloadFile("test-code")).thenReturn(ResponseEntity.ok().contentType(org.springframework.http.MediaType.valueOf("application/zip")).header("Content-Length", "1024").body(new ByteArrayResource(new byte[1024])));
        generateAndDownload("clients", "java");
    }

    @Test
    public void generateServer() throws Exception {
        when(genApiDelegate.generateServerForLanguage(anyString(), any())).thenReturn(ResponseEntity.ok(new ResponseCode("test-code", "http://test.com:1234/api/gen/download/test-code")));
        when(genApiDelegate.downloadFile("test-code")).thenReturn(ResponseEntity.ok().contentType(org.springframework.http.MediaType.valueOf("application/zip")).header("Content-Length", "1024").body(new ByteArrayResource(new byte[1024])));
        generateAndDownload("servers", "spring");
    }

    private void generateAndDownload(String type, String name) throws Exception {
        String result = mockMvc.perform(post("http://test.com:1234/api/gen/" + type + "/" + name)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("{\"openAPIUrl\": \"" + OPENAPI_URL + "\"}"))
                .andExpect(status().isOk())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                .andExpect(jsonPath("$.code").value("test-code"))
                .andExpect(jsonPath("$.link").value("http://test.com:1234/api/gen/download/test-code"))
                .andReturn().getResponse().getContentAsString();

        String code = new ObjectMapper().readValue(result, ResponseCode.class).getCode();

        mockMvc.perform(get("http://test.com:1234/api/gen/download/" + code))
                .andExpect(content().contentType("application/zip"))
                .andExpect(status().isOk())
                .andExpect(header().string(HttpHeaders.CONTENT_LENGTH, not(0)));
    }

    @Test
    public void generateWIthForwardedHeaders() throws Exception {
        when(genApiDelegate.generateClient(anyString(), any())).thenReturn(ResponseEntity.ok(new ResponseCode("test-code", "https://forwarded.com:5678/api/gen/download/test-code")));
        when(genApiDelegate.downloadFile("test-code")).thenReturn(ResponseEntity.ok().contentType(org.springframework.http.MediaType.valueOf("application/zip")).header("Content-Length", "1024").body(new ByteArrayResource(new byte[1024])));
        
        String result = mockMvc.perform(post("http://test.com:1234/api/gen/clients/java")
                        .contentType(MediaType.APPLICATION_JSON)
                        .header("X-Forwarded-Proto", "https")
                        .header("X-Forwarded-Host", "forwarded.com")
                        .header("X-Forwarded-Port", "5678")
                        .content("{\"openAPIUrl\": \"" + OPENAPI_URL + "\"}"))
                .andExpect(status().isOk())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                .andExpect(jsonPath("$.code").value("test-code"))
                .andExpect(jsonPath("$.link").value("https://forwarded.com:5678/api/gen/download/test-code"))
                .andReturn().getResponse().getContentAsString();

        String code = new ObjectMapper().readValue(result, ResponseCode.class).getCode();

        mockMvc.perform(get("http://test.com:1234/api/gen/download/" + code))
                .andExpect(content().contentType("application/zip"))
                .andExpect(status().isOk())
                .andExpect(header().string(HttpHeaders.CONTENT_LENGTH, not(0)));
    }

    @Test
    public void generateClientWithInvalidOpenAPIUrl() throws Exception {
        when(genApiDelegate.generateClient(anyString(), any())).thenReturn(ResponseEntity.badRequest().build());
        final String invalidOpenAPIUrl = "https://[::1]/invalid_openapi.json";
        mockMvc.perform(post("http://test.com:1234/api/gen/clients/java")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("{\"openAPIUrl\": \"" + invalidOpenAPIUrl + "\"}"))
                .andExpect(status().isBadRequest());
    }

    @Test
    public void generateWithOpenAPINormalizer() throws Exception {
        when(genApiDelegate.generateClient(anyString(), any()))
                .thenReturn(ResponseEntity.ok(new ResponseCode("test-code-1", "http://test.com:1234/api/gen/download/test-code-1")))
                .thenReturn(ResponseEntity.ok(new ResponseCode("test-code-2", "http://test.com:1234/api/gen/download/test-code-2")));
        when(genApiDelegate.downloadFile("test-code-1")).thenReturn(ResponseEntity.ok().contentType(org.springframework.http.MediaType.valueOf("application/zip")).header("Content-Length", "512").body(new ByteArrayResource(new byte[512])));
        when(genApiDelegate.downloadFile("test-code-2")).thenReturn(ResponseEntity.ok().contentType(org.springframework.http.MediaType.valueOf("application/zip")).header("Content-Length", "1024").body(new ByteArrayResource(new byte[1024])));
        
        String withOpenAPINormalizer = "{\"openAPIUrl\":\"https://raw.githubusercontent.com/OpenAPITools/openapi-generator/master/modules/openapi-generator/src/test/resources/2_0/petstore.yaml\",\"openapiNormalizer\":[\"FILTER=operationId:updatePet\"],\"options\":{},\"spec\":{}}";
        String withoutOpenAPINormalizer = "{\"openAPIUrl\":\"https://raw.githubusercontent.com/OpenAPITools/openapi-generator/master/modules/openapi-generator/src/test/resources/2_0/petstore.yaml\",\"options\":{},\"spec\":{}}";

        String responseOfNormalized = mockMvc.perform(post("http://test.com:1234/api/gen/clients/java")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(withOpenAPINormalizer))
                .andExpect(status().isOk()).andReturn().getResponse().getContentAsString();
        String codeOfNormalized = new ObjectMapper().readValue(responseOfNormalized, ResponseCode.class).getCode();
        Long lengthOfNormalized = Long.parseLong(mockMvc.perform(get("http://test.com:1234/api/gen/download/" + codeOfNormalized))
                .andExpect(content().contentType("application/zip"))
                .andExpect(status().isOk()).andReturn().getResponse().getHeader("Content-Length"));

        String responseOfNotNormalized = mockMvc.perform(post("http://test.com:1234/api/gen/clients/java")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(withoutOpenAPINormalizer))
                .andExpect(status().isOk()).andReturn().getResponse().getContentAsString();

        String codeOfNotNormalized = new ObjectMapper().readValue(responseOfNotNormalized, ResponseCode.class).getCode();
        Long lengthOfNotNormalized = Long.parseLong(mockMvc.perform(get("http://test.com:1234/api/gen/download/" + codeOfNotNormalized))
                .andExpect(content().contentType("application/zip"))
                .andExpect(status().isOk()).andReturn().getResponse().getHeader("Content-Length"));

        assertTrue(lengthOfNormalized <= lengthOfNotNormalized, "Using the normalizer should result in a smaller or equal file size");

    }
}
