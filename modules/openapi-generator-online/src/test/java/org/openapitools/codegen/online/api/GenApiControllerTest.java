package org.openapitools.codegen.online.api;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.openapitools.codegen.online.model.Generated;
import org.openapitools.codegen.online.model.ResponseCode;
import org.openapitools.codegen.online.service.GenApiService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.util.Assert;

import java.lang.reflect.Field;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import static org.hamcrest.Matchers.*;
import static org.hamcrest.text.MatchesPattern.matchesPattern;
import static org.junit.jupiter.api.Assertions.*;
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

    @Autowired
    private GenApiService genApiService;

    @Test
    public void clientLanguages() throws Exception {
        getLanguages("clients", "java");
    }

    @Test
    public void serverFrameworks() throws Exception {
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
        getOptions("clients", "java");
    }

    @Test
    public void clientOptionsUnknown() throws Exception {
        mockMvc.perform(get("/api/gen/clients/unknown"))
                .andExpect(status().isNotFound());
    }

    @Test
    public void serverOptions() throws Exception {
        getOptions("servers", "spring");
    }

    @Test
    public void serverOptionsUnknown() throws Exception {
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
        generateAndDownload("clients", "java");
    }

    @Test
    public void generateServer() throws Exception {
        generateAndDownload("servers", "spring");
    }

    private void generateAndDownload(String type, String name) throws Exception {
        String result = mockMvc.perform(post("http://test.com:1234/api/gen/" + type + "/" + name)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("{\"openAPIUrl\": \"" + OPENAPI_URL + "\"}"))
                .andExpect(status().isOk())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                .andExpect(jsonPath("$.code").value(matchesPattern(UUID_REGEX)))
                .andExpect(jsonPath("$.link").value(matchesPattern("http\\:\\/\\/test.com\\:1234\\/api\\/gen\\/download\\/" + UUID_REGEX)))
                .andReturn().getResponse().getContentAsString();

        String code = new ObjectMapper().readValue(result, ResponseCode.class).getCode();

        mockMvc.perform(get("http://test.com:1234/api/gen/download/" + code))
                .andExpect(content().contentType("application/zip"))
                .andExpect(status().isOk())
                .andExpect(header().string(HttpHeaders.CONTENT_LENGTH, not(0)));
    }

    @Test
    public void generateWIthForwardedHeaders() throws Exception {
        String result = mockMvc.perform(post("http://test.com:1234/api/gen/clients/java")
                        .contentType(MediaType.APPLICATION_JSON)
                        .header("X-Forwarded-Proto", "https")
                        .header("X-Forwarded-Host", "forwarded.com")
                        .header("X-Forwarded-Port", "5678")
                        .content("{\"openAPIUrl\": \"" + OPENAPI_URL + "\"}"))
                .andExpect(status().isOk())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                .andExpect(jsonPath("$.code").value(matchesPattern(UUID_REGEX)))
                .andExpect(jsonPath("$.link").value(matchesPattern("https\\:\\/\\/forwarded.com\\:5678\\/api\\/gen\\/download\\/" + UUID_REGEX)))
                .andReturn().getResponse().getContentAsString();

        String code = new ObjectMapper().readValue(result, ResponseCode.class).getCode();

        mockMvc.perform(get("http://test.com:1234/api/gen/download/" + code))
                .andExpect(content().contentType("application/zip"))
                .andExpect(status().isOk())
                .andExpect(header().string(HttpHeaders.CONTENT_LENGTH, not(0)));
    }

    @Test
    public void generateClientWithInvalidOpenAPIUrl() throws Exception {
        final String invalidOpenAPIUrl = "https://[::1]/invalid_openapi.json";
        mockMvc.perform(post("http://test.com:1234/api/gen/clients/java")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("{\"openAPIUrl\": \"" + invalidOpenAPIUrl + "\"}"))
                .andExpect(status().isBadRequest());
    }

    @Test
    public void generateWithOpenAPINormalizer() throws Exception {
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

        Assert.isTrue(lengthOfNormalized <= lengthOfNotNormalized, "Using the normalizer should result in a smaller or equal file size");

    }

    // Fix #3: Content-Length header is present and non-zero on download response
    @Test
    public void downloadHasContentLengthHeader() throws Exception {
        String result = mockMvc.perform(post("http://test.com:1234/api/gen/clients/java")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("{\"openAPIUrl\": \"" + OPENAPI_URL + "\"}"))
                .andExpect(status().isOk())
                .andReturn().getResponse().getContentAsString();

        String code = new ObjectMapper().readValue(result, ResponseCode.class).getCode();

        String contentLength = mockMvc.perform(get("http://test.com:1234/api/gen/download/" + code))
                .andExpect(status().isOk())
                .andExpect(header().exists(HttpHeaders.CONTENT_LENGTH))
                .andReturn().getResponse().getHeader(HttpHeaders.CONTENT_LENGTH);

        assertTrue(Long.parseLong(contentLength) > 0, "Content-Length should be greater than 0");
    }

    private static void setCreatedAt(Generated g, Instant value) throws Exception {
        Field f = Generated.class.getDeclaredField("createdAt");
        f.setAccessible(true);
        f.set(g, value);
    }

    // Fix: expired entry still in map returns 404, not the file
    @Test
    public void downloadExpiredEntryInMapReturns404() throws Exception {
        Field field = GenApiService.class.getDeclaredField("fileMap");
        field.setAccessible(true);
        @SuppressWarnings("unchecked")
        Map<String, Generated> fileMap = (Map<String, Generated>) field.get(null);

        Generated expired = new Generated();
        expired.setFilename("/tmp/some-file.zip");
        expired.setFriendlyName("test");
        setCreatedAt(expired, Instant.now().minusSeconds(25 * 3600)); // 25h ago, beyond 24h TTL
        fileMap.put("ttl-check-key", expired);

        mockMvc.perform(get("http://test.com:1234/api/gen/download/ttl-check-key"))
                .andExpect(status().isNotFound());

        fileMap.remove("ttl-check-key");
    }

    // Fix #1: downloading an expired/evicted fileId returns 404, not NPE
    @Test
    public void downloadExpiredFileReturns404() throws Exception {
        mockMvc.perform(get("http://test.com:1234/api/gen/download/nonexistent-or-expired-id"))
                .andExpect(status().isNotFound());
    }

    // Fix #1: fileMap uses ConcurrentHashMap (thread-safe)
    @Test
    public void fileMapIsConcurrentHashMap() throws Exception {
        Field field = GenApiService.class.getDeclaredField("fileMap");
        field.setAccessible(true);
        Object map = field.get(null);
        assertInstanceOf(ConcurrentHashMap.class, map, "fileMap should be a ConcurrentHashMap");
    }

    // Fix #1: TTL cleanup removes expired entries
    @Test
    public void cleanExpiredFilesRemovesOldEntries() throws Exception {
        Field field = GenApiService.class.getDeclaredField("fileMap");
        field.setAccessible(true);
        @SuppressWarnings("unchecked")
        Map<String, Generated> fileMap = (Map<String, Generated>) field.get(null);

        // Insert an entry with a creation time well in the past
        Generated expired = new Generated();
        expired.setFilename("/tmp/nonexistent-expired.zip");
        expired.setFriendlyName("test");
        setCreatedAt(expired, Instant.now().minusSeconds(25 * 3600)); // 25 hours ago, beyond 24h TTL
        fileMap.put("expired-test-key", expired);

        genApiService.cleanExpiredFiles();

        assertFalse(fileMap.containsKey("expired-test-key"), "Expired entry should have been removed by cleanExpiredFiles");
    }

    // Fix #1: concurrent generation does not lose entries (thread-safety smoke test)
    @Test
    public void concurrentGenerationDoesNotLoseEntries() throws Exception {
        int threads = 5;
        CountDownLatch latch = new CountDownLatch(threads);
        ExecutorService executor = Executors.newFixedThreadPool(threads);
        List<String> codes = new ArrayList<>();

        for (int i = 0; i < threads; i++) {
            executor.submit(() -> {
                try {
                    String result = mockMvc.perform(post("http://test.com:1234/api/gen/clients/java")
                                    .contentType(MediaType.APPLICATION_JSON)
                                    .content("{\"openAPIUrl\": \"" + OPENAPI_URL + "\"}"))
                            .andExpect(status().isOk())
                            .andReturn().getResponse().getContentAsString();
                    synchronized (codes) {
                        codes.add(new ObjectMapper().readValue(result, ResponseCode.class).getCode());
                    }
                } catch (Exception e) {
                    fail("Concurrent generation failed: " + e.getMessage());
                } finally {
                    latch.countDown();
                }
            });
        }

        latch.await();
        executor.shutdown();
        assertEquals(threads, codes.size(), "All concurrent generations should produce distinct download codes");
        assertEquals(threads, codes.stream().distinct().count(), "All codes should be unique");
    }
}

    private static final String OPENAPI_URL = "https://raw.githubusercontent.com/OpenAPITools/openapi-generator/v4.3.1/modules/openapi-generator/src/test/resources/petstore.json";
    private static final String UUID_REGEX = "[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-4[a-fA-F0-9]{3}-[89aAbB][a-fA-F0-9]{3}-[a-fA-F0-9]{12}";

    @Autowired
    private MockMvc mockMvc;

    @Test
    public void clientLanguages() throws Exception {
        getLanguages("clients", "java");
    }

    @Test
    public void serverFrameworks() throws Exception {
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
        getOptions("clients", "java");
    }

    @Test
    public void clientOptionsUnknown() throws Exception {
        mockMvc.perform(get("/api/gen/clients/unknown"))
                .andExpect(status().isNotFound());
    }

    @Test
    public void serverOptions() throws Exception {
        getOptions("servers", "spring");
    }

    @Test
    public void serverOptionsUnknown() throws Exception {
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
        generateAndDownload("clients", "java");
    }

    @Test
    public void generateServer() throws Exception {
        generateAndDownload("servers", "spring");
    }

    private void generateAndDownload(String type, String name) throws Exception {
        String result = mockMvc.perform(post("http://test.com:1234/api/gen/" + type + "/" + name)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("{\"openAPIUrl\": \"" + OPENAPI_URL + "\"}"))
                .andExpect(status().isOk())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                .andExpect(jsonPath("$.code").value(matchesPattern(UUID_REGEX)))
                .andExpect(jsonPath("$.link").value(matchesPattern("http\\:\\/\\/test.com\\:1234\\/api\\/gen\\/download\\/" + UUID_REGEX)))
                .andReturn().getResponse().getContentAsString();

        String code = new ObjectMapper().readValue(result, ResponseCode.class).getCode();

        mockMvc.perform(get("http://test.com:1234/api/gen/download/" + code))
                .andExpect(content().contentType("application/zip"))
                .andExpect(status().isOk())
                .andExpect(header().string(HttpHeaders.CONTENT_LENGTH, not(0)));
    }

    @Test
    public void generateWIthForwardedHeaders() throws Exception {
        String result = mockMvc.perform(post("http://test.com:1234/api/gen/clients/java")
                        .contentType(MediaType.APPLICATION_JSON)
                        .header("X-Forwarded-Proto", "https")
                        .header("X-Forwarded-Host", "forwarded.com")
                        .header("X-Forwarded-Port", "5678")
                        .content("{\"openAPIUrl\": \"" + OPENAPI_URL + "\"}"))
                .andExpect(status().isOk())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                .andExpect(jsonPath("$.code").value(matchesPattern(UUID_REGEX)))
                .andExpect(jsonPath("$.link").value(matchesPattern("https\\:\\/\\/forwarded.com\\:5678\\/api\\/gen\\/download\\/" + UUID_REGEX)))
                .andReturn().getResponse().getContentAsString();

        String code = new ObjectMapper().readValue(result, ResponseCode.class).getCode();

        mockMvc.perform(get("http://test.com:1234/api/gen/download/" + code))
                .andExpect(content().contentType("application/zip"))
                .andExpect(status().isOk())
                .andExpect(header().string(HttpHeaders.CONTENT_LENGTH, not(0)));
    }

    @Test
    public void generateClientWithInvalidOpenAPIUrl() throws Exception {
        final String invalidOpenAPIUrl = "https://[::1]/invalid_openapi.json";
        mockMvc.perform(post("http://test.com:1234/api/gen/clients/java")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("{\"openAPIUrl\": \"" + invalidOpenAPIUrl + "\"}"))
                .andExpect(status().isBadRequest());
    }

    @Test
    public void generateWithOpenAPINormalizer() throws Exception {
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

        Assert.isTrue(lengthOfNormalized <= lengthOfNotNormalized, "Using the normalizer should result in a smaller or equal file size");

    }
}
