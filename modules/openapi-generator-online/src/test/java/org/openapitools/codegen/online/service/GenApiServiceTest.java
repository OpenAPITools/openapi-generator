package org.openapitools.codegen.online.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;
import org.openapitools.codegen.online.model.Generated;
import org.openapitools.codegen.online.model.ResponseCode;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import java.io.File;
import java.nio.file.Files;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@SpringBootTest
@AutoConfigureMockMvc
public class GenApiServiceTest {

    private static final String OPENAPI_URL = "https://raw.githubusercontent.com/OpenAPITools/openapi-generator/v4.3.1/modules/openapi-generator/src/test/resources/petstore.json";

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private GenApiService genApiService;

    // Fix #1: TTL cleanup removes expired entries and deletes the temp directory
    @Test
    public void cleanExpiredFilesRemovesExpiredEntry() throws Exception {
        File tempDir = Files.createTempDirectory("codegen-test").toFile();
        File bundle = new File(tempDir, "bundle.zip");
        bundle.createNewFile();

        Generated entry = new Generated();
        entry.setFilename(bundle.getAbsolutePath());
        entry.setFriendlyName("test");
        entry.setCreatedAt(Instant.now().minusSeconds(25 * 3600));
        genApiService.putFileEntry("test-expired-key", entry);

        genApiService.cleanExpiredFiles();

        assertNull(genApiService.getFileEntry("test-expired-key"), "Expired entry should have been evicted");
        assertFalse(tempDir.exists(), "Temp directory should have been deleted");
    }

    // Fix: entry is retained if directory deletion fails (no orphaned files)
    @Test
    public void cleanExpiredFilesRetainsEntryWhenDeletionFails() throws Exception {
        // Point filename at a path whose "parent" is a regular file — deleteDirectory will fail
        File fakeParent = Files.createTempFile("codegen-not-a-dir", ".tmp").toFile();

        Generated entry = new Generated();
        entry.setFilename(new File(fakeParent, "bundle.zip").getAbsolutePath());
        entry.setFriendlyName("test");
        entry.setCreatedAt(Instant.now().minusSeconds(25 * 3600));
        genApiService.putFileEntry("test-deletion-fail-key", entry);

        genApiService.cleanExpiredFiles();

        assertNotNull(genApiService.getFileEntry("test-deletion-fail-key"), "Entry should be retained when deletion fails");

        // cleanup
        fakeParent.delete();
        genApiService.removeFileEntry("test-deletion-fail-key");
    }

    // Fix #1: recent entry is not evicted by cleanup
    @Test
    public void cleanExpiredFilesKeepsRecentEntry() throws Exception {
        File tempDir = Files.createTempDirectory("codegen-test").toFile();

        Generated entry = new Generated();
        entry.setFilename(new File(tempDir, "bundle.zip").getAbsolutePath());
        entry.setFriendlyName("test");
        genApiService.putFileEntry("test-recent-key", entry);

        genApiService.cleanExpiredFiles();

        assertNotNull(genApiService.getFileEntry("test-recent-key"), "Recent entry should not be evicted");
        genApiService.removeFileEntry("test-recent-key");
        tempDir.delete();
    }

    // Fix #1: missing fileId returns 404
    @Test
    public void downloadMissingFileReturns404() throws Exception {
        mockMvc.perform(get("/api/gen/download/nonexistent-id"))
                .andExpect(status().isNotFound());
    }

    // Fix #1: existing entry past TTL returns 404 (TTL enforced at request time)
    @Test
    public void downloadExpiredEntryReturns404() throws Exception {
        String result = mockMvc.perform(post("/api/gen/clients/java")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("{\"openAPIUrl\": \"" + OPENAPI_URL + "\"}"))
                .andExpect(status().isOk())
                .andReturn().getResponse().getContentAsString();
        String code = new ObjectMapper().readValue(result, ResponseCode.class).getCode();

        genApiService.getFileEntry(code).setCreatedAt(Instant.now().minusSeconds(25 * 3600));

        mockMvc.perform(get("/api/gen/download/" + code))
                .andExpect(status().isNotFound());
    }

    // Fix #1: concurrent generation does not lose entries (thread-safety)
    @Test
    public void concurrentGenerationDoesNotLoseEntries() throws Exception {
        int threads = 5;
        CountDownLatch latch = new CountDownLatch(threads);
        ExecutorService executor = Executors.newFixedThreadPool(threads);
        List<String> codes = new ArrayList<>();

        for (int i = 0; i < threads; i++) {
            executor.submit(() -> {
                try {
                    String result = mockMvc.perform(post("/api/gen/clients/java")
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
        assertEquals(threads, codes.size(), "All concurrent generations should succeed");
        assertEquals(threads, codes.stream().distinct().count(), "All codes should be unique");
    }
}
