package org.openapitools.codegen.online.service;

import org.junit.jupiter.api.Test;
import org.openapitools.codegen.online.model.Generated;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.time.Instant;

import static org.junit.jupiter.api.Assertions.*;

@SpringBootTest
public class GenApiServiceTest {

    @Autowired
    private GenApiService genApiService;

    // Fix #1: TTL cleanup removes expired entries
    @Test
    public void cleanExpiredFilesRemovesExpiredEntry() {
        Generated entry = new Generated();
        entry.setFilename("/tmp/codegen-test/bundle.zip");
        entry.setFriendlyName("test");
        entry.setCreatedAt(Instant.now().minusSeconds(25 * 3600)); // 25h ago, beyond 24h TTL
        genApiService.putFileEntry("test-expired-key", entry);

        genApiService.cleanExpiredFiles();

        assertNull(genApiService.getFileEntry("test-expired-key"), "Expired entry should have been evicted");
    }

    // Fix #1: recent entry is not evicted by cleanup
    @Test
    public void cleanExpiredFilesKeepsRecentEntry() {
        Generated entry = new Generated();
        entry.setFilename("/tmp/codegen-test/bundle.zip");
        entry.setFriendlyName("test");
        // createdAt defaults to Instant.now() — well within 24h TTL
        genApiService.putFileEntry("test-recent-key", entry);

        genApiService.cleanExpiredFiles();

        assertNotNull(genApiService.getFileEntry("test-recent-key"), "Recent entry should not be evicted");
        genApiService.removeFileEntry("test-recent-key");
    }
}
