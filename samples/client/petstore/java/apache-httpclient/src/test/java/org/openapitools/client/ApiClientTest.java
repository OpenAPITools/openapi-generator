package org.openapitools.client;

import java.io.IOException;
import java.net.ServerSocket;
import java.time.Duration;

import org.junit.jupiter.api.*;
import org.openapitools.client.api.PetApi;

import static org.junit.jupiter.api.Assertions.*;

public class ApiClientTest {
    ApiClient apiClient = null;

    @BeforeEach
    public void setup() {
        apiClient = new ApiClient();
    }

    @Test
    public void testConnectTimeoutRoundTrip() {
        assertEquals(0, apiClient.getConnectTimeout());
        apiClient.setConnectTimeout(15000);
        assertEquals(15000, apiClient.getConnectTimeout());
    }

    @Test
    public void testReadTimeoutRoundTrip() {
        assertEquals(0, apiClient.getReadTimeout());
        apiClient.setReadTimeout(15000);
        assertEquals(15000, apiClient.getReadTimeout());
    }

    @Test
    public void testConnectTimeoutIsApplied() {
        // 192.0.2.1 (RFC 5737 TEST-NET-1) is non-routable: connecting blocks until the
        // connect timeout fires. If the configured timeout is not applied to the request,
        // Apache HttpClient's own default (3 minutes) is used instead and the assertion
        // aborts the test after 30 seconds.
        apiClient.setBasePath("http://192.0.2.1:81");
        apiClient.setConnectTimeout(500);
        PetApi petApi = new PetApi(apiClient);
        assertTimeoutPreemptively(Duration.ofSeconds(30), () ->
            assertThrows(ApiException.class, () -> petApi.getPetById(1L)));
    }

    @Test
    public void testReadTimeoutIsApplied() throws IOException {
        // A server socket that is never accepted from still completes the TCP handshake
        // (via the OS backlog), so the request is sent and then blocks waiting for the
        // response. If the configured timeout is not applied to the request, Apache
        // HttpClient waits forever by default and the assertion aborts the test after
        // 30 seconds.
        try (ServerSocket serverSocket = new ServerSocket(0)) {
            apiClient.setBasePath("http://localhost:" + serverSocket.getLocalPort());
            apiClient.setReadTimeout(500);
            PetApi petApi = new PetApi(apiClient);
            assertTimeoutPreemptively(Duration.ofSeconds(30), () ->
                assertThrows(ApiException.class, () -> petApi.getPetById(1L)));
        }
    }
}
