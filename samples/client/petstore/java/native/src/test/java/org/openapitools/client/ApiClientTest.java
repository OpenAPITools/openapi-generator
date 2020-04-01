package org.openapitools.client;

import org.junit.Test;

import java.net.http.HttpClient;
import java.time.Duration;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class ApiClientTest {

    @Test
    public void urlEncodeTest() {
        String encoded = ApiClient.urlEncode("/&= $");
        assertEquals("%2F%26%3D+%24", encoded);
    }

    @Test
    public void parameterToPairsTest() {
        List<Pair> l = ApiClient.parameterToPairs("$name", "some value");
        assertEquals(1, l.size());
        Pair p = l.get(0);
        assertEquals("%24name", p.getName());
        assertEquals("some+value", p.getValue());

        l = ApiClient.parameterToPairs("$name", null);
        assertTrue(l.isEmpty());
    }

    @Test
    public void parameterToPairsCollectionTest() {
        List<Pair> l = ApiClient.parameterToPairs(
                "csv",
                "$name",
                List.of("value 1", "value 2"));
        assertEquals(1, l.size());
        Pair p = l.get(0);
        assertEquals("%24name", p.getName());
        assertEquals("value+1%2Cvalue+2", p.getValue());

        l = ApiClient.parameterToPairs(
                "multi",
                "$name",
                List.of("value 1", "value 2"));
        assertEquals(2, l.size());
        p = l.get(0);
        assertEquals("%24name", p.getName());
        assertEquals("value+1", p.getValue());
        p = l.get(1);
        assertEquals("%24name", p.getName());
        assertEquals("value+2", p.getValue());

        l = ApiClient.parameterToPairs(
                "multi",
                "$name",
                List.of());
        assertTrue(l.isEmpty());
    }

    @Test
    public void uriOverrideTest() {
        ApiClient apiClient = new ApiClient();
        apiClient.setHost("test.swagger.io");
        apiClient.setPort(9999);
        apiClient.setBasePath("/testing");
        String baseUri = apiClient.getBaseUri();
        assertTrue(baseUri.endsWith("://test.swagger.io:9999/testing"));
    }

    @Test
    public void clientConnectTimeoutTest() {
        ApiClient apiClient = new ApiClient();
        HttpClient client = apiClient.getHttpClient();
        assertTrue(client.connectTimeout().isEmpty());

        HttpClient.Builder builder = HttpClient.newBuilder()
                .connectTimeout(Duration.ofSeconds(10));
        apiClient.setHttpClientBuilder(builder);
        client = apiClient.getHttpClient();
        assertTrue(client.connectTimeout().isPresent());
        assertEquals(Duration.ofSeconds(10), client.connectTimeout().get());
    }
}
