package org.openapitools.server;

import java.util.Collections;
import java.util.concurrent.TimeUnit;

import jakarta.json.Json;
import jakarta.json.JsonBuilderFactory;

import io.helidon.media.jsonp.JsonpSupport;
import io.helidon.webclient.WebClient;
import io.helidon.webserver.WebServer;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

@Disabled
public class MainTest {

    private static WebServer webServer;
    private static WebClient webClient;
    private static final JsonBuilderFactory JSON_BUILDER = Json.createBuilderFactory(Collections.emptyMap());

    @BeforeAll
    public static void startTheServer() throws Exception {
        webServer = Main.startServer().await();

        webClient = WebClient.builder()
                             .baseUri("http://localhost:" + webServer.port())
                             .addMediaSupport(JsonpSupport.create())
                             .build();
    }

    @AfterAll
    public static void stopServer() throws Exception {
        if (webServer != null) {
            webServer.shutdown()
                     .toCompletableFuture()
                     .get(10, TimeUnit.SECONDS);
        }
    }

    @Test
    public void test() throws Exception {
    }
}