package org.openapitools.client.api;

import java.io.File;
import org.junit.jupiter.api.Test;

import org.openapitools.client.model.DataChannel;
import org.openapitools.client.model.DataDirection;

import java.nio.file.Files;
import java.nio.file.Paths;

import org.openapitools.client.ApiClient;
import java.nio.charset.StandardCharsets;
import java.util.UUID;
import org.springframework.web.client.RestClient;
import org.springframework.core.io.AbstractResource;
import org.springframework.core.io.FileSystemResource;

import static org.junit.jupiter.api.Assertions.assertTrue;

class BasApiTest {

    @Test
    public void requestTest() {

        RestClient restClient = RestClient.builder() //
                .requestInterceptor(
                        (request, body, execution) -> {
                            if (body != null && body.length > 0) {
                                String bodyString = new String(body, StandardCharsets.UTF_8);
                                bodyString = bodyString.replace("\r", "").replace("\n", "");
                                String expected = "Content-Disposition: form-data; name=\"dataChannel\"Content-Type: text/plain;charset=UTF-8";
                                assertTrue(bodyString.contains(expected));
                            }
                            return execution.execute(request, body);
                        })
                .build();

        ApiClient apiClient = new ApiClient(restClient);
        apiClient.setBasePath("http://localhost:8080");
        apiClient.setUsername("user");
        apiClient.setPassword("password");
        BasApi basApi = new BasApi(apiClient);

        File fileContent = new File("src/test/resources/testdata.xml");
        String idempotencyKey = UUID.randomUUID().toString();
        DataDirection dataDirection = DataDirection.INGOING;
        DataChannel dataChannel = DataChannel.BIKE;
        try {
            basApi.createMessage(fileContent, idempotencyKey, dataDirection, dataChannel);
        } catch (Exception e) {
            // Will happen as we can´t use endpoint
        }

    }

}