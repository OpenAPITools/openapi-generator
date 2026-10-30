// manually created counterpart to unit tests. Do not delete!
package org.openapitools.api;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;
import org.openapitools.model.MultipartMixedRequestMarker;
import org.openapitools.model.MultipartMixedStatus;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.reactive.AutoConfigureWebTestClient;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.core.io.ByteArrayResource;
import org.springframework.http.MediaType;
import org.springframework.http.client.MultipartBodyBuilder;
import org.springframework.test.web.reactive.server.WebTestClient;
import org.springframework.web.reactive.function.BodyInserters;

import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Base64;
import java.util.List;


@SpringBootTest
@AutoConfigureWebTestClient
class CoverageApiIntegrationTest {

    @Autowired
    private WebTestClient webTestClient;

    @Autowired
    private ObjectMapper objectMapper;

    private static final byte[] SAMPLE_BYTES = "hello".getBytes(StandardCharsets.UTF_8);
    private static final String SAMPLE_BYTES_BASE64 = Base64.getEncoder().encodeToString(SAMPLE_BYTES);

    // ==================== multipartMixed tests ====================
    // Covers: enum @RequestParam, file @RequestPart, enum array @RequestParam,
    //         object @RequestPart, object array @RequestPart

    @Test
    void multipartMixed_withRequiredFieldsOnly_returns204() throws Exception {
        MultipartBodyBuilder bodyBuilder = new MultipartBodyBuilder();
        bodyBuilder.part("file", new ByteArrayResource(SAMPLE_BYTES), MediaType.APPLICATION_OCTET_STREAM);
        bodyBuilder.part("status", MultipartMixedStatus.ALLOWED.getValue());

        webTestClient
                .post()
                .uri(CoverageApi.PATH_MULTIPART_MIXED)
                .contentType(MediaType.MULTIPART_FORM_DATA)
                .body(BodyInserters.fromMultipartData(bodyBuilder.build()))
                .exchange()
                .expectStatus().isNoContent();
    }

    @Test
    void multipartMixed_withAllFields_returns204() throws Exception {
        MultipartMixedRequestMarker marker = new MultipartMixedRequestMarker("marker1")
                .priority(10)
                .active(true);

        List<MultipartMixedRequestMarker> markerArray = Arrays.asList(
                new MultipartMixedRequestMarker("marker2").priority(20).active(false),
                new MultipartMixedRequestMarker("marker3").priority(30).active(true)
        );

        MultipartBodyBuilder bodyBuilder = new MultipartBodyBuilder();
        bodyBuilder.part("file", new ByteArrayResource(SAMPLE_BYTES), MediaType.APPLICATION_OCTET_STREAM);
        bodyBuilder.part("status", MultipartMixedStatus.IN_PROGRESS.getValue());
        bodyBuilder.part("statusArray", MultipartMixedStatus.ALLOWED.getValue());
        bodyBuilder.part("statusArray", MultipartMixedStatus.REJECTED.getValue());
        bodyBuilder.part("marker", objectMapper.writeValueAsString(marker), MediaType.APPLICATION_JSON);
        bodyBuilder.part("markerArray", objectMapper.writeValueAsString(markerArray), MediaType.APPLICATION_JSON);

        webTestClient
                .post()
                .uri(CoverageApi.PATH_MULTIPART_MIXED)
                .contentType(MediaType.MULTIPART_FORM_DATA)
                .body(BodyInserters.fromMultipartData(bodyBuilder.build()))
                .exchange()
                .expectStatus().isNoContent();
    }

    @Test
    void multipartMixed_enumValue_deserializesCorrectly() {
        for (MultipartMixedStatus status : MultipartMixedStatus.values()) {
            MultipartBodyBuilder bodyBuilder = new MultipartBodyBuilder();
            bodyBuilder.part("file", new ByteArrayResource(SAMPLE_BYTES), MediaType.APPLICATION_OCTET_STREAM);
            bodyBuilder.part("status", status.getValue());

            webTestClient
                    .post()
                    .uri(CoverageApi.PATH_MULTIPART_MIXED)
                    .contentType(MediaType.MULTIPART_FORM_DATA)
                    .body(BodyInserters.fromMultipartData(bodyBuilder.build()))
                    .exchange()
                    .expectStatus().isNoContent();
        }
    }

    // ==================== multipartSimple tests ====================
    // Covers: scalar @RequestParam, base64 bytes @RequestParam, file @RequestPart

    @Test
    void multipartSimple_withAllFields_returns204() {
        MultipartBodyBuilder bodyBuilder = new MultipartBodyBuilder();
        bodyBuilder.part("file", new ByteArrayResource("content".getBytes()), MediaType.TEXT_PLAIN);
        bodyBuilder.part("plain", "plainText");
        bodyBuilder.part("bytes", SAMPLE_BYTES_BASE64);

        webTestClient
                .post()
                .uri(CoverageApi.PATH_MULTIPART_SIMPLE)
                .contentType(MediaType.MULTIPART_FORM_DATA)
                .body(BodyInserters.fromMultipartData(bodyBuilder.build()))
                .exchange()
                .expectStatus().isNoContent();
    }

    @Test
    void multipartSimple_withNoFields_returns204() {
        MultipartBodyBuilder bodyBuilder = new MultipartBodyBuilder();

        webTestClient
                .post()
                .uri(CoverageApi.PATH_MULTIPART_SIMPLE)
                .body(BodyInserters.fromMultipartData(bodyBuilder.build()))
                .exchange()
                .expectStatus().isNoContent();
    }

    @Test
    void multipartSimple_withEmptyFilePart_returns204() {
        MultipartBodyBuilder bodyBuilder = new MultipartBodyBuilder();
        bodyBuilder.part("file", new ByteArrayResource(new byte[0]), MediaType.APPLICATION_OCTET_STREAM);

        webTestClient
                .post()
                .uri(CoverageApi.PATH_MULTIPART_SIMPLE)
                .contentType(MediaType.MULTIPART_FORM_DATA)
                .body(BodyInserters.fromMultipartData(bodyBuilder.build()))
                .exchange()
                .expectStatus().isNoContent();
    }

    // ==================== multipartSimpleValidated tests ====================
    // Covers: validated file content deserialization

    @Test
    void multipartSimpleValidated_withValidContent_returns204() {
        MultipartBodyBuilder bodyBuilder = new MultipartBodyBuilder();
        bodyBuilder.part("file", new ByteArrayResource(SAMPLE_BYTES), MediaType.TEXT_PLAIN);
        bodyBuilder.part("plain", "plainText");
        bodyBuilder.part("bytes", SAMPLE_BYTES_BASE64);

        webTestClient
                .post()
                .uri(CoverageApi.PATH_MULTIPART_SIMPLE_VALIDATED)
                .contentType(MediaType.MULTIPART_FORM_DATA)
                .body(BodyInserters.fromMultipartData(bodyBuilder.build()))
                .exchange()
                .expectStatus().isNoContent();
    }

    // ==================== multipartFileArray tests ====================
    // Covers: array of files @RequestPart List<Part>

    @Test
    void multipartFileArray_withMultipleFiles_returns204() {
        MultipartBodyBuilder bodyBuilder = new MultipartBodyBuilder();
        bodyBuilder.part("files", new ByteArrayResource("content1".getBytes()), MediaType.APPLICATION_OCTET_STREAM)
                .filename("file1.bin");
        bodyBuilder.part("files", new ByteArrayResource("content2".getBytes()), MediaType.APPLICATION_OCTET_STREAM)
                .filename("file2.bin");
        bodyBuilder.part("files", new ByteArrayResource("content3".getBytes()), MediaType.APPLICATION_OCTET_STREAM)
                .filename("file3.bin");

        webTestClient
                .post()
                .uri(CoverageApi.PATH_MULTIPART_FILE_ARRAY)
                .contentType(MediaType.MULTIPART_FORM_DATA)
                .body(BodyInserters.fromMultipartData(bodyBuilder.build()))
                .exchange()
                .expectStatus().isNoContent();
    }

    @Test
    void multipartFileArray_withNoFiles_returns204() {
        MultipartBodyBuilder bodyBuilder = new MultipartBodyBuilder();

        webTestClient
                .post()
                .uri(CoverageApi.PATH_MULTIPART_FILE_ARRAY)
                .body(BodyInserters.fromMultipartData(bodyBuilder.build()))
                .exchange()
                .expectStatus().isNoContent();
    }

    // ==================== formParams tests ====================
    // Covers: application/x-www-form-urlencoded with @RequestParam

    @Test
    void formParams_withAllFields_returns204() {
        webTestClient
                .post()
                .uri(CoverageApi.PATH_FORM_PARAMS)
                .contentType(MediaType.APPLICATION_FORM_URLENCODED)
                .bodyValue("plain=plainValue&bytes=" + SAMPLE_BYTES_BASE64)
                .exchange()
                .expectStatus().isNoContent();
    }

    @Test
    void formParams_withNoFields_returns204() {
        webTestClient
                .post()
                .uri(CoverageApi.PATH_FORM_PARAMS)
                .contentType(MediaType.APPLICATION_FORM_URLENCODED)
                .exchange()
                .expectStatus().isNoContent();
    }

    // ==================== queryParams tests ====================
    // Covers: query string @RequestParam

    @Test
    void queryParams_withAllParams_returns204() {
        webTestClient
                .get()
                .uri(uriBuilder -> uriBuilder
                        .path(CoverageApi.PATH_QUERY_PARAMS)
                        .queryParam("plain", "queryPlain")
                        .queryParam("bytes", SAMPLE_BYTES_BASE64)
                        .build())
                .exchange()
                .expectStatus().isNoContent();
    }

    @Test
    void queryParams_withNoParams_returns204() {
        webTestClient
                .get()
                .uri(CoverageApi.PATH_QUERY_PARAMS)
                .exchange()
                .expectStatus().isNoContent();
    }

    // ==================== pathParams tests ====================
    // Covers: @PathVariable

    @Test
    void pathParams_withBothParams_returns204() {
        webTestClient
                .get()
                .uri("/coverage/path/{plain}/{bytes}", "pathPlain", SAMPLE_BYTES_BASE64)
                .exchange()
                .expectStatus().isNoContent();
    }

    // ==================== headerParams tests ====================
    // Covers: @RequestHeader

    @Test
    void headerParams_withAllHeaders_returns204() {
        webTestClient
                .get()
                .uri(CoverageApi.PATH_HEADER_PARAMS)
                .header("X-Plain", "headerPlain")
                .header("X-Byte", SAMPLE_BYTES_BASE64)
                .exchange()
                .expectStatus().isNoContent();
    }

    @Test
    void headerParams_withNoHeaders_returns204() {
        webTestClient
                .get()
                .uri(CoverageApi.PATH_HEADER_PARAMS)
                .exchange()
                .expectStatus().isNoContent();
    }

    // ==================== cookieParams tests ====================
    // Covers: @CookieValue

    @Test
    void cookieParams_withAllCookies_returns204() {
        webTestClient
                .get()
                .uri(CoverageApi.PATH_COOKIE_PARAMS)
                .cookie("plain", "cookiePlain")
                .cookie("bytes", SAMPLE_BYTES_BASE64)
                .exchange()
                .expectStatus().isNoContent();
    }

    @Test
    void cookieParams_withNoCookies_returns204() {
        webTestClient
                .get()
                .uri(CoverageApi.PATH_COOKIE_PARAMS)
                .exchange()
                .expectStatus().isNoContent();
    }

    // ==================== jsonBody tests ====================
    // Covers: @RequestBody with JSON POJO

    @Test
    void jsonBody_withValidBody_returns204() {
        String json = "{\"plain\": \"jsonPlain\", \"bytes\": \"" + SAMPLE_BYTES_BASE64 + "\"}";

        webTestClient
                .post()
                .uri(CoverageApi.PATH_JSON_BODY)
                .contentType(MediaType.APPLICATION_JSON)
                .bodyValue(json)
                .exchange()
                .expectStatus().isNoContent();
    }

    @Test
    void jsonBody_withEmptyObject_returns204() {
        webTestClient
                .post()
                .uri(CoverageApi.PATH_JSON_BODY)
                .contentType(MediaType.APPLICATION_JSON)
                .bodyValue("{}")
                .exchange()
                .expectStatus().isNoContent();
    }

    // ==================== binaryBody tests ====================
    // Covers: @RequestBody with application/octet-stream Resource

    @Test
    void binaryBody_withBinaryContent_returns204() {
        webTestClient
                .post()
                .uri(CoverageApi.PATH_BINARY_BODY)
                .contentType(MediaType.APPLICATION_OCTET_STREAM)
                .bodyValue(SAMPLE_BYTES)
                .exchange()
                .expectStatus().isNoContent();
    }

}
