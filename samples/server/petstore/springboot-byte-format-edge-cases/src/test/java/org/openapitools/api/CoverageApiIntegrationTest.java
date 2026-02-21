// manually created counterpart to unit tests. Do not delete!
package org.openapitools.api;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;
import org.openapitools.model.MultipartMixedRequestMarker;
import org.openapitools.model.MultipartMixedStatus;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.web.servlet.MockMvc;

import javax.servlet.http.Cookie;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Base64;
import java.util.List;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
@SpringBootTest
@AutoConfigureMockMvc
class CoverageApiIntegrationTest {

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private ObjectMapper objectMapper;

    private static final byte[] SAMPLE_BYTES = "hello".getBytes(StandardCharsets.UTF_8);
    private static final String SAMPLE_BYTES_BASE64 = Base64.getEncoder().encodeToString(SAMPLE_BYTES);

    // ==================== multipartMixed tests ====================
    // Covers: enum @RequestParam, file @RequestPart, enum array @RequestParam,
    //         object @RequestPart, object array @RequestPart

    @Test
    void multipartMixed_withRequiredFieldsOnly_returns204() throws Exception {
        MockMultipartFile file = new MockMultipartFile(
                "file", "test.bin", MediaType.APPLICATION_OCTET_STREAM_VALUE, SAMPLE_BYTES);

        mockMvc.perform(multipart(CoverageApi.PATH_MULTIPART_MIXED)
                        .file(file)
                        .param("status", MultipartMixedStatus.ALLOWED.getValue()))
                .andExpect(status().isNoContent());
    }

    @Test
    void multipartMixed_withAllFields_returns204() throws Exception {
        MockMultipartFile file = new MockMultipartFile(
                "file", "test.bin", MediaType.APPLICATION_OCTET_STREAM_VALUE, SAMPLE_BYTES);

        MultipartMixedRequestMarker marker = new MultipartMixedRequestMarker("marker1")
                .priority(10)
                .active(true);

        List<MultipartMixedRequestMarker> markerArray = Arrays.asList(
                new MultipartMixedRequestMarker("marker2").priority(20).active(false),
                new MultipartMixedRequestMarker("marker3").priority(30).active(true)
        );

        // marker as JSON part
        MockMultipartFile markerPart = new MockMultipartFile(
                "marker", "", MediaType.APPLICATION_JSON_VALUE,
                objectMapper.writeValueAsBytes(marker));

        // markerArray as JSON part
        MockMultipartFile markerArrayPart = new MockMultipartFile(
                "markerArray", "", MediaType.APPLICATION_JSON_VALUE,
                objectMapper.writeValueAsBytes(markerArray));

        mockMvc.perform(multipart(CoverageApi.PATH_MULTIPART_MIXED)
                        .file(file)
                        .file(markerPart)
                        .file(markerArrayPart)
                        .param("status", MultipartMixedStatus.IN_PROGRESS.getValue())
                        .param("statusArray", MultipartMixedStatus.ALLOWED.getValue(),
                                MultipartMixedStatus.REJECTED.getValue()))
                .andExpect(status().isNoContent());
    }

    @Test
    void multipartMixed_enumValue_deserializesCorrectly() throws Exception {
        MockMultipartFile file = new MockMultipartFile(
                "file", "test.bin", MediaType.APPLICATION_OCTET_STREAM_VALUE, SAMPLE_BYTES);

        // Test each enum value
        for (MultipartMixedStatus status : MultipartMixedStatus.values()) {
            mockMvc.perform(multipart(CoverageApi.PATH_MULTIPART_MIXED)
                            .file(file)
                            .param("status", status.getValue()))
                    .andExpect(status().isNoContent());
        }
    }

    // ==================== multipartSimple tests ====================
    // Covers: scalar @RequestParam, base64 bytes @RequestParam, file @RequestPart

    @Test
    void multipartSimple_withAllFields_returns204() throws Exception {
        MockMultipartFile file = new MockMultipartFile(
                "file", "test.txt", MediaType.TEXT_PLAIN_VALUE, SAMPLE_BYTES);

        mockMvc.perform(multipart(CoverageApi.PATH_MULTIPART_SIMPLE)
                        .file(file)
                        .param("plain", "plainText")
                        .param("bytes", SAMPLE_BYTES_BASE64))
                .andExpect(status().isNoContent());
    }

    @Test
    void multipartSimple_withEmptyFilePart_returns204() throws Exception {
        MockMultipartFile emptyFile =
                new MockMultipartFile("file", "", MediaType.APPLICATION_OCTET_STREAM_VALUE, new byte[0]);

        mockMvc.perform(multipart(CoverageApi.PATH_MULTIPART_SIMPLE)
                        .file(emptyFile))
                .andExpect(status().isNoContent());
    }

    // ==================== multipartSimpleValidated tests ====================
    // Covers: validated file content deserialization

    @Test
    void multipartSimpleValidated_withValidContent_returns204() throws Exception {
        MockMultipartFile file = new MockMultipartFile(
                "file", "test.txt", MediaType.TEXT_PLAIN_VALUE, SAMPLE_BYTES);

        mockMvc.perform(multipart(CoverageApi.PATH_MULTIPART_SIMPLE_VALIDATED)
                        .file(file)
                        .param("plain", "plainText")
                        .param("bytes", SAMPLE_BYTES_BASE64))
                .andExpect(status().isNoContent());
    }

    // ==================== multipartFileArray tests ====================
    // Covers: array of files @RequestPart List<MultipartFile>

    @Test
    void multipartFileArray_withMultipleFiles_returns204() throws Exception {
        MockMultipartFile file1 = new MockMultipartFile(
                "files", "file1.bin", MediaType.APPLICATION_OCTET_STREAM_VALUE, "content1".getBytes());
        MockMultipartFile file2 = new MockMultipartFile(
                "files", "file2.bin", MediaType.APPLICATION_OCTET_STREAM_VALUE, "content2".getBytes());
        MockMultipartFile file3 = new MockMultipartFile(
                "files", "file3.bin", MediaType.APPLICATION_OCTET_STREAM_VALUE, "content3".getBytes());

        mockMvc.perform(multipart(CoverageApi.PATH_MULTIPART_FILE_ARRAY)
                        .file(file1)
                        .file(file2)
                        .file(file3))
                .andExpect(status().isNoContent());
    }

    @Test
    void multipartFileArray_withNoFiles_returns204() throws Exception {
        mockMvc.perform(multipart(CoverageApi.PATH_MULTIPART_FILE_ARRAY))
                .andExpect(status().isNoContent());
    }

    // ==================== formParams tests ====================
    // Covers: application/x-www-form-urlencoded with @RequestParam

    @Test
    void formParams_withAllFields_returns204() throws Exception {
        mockMvc.perform(post(CoverageApi.PATH_FORM_PARAMS)
                        .contentType(MediaType.APPLICATION_FORM_URLENCODED)
                        .param("plain", "plainValue")
                        .param("bytes", SAMPLE_BYTES_BASE64))
                .andExpect(status().isNoContent());
    }

    @Test
    void formParams_withNoFields_returns204() throws Exception {
        mockMvc.perform(post(CoverageApi.PATH_FORM_PARAMS)
                        .contentType(MediaType.APPLICATION_FORM_URLENCODED))
                .andExpect(status().isNoContent());
    }

    // ==================== queryParams tests ====================
    // Covers: query string @RequestParam

    @Test
    void queryParams_withAllParams_returns204() throws Exception {
        mockMvc.perform(get(CoverageApi.PATH_QUERY_PARAMS)
                        .param("plain", "queryPlain")
                        .param("bytes", SAMPLE_BYTES_BASE64))
                .andExpect(status().isNoContent());
    }

    @Test
    void queryParams_withNoParams_returns204() throws Exception {
        mockMvc.perform(get(CoverageApi.PATH_QUERY_PARAMS))
                .andExpect(status().isNoContent());
    }

    // ==================== pathParams tests ====================
    // Covers: @PathVariable

    @Test
    void pathParams_withBothParams_returns204() throws Exception {
        mockMvc.perform(get("/coverage/path/{plain}/{bytes}", "pathPlain", SAMPLE_BYTES_BASE64))
                .andExpect(status().isNoContent());
    }

    // ==================== headerParams tests ====================
    // Covers: @RequestHeader

    @Test
    void headerParams_withAllHeaders_returns204() throws Exception {
        mockMvc.perform(get(CoverageApi.PATH_HEADER_PARAMS)
                        .header("X-Plain", "headerPlain")
                        .header("X-Byte", SAMPLE_BYTES_BASE64))
                .andExpect(status().isNoContent());
    }

    @Test
    void headerParams_withNoHeaders_returns204() throws Exception {
        mockMvc.perform(get(CoverageApi.PATH_HEADER_PARAMS))
                .andExpect(status().isNoContent());
    }

    // ==================== cookieParams tests ====================
    // Covers: @CookieValue

    @Test
    void cookieParams_withAllCookies_returns204() throws Exception {
        mockMvc.perform(get(CoverageApi.PATH_COOKIE_PARAMS)
                        .cookie(new Cookie("plain", "cookiePlain"))
                        .cookie(new Cookie("bytes", SAMPLE_BYTES_BASE64)))
                .andExpect(status().isNoContent());
    }

    @Test
    void cookieParams_withNoCookies_returns204() throws Exception {
        mockMvc.perform(get(CoverageApi.PATH_COOKIE_PARAMS))
                .andExpect(status().isNoContent());
    }

    // ==================== jsonBody tests ====================
    // Covers: @RequestBody with JSON POJO

    @Test
    void jsonBody_withValidBody_returns204() throws Exception {
        String json = "{\"plain\": \"jsonPlain\", \"bytes\": \"" + SAMPLE_BYTES_BASE64 + "\"}";

        mockMvc.perform(post(CoverageApi.PATH_JSON_BODY)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(json))
                .andExpect(status().isNoContent());
    }

    @Test
    void jsonBody_withEmptyObject_returns204() throws Exception {
        mockMvc.perform(post(CoverageApi.PATH_JSON_BODY)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("{}"))
                .andExpect(status().isNoContent());
    }

    // ==================== binaryBody tests ====================
    // Covers: @RequestBody with application/octet-stream Resource

    @Test
    void binaryBody_withBinaryContent_returns204() throws Exception {
        mockMvc.perform(post(CoverageApi.PATH_BINARY_BODY)
                        .contentType(MediaType.APPLICATION_OCTET_STREAM)
                        .content(SAMPLE_BYTES))
                .andExpect(status().isNoContent());
    }
}

