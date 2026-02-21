// manually created counterpart to unit tests. Do not delete!
package org.openapitools.api;

import org.openapitools.model.FormParamsRequest;
import org.openapitools.model.MultipartMixedRequestMarker;
import org.openapitools.model.MultipartMixedStatus;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.multipart.MultipartFile;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Base64;
import java.util.List;

@Controller
@RequestMapping("${openapi.multipartByteEdgeCaseCoverage.base-path:}")
public class CoverageApiTestControllerImpl implements CoverageApi {

    // Expected content for binary data verification - matches test constants
    private static final byte[] EXPECTED_BYTES = "hello".getBytes(StandardCharsets.UTF_8);
    private static final String EXPECTED_BYTES_STRING = "hello";

    private void verifyBytesContent(byte[] actual, String fieldName) {
        if (!Arrays.equals(EXPECTED_BYTES, actual)) {
            String actualString = new String(actual, StandardCharsets.UTF_8);
            throw new IllegalArgumentException(
                    fieldName + " content mismatch: expected '" + EXPECTED_BYTES_STRING + "', got '" + actualString + "'");
        }
    }

    private void verifyBase64Content(String base64Value, String fieldName) {
        byte[] decoded = Base64.getDecoder().decode(base64Value);
        verifyBytesContent(decoded, fieldName);
    }

    @Override
    public ResponseEntity<Void> binaryBody(Resource body) {
        if (body == null) {
            throw new IllegalArgumentException("body is required");
        }
        try {
            byte[] content = readAllBytes(body.getInputStream());
            if (content.length == 0) {
                throw new IllegalArgumentException("body content is empty");
            }
            verifyBytesContent(content, "body");
        } catch (IOException e) {
            throw new RuntimeException("Failed to read binary body", e);
        }
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

    private byte[] readAllBytes(InputStream inputStream) throws IOException {
        ByteArrayOutputStream buffer = new ByteArrayOutputStream();
        int nRead;
        byte[] data = new byte[1024];
        while ((nRead = inputStream.read(data, 0, data.length)) != -1) {
            buffer.write(data, 0, nRead);
        }
        buffer.flush();
        return buffer.toByteArray();
    }

    @Override
    public ResponseEntity<Void> cookieParams(String plain, String bytes) {
        if (bytes != null) {
            verifyBase64Content(bytes, "bytes");
        }
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

    @Override
    public ResponseEntity<Void> formParams(String plain, String bytes) {
        if (bytes != null) {
            verifyBase64Content(bytes, "bytes");
        }
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

    @Override
    public ResponseEntity<Void> headerParams(String xPlain, String xByte) {
        if (xByte != null) {
            verifyBase64Content(xByte, "X-Byte");
        }
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

    @Override
    public ResponseEntity<Void> jsonBody(FormParamsRequest formParamsRequest) {
        if (formParamsRequest == null) {
            throw new IllegalArgumentException("formParamsRequest is required");
        }
        byte[] bytes = formParamsRequest.getBytes();
        if (bytes != null) {
            verifyBytesContent(bytes, "bytes");
        }
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

    @Override
    public ResponseEntity<Void> multipartFileArray(List<MultipartFile> files) {
        // Expected content for each file in the array - matches test constants
        String[] expectedContents = {"content1", "content2", "content3"};

        if (files != null) {
            for (int i = 0; i < files.size(); i++) {
                MultipartFile file = files.get(i);
                try {
                    byte[] content = file.getBytes();
                    if (content.length == 0) {
                        throw new IllegalArgumentException("files[" + i + "] content is empty");
                    }
                    // Verify content matches expected value for this index
                    if (i < expectedContents.length) {
                        byte[] expected = expectedContents[i].getBytes(StandardCharsets.UTF_8);
                        if (!Arrays.equals(expected, content)) {
                            String actualString = new String(content, StandardCharsets.UTF_8);
                            throw new IllegalArgumentException(
                                    "files[" + i + "] content mismatch: expected '" + expectedContents[i] + "', got '" + actualString + "'");
                        }
                    }
                } catch (IOException e) {
                    throw new RuntimeException("Failed to read file", e);
                }
            }
        }
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

    @Override
    public ResponseEntity<Void> multipartMixed(
            MultipartMixedStatus status,
            MultipartFile file,
            List<MultipartMixedStatus> statusArray,
            MultipartMixedRequestMarker marker,
            List<MultipartMixedRequestMarker> markerArray
    ) {
        if (status == null) {
            throw new IllegalArgumentException("status is required");
        }
        if (file == null) {
            throw new IllegalArgumentException("file is required");
        }

        // Verify enum value is valid
        String reconstructedStatus = status.getValue();
        if (reconstructedStatus == null || reconstructedStatus.isEmpty()) {
            throw new IllegalArgumentException("status value is invalid");
        }

        // Verify file content matches expected bytes
        try {
            byte[] fileContent = file.getBytes();
            verifyBytesContent(fileContent, "file");
        } catch (IOException e) {
            throw new RuntimeException("Failed to read file", e);
        }

        // Verify enum array values
        if (statusArray != null) {
            for (int i = 0; i < statusArray.size(); i++) {
                MultipartMixedStatus s = statusArray.get(i);
                String value = s.getValue();
                if (value == null) {
                    throw new IllegalArgumentException("statusArray[" + i + "] contains invalid enum");
                }
            }
        }

        // Verify POJO fields
        if (marker != null) {
            String name = marker.getName();
            if (name == null) {
                throw new IllegalArgumentException("marker.name is required");
            }
        }

        // Verify POJO array fields
        if (markerArray != null) {
            for (int i = 0; i < markerArray.size(); i++) {
                MultipartMixedRequestMarker m = markerArray.get(i);
                String name = m.getName();
                if (name == null) {
                    throw new IllegalArgumentException("markerArray[" + i + "].name is required");
                }
            }
        }

        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

    @Override
    public ResponseEntity<Void> multipartSimple(String plain, String bytes, MultipartFile file) {
        // This endpoint does NOT validate file content - allows empty files
        if (bytes != null) {
            verifyBase64Content(bytes, "bytes");
        }
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

    @Override
    public ResponseEntity<Void> multipartSimpleValidated(String plain, String bytes, MultipartFile file) {
        // This endpoint validates file content
        if (bytes != null) {
            verifyBase64Content(bytes, "bytes");
        }
        if (file != null) {
            try {
                byte[] content = file.getBytes();
                if (content.length == 0) {
                    throw new IllegalArgumentException("file content is empty");
                }
                verifyBytesContent(content, "file");
            } catch (IOException e) {
                throw new RuntimeException("Failed to read file", e);
            }
        }
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

    @Override
    public ResponseEntity<Void> pathParams(String plain, String bytes) {
        if (plain == null) {
            throw new IllegalArgumentException("plain is required");
        }
        if (bytes == null) {
            throw new IllegalArgumentException("bytes is required");
        }
        verifyBase64Content(bytes, "bytes");
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

    @Override
    public ResponseEntity<Void> queryParams(String plain, String bytes) {
        if (bytes != null) {
            verifyBase64Content(bytes, "bytes");
        }
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }
}

