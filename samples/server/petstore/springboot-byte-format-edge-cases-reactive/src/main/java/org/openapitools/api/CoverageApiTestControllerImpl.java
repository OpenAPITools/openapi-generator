// manually created counterpart to unit tests. Do not delete!
package org.openapitools.api;

import org.openapitools.model.FormParamsRequest;
import org.openapitools.model.MultipartMixedRequestMarker;
import org.openapitools.model.MultipartMixedStatus;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Base64;
import java.util.List;

import org.springframework.core.io.Resource;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.http.codec.multipart.Part;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.server.ServerWebExchange;

@Controller
@RequestMapping("${openapi.multipartByteEdgeCaseCoverage.base-path:}")
public class CoverageApiTestControllerImpl implements CoverageApi {

    private static final Logger logger = LoggerFactory.getLogger(CoverageApiTestControllerImpl.class);

    // Expected content for binary data verification - matches test constants
    private static final byte[] EXPECTED_BYTES = "hello".getBytes(StandardCharsets.UTF_8);
    private static final String EXPECTED_BYTES_STRING = "hello";

    private Mono<Void> verifyBytesContent(byte[] actual, String fieldName) {
        if (!Arrays.equals(EXPECTED_BYTES, actual)) {
            String actualString = new String(actual, StandardCharsets.UTF_8);
            String errorMsg = fieldName + " content mismatch: expected '" + EXPECTED_BYTES_STRING + "', got '" + actualString + "'";
            logger.warn("Validation failed: {}", errorMsg);
            return Mono.error(new IllegalArgumentException(errorMsg));
        }
        logger.debug("Bytes verification passed for field: {}", fieldName);
        return Mono.empty();
    }

    private Mono<Void> verifyBase64Content(String base64Value, String fieldName) {
        try {
            byte[] decoded = Base64.getDecoder().decode(base64Value);
            logger.debug("Base64 decoded successfully for field: {}", fieldName);
            return verifyBytesContent(decoded, fieldName);
        } catch (IllegalArgumentException e) {
            logger.warn("Invalid base64 in field {}: {}", fieldName, e.getMessage());
            return Mono.error(new IllegalArgumentException("Invalid base64 in " + fieldName, e));
        }
    }

    @Override
    public Mono<ResponseEntity<Void>> binaryBody(
            Mono<Resource> body,
            ServerWebExchange exchange
    ) {
        return body.flatMap(resource -> {
                    if (resource == null) {
                        return Mono.error(new IllegalArgumentException("body is required"));
                    }
                    return Mono.fromCallable(() -> {
                        try {
                            byte[] content = readAllBytes(resource.getInputStream());
                            if (content.length == 0) {
                                throw new IllegalArgumentException("body content is empty");
                            }
                            return content;
                        } catch (IOException e) {
                            throw new RuntimeException("Failed to read binary body", e);
                        }
                    }).flatMap(content -> verifyBytesContent(content, "body"));
                }).then(Mono.just(new ResponseEntity<Void>(HttpStatus.NO_CONTENT)))
                .onErrorResume(e -> {
                    exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
                    return Mono.error(e);
                });
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
    public Mono<ResponseEntity<Void>> cookieParams(
            String plain,
            String bytes,
            ServerWebExchange exchange
    ) {
        if (bytes != null) {
            return verifyBase64Content(bytes, "bytes")
                    .then(Mono.just(new ResponseEntity<Void>(HttpStatus.NO_CONTENT)))
                    .onErrorResume(e -> {
                        exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
                        return Mono.error(e);
                    });
        }
        return Mono.just(new ResponseEntity<Void>(HttpStatus.NO_CONTENT));
    }

    @Override
    public Mono<ResponseEntity<Void>> formParams(
            String plain,
            String bytes,
            ServerWebExchange exchange
    ) {
        if (bytes != null) {
            return verifyBase64Content(bytes, "bytes")
                    .then(Mono.just(new ResponseEntity<Void>(HttpStatus.NO_CONTENT)))
                    .onErrorResume(e -> {
                        exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
                        return Mono.error(e);
                    });
        }
        return Mono.just(new ResponseEntity<Void>(HttpStatus.NO_CONTENT));
    }

    @Override
    public Mono<ResponseEntity<Void>> headerParams(
            String xPlain,
            String xByte,
            ServerWebExchange exchange
    ) {
        if (xByte != null) {
            return verifyBase64Content(xByte, "X-Byte")
                    .then(Mono.just(new ResponseEntity<Void>(HttpStatus.NO_CONTENT)))
                    .onErrorResume(e -> {
                        exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
                        return Mono.error(e);
                    });
        }
        return Mono.just(new ResponseEntity<Void>(HttpStatus.NO_CONTENT));
    }

    @Override
    public Mono<ResponseEntity<Void>> jsonBody(
            Mono<FormParamsRequest> formParamsRequest,
            ServerWebExchange exchange
    ) {
        return formParamsRequest.flatMap(request -> {
                    if (request == null) {
                        return Mono.error(new IllegalArgumentException("formParamsRequest is required"));
                    }
                    byte[] bytes = request.getBytes();
                    if (bytes != null) {
                        return verifyBytesContent(bytes, "bytes");
                    }
                    return Mono.empty();
                }).then(Mono.just(new ResponseEntity<Void>(HttpStatus.NO_CONTENT)))
                .onErrorResume(e -> {
                    exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
                    return Mono.error(e);
                });
    }

    @Override
    public Mono<ResponseEntity<Void>> multipartFileArray(
            Flux<Part> files,
            ServerWebExchange exchange
    ) {
        // Expected content for each file in the array - matches test constants
        String[] expectedContents = { "content1", "content2", "content3" };

        if (files == null) {
            return Mono.just(new ResponseEntity<Void>(HttpStatus.NO_CONTENT));
        }

        return files.collectList().flatMap(partList -> {
            return Flux.range(0, partList.size())
                    .flatMap(i -> {
                        Part part = partList.get(i);
                        return part.content()
                                .collectList()
                                .flatMap(buffers -> {
                                    byte[] content = new byte[buffers.stream().mapToInt(b -> b.readableByteCount()).sum()];
                                    int offset = 0;
                                    for (DataBuffer buffer : buffers) {
                                        int readable = buffer.readableByteCount();
                                        buffer.read(content, offset, readable);
                                        offset += readable;
                                    }

                                    if (content.length == 0) {
                                        return Mono.error(new IllegalArgumentException("files[" + i + "] content is empty"));
                                    }

                                    if (i < expectedContents.length) {
                                        byte[] expected = expectedContents[i].getBytes(StandardCharsets.UTF_8);
                                        if (!Arrays.equals(expected, content)) {
                                            String actualString = new String(content, StandardCharsets.UTF_8);
                                            return Mono.error(new IllegalArgumentException(
                                                    "files[" + i + "] content mismatch: expected '" + expectedContents[i] + "', got '" + actualString + "'"));
                                        }
                                    }
                                    return Mono.empty();
                                });
                    })
                    .then(Mono.just(new ResponseEntity<Void>(HttpStatus.NO_CONTENT)));
        }).onErrorResume(e -> {
            exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
            return Mono.error(e);
        });
    }

    @Override
    public Mono<ResponseEntity<Void>> multipartMixed(
            String status,
            Part file,
            Flux<String> statusArray,
            MultipartMixedRequestMarker marker,
            List<MultipartMixedRequestMarker> markerArray,
            ServerWebExchange exchange
    ) {
        if (status == null) {
            return Mono.error(new IllegalArgumentException("status is required"));
        }
        if (file == null) {
            return Mono.error(new IllegalArgumentException("file is required"));
        }

        // Verify enum value is valid
        MultipartMixedStatus enumStatus = MultipartMixedStatus.fromValue(status);
        if (enumStatus == null) {
            return Mono.error(new IllegalArgumentException("status value is invalid: " + status));
        }
        String reconstructedStatus = enumStatus.getValue();
        if (reconstructedStatus == null || reconstructedStatus.isEmpty()) {
            return Mono.error(new IllegalArgumentException("status value is invalid"));
        }

        return file.content()
                .collectList()
                .flatMap(buffers -> {
                    byte[] fileContent = new byte[buffers.stream().mapToInt(b -> b.readableByteCount()).sum()];
                    int offset = 0;
                    for (DataBuffer buffer : buffers) {
                        int readable = buffer.readableByteCount();
                        buffer.read(fileContent, offset, readable);
                        offset += readable;
                    }
                    return verifyBytesContent(fileContent, "file");
                })
                .then(statusArray != null ?
                        statusArray.index()
                                .doOnNext(tuple -> {
                                    long index = tuple.getT1();
                                    String statusValue = tuple.getT2();
                                    try {
                                        MultipartMixedStatus enumValue = MultipartMixedStatus.fromValue(statusValue);
                                        if (enumValue == null || enumValue.getValue() == null) {
                                            throw new IllegalArgumentException("statusArray[" + index + "] contains invalid enum: " + statusValue);
                                        }
                                    } catch (IllegalArgumentException e) {
                                        throw new IllegalArgumentException("statusArray[" + index + "] contains invalid enum: " + statusValue, e);
                                    }
                                })
                                .then()
                        : Mono.empty())
                .then(Mono.fromCallable(() -> {

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
                    return null;
                }))
                .then(Mono.just(new ResponseEntity<Void>(HttpStatus.NO_CONTENT)))
                .onErrorResume(e -> {
                    exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
                    return Mono.error(e);
                });
    }

    @Override
    public Mono<ResponseEntity<Void>> multipartSimple(
            String plain,
            String bytes,
            Part file,
            ServerWebExchange exchange
    ) {
        return Mono.fromCallable(() -> {
                    if (bytes != null) {
                        try {
                            byte[] decoded = Base64.getDecoder().decode(bytes);
                            if (!Arrays.equals(EXPECTED_BYTES, decoded)) {
                                String actualString = new String(decoded, StandardCharsets.UTF_8);
                                throw new IllegalArgumentException(
                                        "bytes content mismatch: expected '" + EXPECTED_BYTES_STRING + "', got '" + actualString + "'");
                            }
                        } catch (IllegalArgumentException e) {
                            throw new IllegalArgumentException("Invalid base64 in bytes", e);
                        }
                    }
                    return null;
                }).then(file != null ?
                        file.content()
                                .collectList()
                                .then(Mono.empty())
                        : Mono.empty())
                .then(Mono.just(new ResponseEntity<Void>(HttpStatus.NO_CONTENT)))
                .onErrorResume(e -> {
                    exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
                    return Mono.error(e);
                });
    }

    @Override
    public Mono<ResponseEntity<Void>> multipartSimpleValidated(
            String plain,
            String bytes,
            Part file,
            ServerWebExchange exchange
    ) {
        return Mono.fromCallable(() -> {
                    if (bytes != null) {
                        try {
                            byte[] decoded = Base64.getDecoder().decode(bytes);
                            if (!Arrays.equals(EXPECTED_BYTES, decoded)) {
                                String actualString = new String(decoded, StandardCharsets.UTF_8);
                                throw new IllegalArgumentException(
                                        "bytes content mismatch: expected '" + EXPECTED_BYTES_STRING + "', got '" + actualString + "'");
                            }
                        } catch (IllegalArgumentException e) {
                            throw new IllegalArgumentException("Invalid base64 in bytes", e);
                        }
                    }
                    return null;
                }).then(file != null ?
                        file.content()
                                .collectList()
                                .flatMap(buffers -> {
                                    byte[] fileContent = new byte[buffers.stream().mapToInt(b -> b.readableByteCount()).sum()];
                                    int offset = 0;
                                    for (DataBuffer buffer : buffers) {
                                        int readable = buffer.readableByteCount();
                                        buffer.read(fileContent, offset, readable);
                                        offset += readable;
                                    }
                                    return verifyBytesContent(fileContent, "file");
                                }).then()
                        : Mono.empty())
                .then(Mono.just(new ResponseEntity<Void>(HttpStatus.NO_CONTENT)))
                .onErrorResume(e -> {
                    exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
                    return Mono.error(e);
                });
    }

    @Override
    public Mono<ResponseEntity<Void>> pathParams(
            String plain,
            String bytes,
            ServerWebExchange exchange
    ) {
        if (plain == null) {
            return Mono.error(new IllegalArgumentException("plain is required"));
        }
        if (bytes == null) {
            return Mono.error(new IllegalArgumentException("bytes is required"));
        }
        return verifyBase64Content(bytes, "bytes")
                .then(Mono.just(new ResponseEntity<Void>(HttpStatus.NO_CONTENT)))
                .onErrorResume(e -> {
                    exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
                    return Mono.error(e);
                });
    }

    @Override
    public Mono<ResponseEntity<Void>> queryParams(
            String plain,
            String bytes,
            ServerWebExchange exchange
    ) {
        if (bytes != null) {
            return verifyBase64Content(bytes, "bytes")
                    .then(Mono.just(new ResponseEntity<Void>(HttpStatus.NO_CONTENT)))
                    .onErrorResume(e -> {
                        exchange.getResponse().setStatusCode(HttpStatus.INTERNAL_SERVER_ERROR);
                        return Mono.error(e);
                    });
        }
        return Mono.just(new ResponseEntity<Void>(HttpStatus.NO_CONTENT));
    }
}

