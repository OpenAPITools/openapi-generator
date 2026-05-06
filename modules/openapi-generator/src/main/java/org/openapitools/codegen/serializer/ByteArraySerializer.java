package org.openapitools.codegen.serializer;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.ser.std.StdSerializer;

import java.io.IOException;
import java.nio.charset.StandardCharsets;

/**
 * Serializes {@code byte[]} as a UTF-8 string, preserving the original base64 representation
 * of an OpenAPI {@code format: byte} example. swagger-parser stores {@code format: byte}
 * examples as the raw bytes of the input base64 string (not the decoded content), so writing
 * those bytes back as a string round-trips to the user's original value. This prevents
 * Jackson's default YAML behavior of emitting a {@code !!binary} block with a re-encoded payload.
 *
 * <p>Load-bearing assumption: this only round-trips correctly because swagger-parser stores the
 * raw input bytes. If swagger-parser ever decodes {@code format: byte} examples internally, this
 * serializer must switch to {@code Base64.getEncoder().encodeToString(value)}.
 */
public class ByteArraySerializer extends StdSerializer<byte[]> {

    public ByteArraySerializer() {
        super(byte[].class);
    }

    @Override
    public void serialize(byte[] value, JsonGenerator gen, SerializerProvider serializers) throws IOException {
        gen.writeString(new String(value, StandardCharsets.UTF_8));
    }
}
