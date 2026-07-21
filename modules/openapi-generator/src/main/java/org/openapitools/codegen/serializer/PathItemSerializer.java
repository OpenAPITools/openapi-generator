package org.openapitools.codegen.serializer;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import io.swagger.v3.oas.models.PathItem;

import java.io.IOException;
import java.util.Map;

/**
 * Serializes a {@link PathItem} with HTTP methods written in classical OpenAPI spec order:
 * GET, PUT, POST, DELETE, OPTIONS, HEAD, PATCH, TRACE — instead of Jackson's default
 * alphabetical order (which would put DELETE before GET).
 */
public class PathItemSerializer extends JsonSerializer<PathItem> {

    @Override
    public void serialize(PathItem value, JsonGenerator gen, SerializerProvider serializers) throws IOException {
        gen.writeStartObject();
        if (value.getSummary() != null) {
            gen.writeStringField("summary", value.getSummary());
        }
        if (value.getDescription() != null) {
            gen.writeStringField("description", value.getDescription());
        }
        // HTTP methods in classical OpenAPI spec order
        if (value.getGet() != null) {
            gen.writeObjectField("get", value.getGet());
        }
        if (value.getPut() != null) {
            gen.writeObjectField("put", value.getPut());
        }
        if (value.getPost() != null) {
            gen.writeObjectField("post", value.getPost());
        }
        if (value.getDelete() != null) {
            gen.writeObjectField("delete", value.getDelete());
        }
        if (value.getOptions() != null) {
            gen.writeObjectField("options", value.getOptions());
        }
        if (value.getHead() != null) {
            gen.writeObjectField("head", value.getHead());
        }
        if (value.getPatch() != null) {
            gen.writeObjectField("patch", value.getPatch());
        }
        if (value.getTrace() != null) {
            gen.writeObjectField("trace", value.getTrace());
        }
        if (value.getServers() != null) {
            gen.writeObjectField("servers", value.getServers());
        }
        if (value.getParameters() != null) {
            gen.writeObjectField("parameters", value.getParameters());
        }
        if (value.getExtensions() != null) {
            for (Map.Entry<String, Object> e : value.getExtensions().entrySet()) {
                gen.writeObjectField(e.getKey(), e.getValue());
            }
        }
        gen.writeEndObject();
    }
}
