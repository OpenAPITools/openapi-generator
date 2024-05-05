package org.openapitools.codegen.serializer;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import io.swagger.v3.oas.models.OpenAPI;
import java.util.Map;
import java.io.IOException;
import java.util.Map.Entry;

public class OpenAPISerializer extends JsonSerializer<OpenAPI> {

    @Override
    public void serialize(OpenAPI value, JsonGenerator gen, SerializerProvider serializers) throws IOException {
        if (value != null) {
            gen.writeStartObject();
            writeFieldIfNotNull(gen, "openapi", value.getOpenapi());
            writeObjectFieldIfNotNull(gen, "info", value.getInfo());
            writeObjectFieldIfNotNull(gen, "externalDocs", value.getExternalDocs());
            writeObjectFieldIfNotNull(gen, "servers", value.getServers());
            writeObjectFieldIfNotNull(gen, "security", value.getSecurity());
            writeObjectFieldIfNotNull(gen, "tags", value.getTags());
            writeObjectFieldIfNotNull(gen, "paths", value.getPaths());
            writeObjectFieldIfNotNull(gen, "components", value.getComponents());
            writeExtensionsIfNotNull(gen, value.getExtensions());
            gen.writeEndObject();
        }
    }
    private void writeFieldIfNotNull(JsonGenerator gen, String fieldName, String value) throws IOException {
        if (value != null) {
            gen.writeStringField(fieldName, value);
        }
    }

    private void writeObjectFieldIfNotNull(JsonGenerator gen, String fieldName, Object value) throws IOException {
        if (value != null) {
            gen.writeObjectField(fieldName, value);
        }
    }

    private void writeExtensionsIfNotNull(JsonGenerator gen, Map<String, Object> extensions) throws IOException {
        if (extensions != null) {
            for (Entry<String, Object> entry : extensions.entrySet()) {
                gen.writeObjectField(entry.getKey(), entry.getValue());
            }
        }
    }
}
