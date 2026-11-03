import com.fasterxml.jackson.databind.ObjectMapper;
import feign.Response;
import org.junit.jupiter.api.Test;
import org.openapitools.client.ApiClient;
import org.openapitools.client.api.DefaultApi;
import org.openapitools.client.model.HeaderArg;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import static org.junit.jupiter.api.Assertions.*;

class FeignJacksonHeaderRuntimeTest {
    @Test
    void serializesJsonHeadersAndPreservesPlainHeaders() {
        ApiClient client = new ApiClient();
        client.getFeignBuilder().client((request, options) -> {
            assertEquals(Collections.singletonList("{\"path\":\"/\\u0442\\u0435\\u0441\\u0442\\uD83D\\uDE00\"}"),
                    request.headers().get("X-Json-Arg"));
            assertEquals(Collections.singletonList("plain"), request.headers().get("X-Plain"));
            return Response.builder().request(request).status(204).reason("No Content")
                    .headers(Collections.emptyMap())
                    .build();
        });
        client.buildClient(DefaultApi.class).getMetadata(new HeaderArg().path("/тест😀"), "plain");
    }

    @Test
    void usesConfiguredMapperForJsonHeaders() {
        ApiClient client = new ApiClient();
        ObjectMapper customMapper = client.getObjectMapper().copy();
        com.fasterxml.jackson.databind.module.SimpleModule module = new com.fasterxml.jackson.databind.module.SimpleModule();
        module.addSerializer(HeaderArg.class, new com.fasterxml.jackson.databind.JsonSerializer<HeaderArg>() {
            @Override
            public void serialize(HeaderArg value, com.fasterxml.jackson.core.JsonGenerator generator,
                                  com.fasterxml.jackson.databind.SerializerProvider provider) throws java.io.IOException {
                generator.writeStartObject();
                generator.writeStringField("custom", value.getPath());
                generator.writeEndObject();
            }
        });
        customMapper.registerModule(module);
        client.setObjectMapper(customMapper);
        client.getFeignBuilder().client((request, options) -> {
            assertEquals(Collections.singletonList("{\"custom\":\"/x\"}"), request.headers().get("X-Json-Arg"));
            return Response.builder().request(request).status(204).reason("No Content").headers(Collections.emptyMap())
                    .build();
        });
        client.buildClient(DefaultApi.class).getMetadata(new HeaderArg().path("/x"), "plain");
    }

}
