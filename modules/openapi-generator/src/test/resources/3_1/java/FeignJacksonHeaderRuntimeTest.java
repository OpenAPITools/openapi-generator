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

    @Test
    void serializesJsonArrayAsOneHeader() {
        ApiClient client = new ApiClient();
        client.getFeignBuilder().client((request, options) -> {
            assertEquals(Collections.singletonList("[{\"path\":\"/a\"},{\"path\":\"/b\"}]"),
                    request.headers().get("X-Json-Array"));
            return Response.builder().request(request).status(204).reason("No Content")
                    .headers(Collections.emptyMap()).build();
        });
        client.buildClient(DefaultApi.class).getArray(java.util.Arrays.asList(new HeaderArg().path("/a"), new HeaderArg().path("/b")));
    }

    @Test
    void replacingBuilderKeepsClientMapper() {
        ApiClient client = new ApiClient();
        com.fasterxml.jackson.databind.module.SimpleModule module = new com.fasterxml.jackson.databind.module.SimpleModule();
        module.addSerializer(HeaderArg.class, new com.fasterxml.jackson.databind.JsonSerializer<HeaderArg>() {
            @Override
            public void serialize(HeaderArg value, com.fasterxml.jackson.core.JsonGenerator generator,
                                  com.fasterxml.jackson.databind.SerializerProvider provider) throws java.io.IOException {
                generator.writeString("custom");
            }
        });
        client.getObjectMapper().registerModule(module);
        client.setFeignBuilder(feign.Feign.builder().client((request, options) -> {
            assertEquals(Collections.singletonList("\"custom\""), request.headers().get("X-Json-Arg"));
            return Response.builder().request(request).status(204).reason("No Content").headers(Collections.emptyMap()).build();
        }));
        client.buildClient(DefaultApi.class).getMetadata(new HeaderArg().path("/x").optionalValue("v"), "plain");
    }

    @Test
    void requestBodyMapperRetainsNullBehavior() throws Exception {
        String json = new ApiClient().getObjectMapper().writeValueAsString(new HeaderArg().path("/x"));
        assertTrue(json.contains("\"optionalValue\":null"));
    }

    @Test
    void serializesEmptyJsonArray() {
        ApiClient client = new ApiClient();
        client.getFeignBuilder().client((request, options) -> {
            assertEquals(Collections.singletonList("[]"), request.headers().get("X-Json-Array"));
            return Response.builder().request(request).status(204).reason("No Content")
                    .headers(Collections.emptyMap()).build();
        });
        DefaultApi api = client.buildClient(DefaultApi.class);
        assertEquals(api, api);
        DefaultApi other = client.buildClient(DefaultApi.class);
        assertEquals(api, other);
        assertEquals(api.hashCode(), other.hashCode());
        api.getArray(Collections.emptyList());
    }

}
