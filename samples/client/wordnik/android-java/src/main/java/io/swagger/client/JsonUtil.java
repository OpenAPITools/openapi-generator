package io.swagger.client;

public class JsonUtil {
    public static ObjectMapper mapper;

    public static ObjectMapper getJsonMapper() {
        return mapper;
    }

    static {
        mapper = new ObjectMapper();
        mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        mapper.disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);
    }
}
