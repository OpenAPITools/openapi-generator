import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;
import org.openapitools.client.ApiClient;
import org.openapitools.client.model.Metadata;
import org.openapitools.client.model.OpenMetadata;
import org.openapitools.client.model.Folder;
import org.openapitools.client.model.Other;
import static org.junit.jupiter.api.Assertions.*;

class FeignJacksonOneOfRuntimeTest {
    @Test
    void deserializesClosedAndOpenTaggedUnions() throws Exception {
        ObjectMapper mapper = new ApiClient().getObjectMapper();
        assertInstanceOf(org.openapitools.client.model.File.class, mapper.readValue("{\".tag\":\"file\",\"name\":\"x\"}", Metadata.class));
        assertInstanceOf(Folder.class, mapper.readValue("{\".tag\":\"folder\",\"name\":\"x\"}", Metadata.class));
        assertThrows(Exception.class, () -> mapper.readValue("{\".tag\":\"other\"}", Metadata.class));
        assertInstanceOf(Other.class, mapper.readValue("{\".tag\":\"other\"}", OpenMetadata.class));
        assertInstanceOf(org.openapitools.client.model.File.class, mapper.readValue("{\".tag\":\"file\",\"name\":\"x\"}", OpenMetadata.class));
    }
}
