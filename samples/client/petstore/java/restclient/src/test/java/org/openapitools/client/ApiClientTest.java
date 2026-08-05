package org.openapitools.client;

import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

class ApiClientTest {

    private final ApiClient apiClient = new ApiClient();

    @Test
    void selectHeaderContentTypePrefersJson() {
        assertEquals(MediaType.APPLICATION_JSON,
                apiClient.selectHeaderContentType(new String[]{"application/xml", "application/json"}));
    }

    @Test
    void selectHeaderContentTypeReturnsFirstConcreteTypeWhenNoJson() {
        assertEquals(MediaType.IMAGE_PNG,
                apiClient.selectHeaderContentType(new String[]{"image/png", "application/xml"}));
    }

    // https://github.com/OpenAPITools/openapi-generator/issues/24118
    // A wildcard media type must never be returned: it cannot be used as a request
    // Content-Type header (Spring throws "Content-Type cannot contain wildcard type '*'").

    @Test
    void selectHeaderContentTypeFallsBackToJsonForJsonCompatibleWildcard() {
        // "application/*" / "*/*" are reported JSON-compatible by isJsonMime, but are wildcards.
        for (String wildcard : new String[]{"application/*", "*/*"}) {
            MediaType selected = apiClient.selectHeaderContentType(new String[]{wildcard});
            assertEquals(MediaType.APPLICATION_JSON, selected);
            assertFalse(selected.isWildcardType());
            assertFalse(selected.isWildcardSubtype());
        }
    }

    @Test
    void selectHeaderContentTypeSkipsNonJsonWildcardForConcreteType() {
        MediaType selected = apiClient.selectHeaderContentType(new String[]{"image/*", "text/plain"});
        assertEquals(MediaType.TEXT_PLAIN, selected);
        assertFalse(selected.isWildcardType());
        assertFalse(selected.isWildcardSubtype());
    }
}
