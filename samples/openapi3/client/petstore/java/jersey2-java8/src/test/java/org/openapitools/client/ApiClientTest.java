package org.openapitools.client;

import org.openapitools.client.model.*;
import org.openapitools.client.ApiClient;

import java.lang.Exception;
import java.util.Arrays;
import java.util.Date;
import java.util.TimeZone;
import java.text.SimpleDateFormat;
import java.time.OffsetDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import org.junit.*;
import static org.junit.Assert.*;

public class ApiClientTest {
    ApiClient apiClient = null;
    Pet pet = null;

    @Before
    public void setup() {
        apiClient = new ApiClient();
        pet = new Pet();
    }

    @Test
    public void testSerializeToString() throws Exception {
        Long petId = 4321L;
        pet.setId(petId);
        pet.setName("jersey2 java8 pet");
        Category category = new Category();
        category.setId(petId);
        category.setName("jersey2 java8 category");
        pet.setCategory(category);
        pet.setStatus(Pet.StatusEnum.AVAILABLE);
        pet.setPhotoUrls(Arrays.asList("A", "B", "C"));
        Tag tag = new Tag();
        tag.setId(petId);
        tag.setName("jersey2 java8 tag");
        pet.setTags(Arrays.asList(tag));

        String result = "{\"id\":4321,\"category\":{\"id\":4321,\"name\":\"jersey2 java8 category\"},\"name\":\"jersey2 java8 pet\",\"photoUrls\":[\"A\",\"B\",\"C\"],\"tags\":[{\"id\":4321,\"name\":\"jersey2 java8 tag\"}],\"status\":\"available\"}";
        assertEquals(result, apiClient.serializeToString(pet, null, "application/json", false));
        // nulllable and there should be no diffencne as the payload is not null
        assertEquals(result, apiClient.serializeToString(pet, null, "application/json", true));

        // non-nullable null object should give "" (empty body)
        assertEquals("", apiClient.serializeToString(null, null, "application/json", false));
        // nullable null object should give "null"
        assertEquals("null", apiClient.serializeToString(null, null, "application/json", true));

        // non-nullable empty string should give "\"\"" (empty json string)
        assertEquals("\"\"", apiClient.serializeToString("", null, "application/json", false));
        // nullable null empty string should give "\"\"" (empty json string)
        assertEquals("\"\"", apiClient.serializeToString("", null, "application/json", true));

        // non-nullable string "null"  should give "\"null\""
        assertEquals("\"null\"", apiClient.serializeToString("null", null, "application/json", false));
        // nullable string "string" should give "\"null\""
        assertEquals("\"null\"", apiClient.serializeToString("null", null, "application/json", true));
    }

}
