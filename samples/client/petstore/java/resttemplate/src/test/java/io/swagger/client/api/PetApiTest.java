package io.swagger.client.api;

import com.fasterxml.jackson.annotation.*;
import com.fasterxml.jackson.databind.*;

import com.fasterxml.jackson.datatype.threetenbp.ThreeTenModule;
import io.swagger.TestUtils;

import io.swagger.client.*;
import io.swagger.client.api.*;
import io.swagger.client.auth.*;
import io.swagger.client.model.*;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import org.junit.*;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestClientException;
import org.threeten.bp.Instant;
import org.threeten.bp.OffsetDateTime;
import org.threeten.bp.ZonedDateTime;

import static org.junit.Assert.*;

public class PetApiTest {
    private PetApi api;
    private ObjectMapper mapper;

    @Before
    public void setup() {
        api = new PetApi();
        // setup authentication
        ApiKeyAuth apiKeyAuth = (ApiKeyAuth) api.getApiClient().getAuthentication("api_key");
        apiKeyAuth.setApiKey("special-key");
    }

    @Test
    public void testApiClient() {
        // the default api client is used
        assertNotNull(api.getApiClient());
        assertEquals("http://petstore.swagger.io:80/v2", api.getApiClient().getBasePath());
        assertFalse(api.getApiClient().isDebugging());

        ApiClient oldClient = api.getApiClient();

        ApiClient newClient = new ApiClient();
        newClient.setBasePath("http://example.com");
        newClient.setDebugging(true);

        // set api client via constructor
        api = new PetApi(newClient);
        assertNotNull(api.getApiClient());
        assertEquals("http://example.com", api.getApiClient().getBasePath());
        assertTrue(api.getApiClient().isDebugging());

        // set api client via setter method
        api.setApiClient(oldClient);
        assertNotNull(api.getApiClient());
        assertEquals("http://petstore.swagger.io:80/v2", api.getApiClient().getBasePath());
        assertFalse(api.getApiClient().isDebugging());
    }

    @Test
    public void testCreateAndGetPet() throws Exception {
        Pet pet = createRandomPet();
        api.addPet(pet);

        Pet fetched = api.getPetById(pet.getId());
        assertNotNull(fetched);
        assertEquals(pet.getId(), fetched.getId());
        assertNotNull(fetched.getCategory());
        assertEquals(fetched.getCategory().getName(), pet.getCategory().getName());
    }

    /*
    @Test
    public void testCreateAndGetPetWithByteArray() throws Exception {
        Pet pet = createRandomPet();
        byte[] bytes = serializeJson(pet).getBytes();
        api.addPetUsingByteArray(bytes);

        byte[] fetchedBytes = api.petPetIdtestingByteArraytrueGet(pet.getId());
        Pet fetched = deserializeJson(new String(fetchedBytes), Pet.class);
        assertNotNull(fetched);
        assertEquals(pet.getId(), fetched.getId());
        assertNotNull(fetched.getCategory());
        assertEquals(fetched.getCategory().getName(), pet.getCategory().getName());
    }

    @Test
    public void testGetPetByIdInObject() throws Exception {
        Pet pet = new Pet();
        pet.setId(TestUtils.nextId());
        pet.setName("pet " + pet.getId());

        Category category = new Category();
        category.setId(TestUtils.nextId());
        category.setName("category " + category.getId());
        pet.setCategory(category);

        pet.setStatus(Pet.StatusEnum.PENDING);
        List<String> photos = Arrays.asList(new String[]{"http://foo.bar.com/1"});
        pet.setPhotoUrls(photos);

        api.addPet(pet);

        InlineResponse200 fetched = api.getPetByIdInObject(pet.getId());
        assertEquals(pet.getId(), fetched.getId());
        assertEquals(pet.getName(), fetched.getName());

        Object categoryObj = fetched.getCategory();
        assertNotNull(categoryObj);
        assertTrue(categoryObj instanceof Map);

        Map categoryMap = (Map) categoryObj;
        Object categoryIdObj = categoryMap.get("id");
        assertTrue(categoryIdObj instanceof Integer);
        Integer categoryIdInt = (Integer) categoryIdObj;
        assertEquals(category.getId(), Long.valueOf(categoryIdInt));
        assertEquals(category.getName(), categoryMap.get("name"));
    }
    */

    @Test
    public void testUpdatePet() throws Exception {
        Pet pet = createRandomPet();
        pet.setName("programmer");

        api.updatePet(pet);

        Pet fetched = api.getPetById(pet.getId());
        assertNotNull(fetched);
        assertEquals(pet.getId(), fetched.getId());
        assertNotNull(fetched.getCategory());
        assertEquals(fetched.getCategory().getName(), pet.getCategory().getName());
    }

    @Test
    public void testFindPetsByStatus() throws Exception {
        Pet pet = createRandomPet();
        pet.setName("programmer");
        pet.setStatus(Pet.StatusEnum.AVAILABLE);

        api.updatePet(pet);

        List<Pet> pets = api.findPetsByStatus(Arrays.asList(new String[]{"available"}));
        assertNotNull(pets);

        boolean found = false;
        for (Pet fetched : pets) {
            if (fetched.getId().equals(pet.getId())) {
                found = true;
                break;
            }
        }

        assertTrue(found);
    }

//    this API is deprecated
//    @Test
//    public void testFindPetsByTags() throws Exception {
//        Pet pet = createRandomPet();
//        pet.setName("monster");
//        pet.setStatus(Pet.StatusEnum.AVAILABLE);
//
//        List<Tag> tags = new ArrayList<Tag>();
//        Tag tag1 = new Tag();
//        tag1.setName("friendly");
//        tags.add(tag1);
//        pet.setTags(tags);
//
//        api.updatePet(pet);
//
//        List<Pet> pets = api.findPetsByTags(Arrays.asList(new String[]{"friendly"}));
//        assertNotNull(pets);
//
//        boolean found = false;
//        for (Pet fetched : pets) {
//            if (fetched.getId().equals(pet.getId())) {
//                found = true;
//                break;
//            }
//        }
//        assertTrue(found);
//    }

    @Test
    public void testUpdatePetWithForm() throws Exception {
        Pet pet = createRandomPet();
        pet.setName("frank");
        api.addPet(pet);

        Pet fetched = api.getPetById(pet.getId());

        api.updatePetWithForm(fetched.getId(), "furt", null);
        Pet updated = api.getPetById(fetched.getId());

        assertEquals(updated.getName(), "furt");
    }

    @Test
    public void testDeletePet() throws Exception {
        Pet pet = createRandomPet();
        api.addPet(pet);

        Pet fetched = api.getPetById(pet.getId());
        api.deletePet(fetched.getId(), null);

        try {
            fetched = api.getPetById(fetched.getId());
            fail("expected an error");
        } catch (RestClientException e) {
            assertTrue(e instanceof HttpClientErrorException);
            assertEquals(404, ((HttpClientErrorException) e).getStatusCode().value());
        }
    }

    @Test
    public void testUploadFile() throws Exception {
        Pet pet = createRandomPet();
        api.addPet(pet);

        File file = new File("hello.txt");
        BufferedWriter writer = new BufferedWriter(new FileWriter(file));
        writer.write("Hello world!");
        writer.close();

        api.uploadFile(pet.getId(), "a test file", new File(file.getAbsolutePath()));
    }

    @Test
    public void testEqualsAndHashCode() {
        Pet pet1 = new Pet();
        Pet pet2 = new Pet();
        assertTrue(pet1.equals(pet2));
        assertTrue(pet2.equals(pet1));
        assertTrue(pet1.hashCode() == pet2.hashCode());
        assertTrue(pet1.equals(pet1));
        assertTrue(pet1.hashCode() == pet1.hashCode());

        pet2.setName("really-happy");
        pet2.setPhotoUrls(Arrays.asList(new String[]{"http://foo.bar.com/1", "http://foo.bar.com/2"}));
        assertFalse(pet1.equals(pet2));
        assertFalse(pet2.equals(pet1));
        assertFalse(pet1.hashCode() == (pet2.hashCode()));
        assertTrue(pet2.equals(pet2));
        assertTrue(pet2.hashCode() == pet2.hashCode());

        pet1.setName("really-happy");
        pet1.setPhotoUrls(Arrays.asList(new String[]{"http://foo.bar.com/1", "http://foo.bar.com/2"}));
        assertTrue(pet1.equals(pet2));
        assertTrue(pet2.equals(pet1));
        assertTrue(pet1.hashCode() == pet2.hashCode());
        assertTrue(pet1.equals(pet1));
        assertTrue(pet1.hashCode() == pet1.hashCode());
    }

    private Pet createRandomPet() {
        Pet pet = new Pet();
        pet.setId(TestUtils.nextId());
        pet.setName("gorilla");

        Category category = new Category();
        category.setName("really-happy");

        pet.setCategory(category);
        pet.setStatus(Pet.StatusEnum.AVAILABLE);
        List<String> photos = Arrays.asList(new String[]{"http://foo.bar.com/1", "http://foo.bar.com/2"});
        pet.setPhotoUrls(photos);

        return pet;
    }

    private String serializeJson(Object o) {
        if (mapper == null) {
            mapper = createObjectMapper();
        }
        try {
            return mapper.writeValueAsString(o);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    private <T> T deserializeJson(String json, Class<T> klass) {
        if (mapper == null) {
            mapper = createObjectMapper();
        }
        try {
            return mapper.readValue(json, klass);
        } catch (Exception e) {
          throw new RuntimeException(e);
        }
    }

    private ObjectMapper createObjectMapper() {
        ObjectMapper mapper = new ObjectMapper();
        mapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
        mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        mapper.disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);
        mapper.enable(SerializationFeature.WRITE_ENUMS_USING_TO_STRING);
        mapper.enable(DeserializationFeature.READ_ENUMS_USING_TO_STRING);
        ThreeTenModule module = new ThreeTenModule();
        module.addDeserializer(Instant.class, CustomInstantDeserializer.INSTANT);
        module.addDeserializer(OffsetDateTime.class, CustomInstantDeserializer.OFFSET_DATE_TIME);
        module.addDeserializer(ZonedDateTime.class, CustomInstantDeserializer.ZONED_DATE_TIME);
        mapper.registerModule(module);
        return mapper;
    }
}
