package io.swagger.petstore.test;

import com.google.gson.reflect.TypeToken;

import io.swagger.TestUtils;

import io.swagger.client.*;
import io.swagger.client.api.*;
import io.swagger.client.auth.*;
import io.swagger.client.model.*;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.*;
import static org.junit.Assert.*;

public class PetApiTest {
    PetApi api = null;

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
        assertEquals(Configuration.getDefaultApiClient(), api.getApiClient());
        assertNotNull(api.getApiClient());
        assertEquals("http://petstore.swagger.io/v2", api.getApiClient().getBasePath());
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
        assertEquals("http://petstore.swagger.io/v2", api.getApiClient().getBasePath());
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

    @Test
    public void testCreateAndGetPetWithByteArray() throws Exception {
        Pet pet = createRandomPet();
        System.out.println(serializeJson(pet, api.getApiClient()));
        byte[] bytes = serializeJson(pet, api.getApiClient()).getBytes();
        api.addPetUsingByteArray(bytes);

        byte[] fetchedBytes = api.getPetByIdWithByteArray(pet.getId());
        System.out.println(new String(fetchedBytes));
        Type type = new TypeToken<Pet>(){}.getType();
        Pet fetched = deserializeJson(new String(fetchedBytes), type, api.getApiClient());
        assertNotNull(fetched);
        assertEquals(pet.getId(), fetched.getId());
        assertNotNull(fetched.getCategory());
        assertEquals(fetched.getCategory().getName(), pet.getCategory().getName());
    }

    @Test
    public void testCreateAndGetPetWithHttpInfo() throws Exception {
        Pet pet = createRandomPet();
        api.addPetWithHttpInfo(pet);

        ApiResponse<Pet> resp = api.getPetByIdWithHttpInfo(pet.getId());
        assertEquals(200, resp.getStatusCode());
        assertEquals("application/json", resp.getHeaders().get("Content-Type").get(0));
        Pet fetched = resp.getData();
        assertNotNull(fetched);
        assertEquals(pet.getId(), fetched.getId());
        assertNotNull(fetched.getCategory());
        assertEquals(fetched.getCategory().getName(), pet.getCategory().getName());
    }

    @Test
    public void testCreateAndGetPetAsync() throws Exception {
        Pet pet = createRandomPet();
        api.addPet(pet);
        // to store returned Pet or error message/exception
        final Map<String, Object> result = new HashMap<String, Object>();

        api.getPetByIdAsync(pet.getId(), new ApiCallback<Pet>() {
            @Override
            public void onFailure(ApiException e, int statusCode, Map<String, List<String>> responseHeaders) {
                result.put("error", e.getMessage());
            }

            @Override
            public void onSuccess(Pet pet, int statusCode, Map<String, List<String>> responseHeaders) {
                result.put("pet", pet);
            }

            @Override
            public void onUploadProgress(long bytesWritten, long contentLength, boolean done) {
                //empty
            }

            @Override
            public void onDownloadProgress(long bytesRead, long contentLength, boolean done) {
                //empty
            }
        });
        // the API call should be executed asynchronously, so result should be empty at the moment
        assertTrue(result.isEmpty());

        // wait for the asynchronous call to finish (at most 10 seconds)
        final int maxTry = 10;
        int tryCount = 1;
        Pet fetched = null;
        do {
            if (tryCount > maxTry) fail("have not got result of getPetByIdAsync after 10 seconds");
            Thread.sleep(1000);
            tryCount += 1;
            if (result.get("error") != null) fail((String) result.get("error"));
            if (result.get("pet") != null) {
                fetched = (Pet) result.get("pet");
                break;
            }
        } while (result.isEmpty());
        assertNotNull(fetched);
        assertEquals(pet.getId(), fetched.getId());
        assertNotNull(fetched.getCategory());
        assertEquals(fetched.getCategory().getName(), pet.getCategory().getName());

        // test getting a nonexistent pet
        result.clear();
        api.getPetByIdAsync(new Long(-10000), new ApiCallback<Pet>() {
            @Override
            public void onFailure(ApiException e, int statusCode, Map<String, List<String>> responseHeaders) {
                result.put("exception", e);
            }

            @Override
            public void onSuccess(Pet pet, int statusCode, Map<String, List<String>> responseHeaders) {
                result.put("pet", pet);
            }

            @Override
            public void onUploadProgress(long bytesWritten, long contentLength, boolean done) {
                //empty
            }

            @Override
            public void onDownloadProgress(long bytesRead, long contentLength, boolean done) {
                //empty
            }
        });
        // the API call should be executed asynchronously, so result should be empty at the moment
        assertTrue(result.isEmpty());

        // wait for the asynchronous call to finish (at most 10 seconds)
        tryCount = 1;
        ApiException exception = null;
        do {
            if (tryCount > maxTry) fail("have not got result of getPetByIdAsync after 10 seconds");
            Thread.sleep(1000);
            tryCount += 1;
            if (result.get("pet") != null) fail("expected an error");
            if (result.get("exception") != null) {
                exception = (ApiException) result.get("exception");
                break;
            }
        } while (result.isEmpty());
        assertNotNull(exception);
        assertEquals(404, exception.getCode());
        assertEquals("Not Found", exception.getMessage());
        assertEquals("application/json", exception.getResponseHeaders().get("Content-Type").get(0));
    }

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

    @Test
    public void testFindPetsByTags() throws Exception {
        Pet pet = createRandomPet();
        pet.setName("monster");
        pet.setStatus(Pet.StatusEnum.AVAILABLE);

        List<Tag> tags = new ArrayList<Tag>();
        Tag tag1 = new Tag();
        tag1.setName("friendly");
        tags.add(tag1);
        pet.setTags(tags);

        api.updatePet(pet);

        List<Pet> pets = api.findPetsByTags(Arrays.asList(new String[]{"friendly"}));
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

    @Test
    public void testUpdatePetWithForm() throws Exception {
        Pet pet = createRandomPet();
        pet.setName("frank");
        api.addPet(pet);

        Pet fetched = api.getPetById(pet.getId());

        api.updatePetWithForm(String.valueOf(fetched.getId()), "furt", null);
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
        } catch (ApiException e) {
            assertEquals(404, e.getCode());
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

    private String serializeJson(Object o, ApiClient apiClient) {
        return apiClient.getJSON().serialize(o);
    }

    private <T> T deserializeJson(String json, Type type, ApiClient apiClient) {
        return (T) apiClient.getJSON().deserialize(json, type);
    }
}
