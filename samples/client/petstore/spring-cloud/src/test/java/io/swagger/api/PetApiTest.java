package io.swagger.api;

import feign.FeignException;
import io.swagger.TestUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import io.swagger.model.Category;
import io.swagger.model.Pet;
import io.swagger.model.Tag;
import org.junit.*;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.builder.SpringApplicationBuilder;
import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.cloud.netflix.feign.EnableFeignClients;
import org.springframework.http.ResponseEntity;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import static org.junit.Assert.*;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(classes = PetApiTest.Application.class)
public class PetApiTest {

    @Autowired
    private PetApiClient client;

    @Test
    public void testCreateAndGetPet() {
        Pet pet = createRandomPet();
        client.addPet(pet);
        ResponseEntity<Pet> rp = client.getPetById(pet.getId());
        Pet fetched =  rp.getBody();
        assertNotNull(fetched);
        assertEquals(pet.getId(), fetched.getId());
        assertNotNull(fetched.getCategory());
        assertEquals(fetched.getCategory().getName(), pet.getCategory().getName());
    }

    @Test
    public void testUpdatePet() throws Exception {
        Pet pet = createRandomPet();
        pet.setName("programmer");

        client.updatePet(pet);

        Pet fetched = client.getPetById(pet.getId()).getBody();
        assertNotNull(fetched);
        assertEquals(pet.getId(), fetched.getId());
        assertNotNull(fetched.getCategory());
        assertEquals(fetched.getCategory().getName(), pet.getCategory().getName());
    }

    @Ignore
    @Test
    public void testFindPetsByStatus() throws Exception {
        Pet pet = createRandomPet();
        pet.setName("programmer");
        pet.setStatus(Pet.StatusEnum.AVAILABLE);

        client.updatePet(pet);

        List<Pet> pets = client.findPetsByStatus(Arrays.asList(new String[]{"available"})).getBody();
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

    @Ignore
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

        client.updatePet(pet);

        List<Pet> pets = client.findPetsByTags(Arrays.asList(new String[]{"friendly"})).getBody();
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
        client.addPet(pet);

        Pet fetched = client.getPetById(pet.getId()).getBody();

        client.updatePetWithForm(fetched.getId(), "furt", null);
        Pet updated = client.getPetById(fetched.getId()).getBody();

        assertEquals(updated.getName(), "furt");
    }

    @Test
    public void testDeletePet() throws Exception {
        Pet pet = createRandomPet();
        client.addPet(pet);

        Pet fetched = client.getPetById(pet.getId()).getBody();
        client.deletePet(fetched.getId(), null);

        try {
            client.getPetById(fetched.getId());
            fail("expected an error");
        } catch (FeignException e) {
            assertTrue(e.getMessage().startsWith("status 404 "));
        }
    }

    @Ignore("Multipart form is not supported by spring-cloud yet.")
    @Test
    public void testUploadFile() throws Exception {
        Pet pet = createRandomPet();
        client.addPet(pet);

        MockMultipartFile filePart = new MockMultipartFile("file", "bar".getBytes());
        client.uploadFile(pet.getId(), "a test file", filePart);
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


    @SpringBootApplication
    @EnableFeignClients
    protected static class Application {
        public static void main(String[] args) {
            new SpringApplicationBuilder(Application.class).run(args);
        }
    }






}
