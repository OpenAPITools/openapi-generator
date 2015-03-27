package io.swagger.petstore.test;

import io.swagger.client.api.*;
import io.swagger.client.model.*;

import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.*;
import org.junit.*;

public class PetApiTest {
  PetApi api = null;

  @Before
  public void setup() {
    api = new PetApi();
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
  public void testFindPetByStatus() throws Exception {
    Pet pet = createRandomPet();
    pet.setName("programmer");
    pet.setStatus(Pet.StatusEnum.available);

    api.updatePet(pet);

    List<Pet> pets = api.findPetsByStatus(Arrays.asList(new String[]{"available"}));
    assertNotNull(pets);

    boolean found = false;
    for(Pet fetched : pets) {
      if(fetched.getId().equals(pet.getId())) {
        found = true;
        break;
      }
    }

    assertTrue(found);
  }

  private Pet createRandomPet() {
    Pet pet = new Pet();
    pet.setId(System.currentTimeMillis());
    pet.setName("gorilla");

    Category category = new Category();
    category.setName("really-happy");

    pet.setCategory(category);
    pet.setStatus(Pet.StatusEnum.available);
    List<String> photos = Arrays.asList(new String[]{"http://foo.bar.com/1", "http://foo.bar.com/2"});
    pet.setPhotoUrls(photos);

    return pet;
  }
}