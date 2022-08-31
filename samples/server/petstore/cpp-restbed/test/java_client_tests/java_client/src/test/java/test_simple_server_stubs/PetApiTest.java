package test_simple_server_stubs;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.openapi.example.api.PetApi;
import org.openapi.example.model.Category;
import org.openapi.example.model.Pet;
import org.openapi.example.model.Tag;
import java.util.List;

import static helper.ApiClientFactories.setUpPetApi;
import static helper.TestingHelper.approveResponseAsJson;

class PetApiTest {

    PetApi apiInstance;

    @BeforeEach
    void setUp() {
        apiInstance = setUpPetApi(1235);
    }

    @Test
    void addPet() {
        Pet pet = createTestPet();
        Pet responsePet = apiInstance.addPet(pet).block();
        approveResponseAsJson(responsePet);
    }

    @Test
    void deletePet() {
        apiInstance.deletePet(1L, "myApiKey").block();
    }

    @Test
    void getPetById() {
        Pet responsePet = apiInstance.getPetById(99L).block();
        approveResponseAsJson(responsePet);
    }

    @Test
    void updatePet() {
        Pet pet = createTestPet();
        apiInstance.updatePet(pet).block();
    }

    private Pet createTestPet() {
        Pet pet = new Pet();

        pet.setId(12L);
        pet.setName("MyPet");
        pet.setStatus(Pet.StatusEnum.SOLD);

        Tag tag = new Tag();
        tag.name("MyTag");
        pet.addTagsItem(tag);

        Category category = new Category();
        category.setId(1L);
        category.setName("MyCategory");
        pet.setCategory(category);

        pet.addPhotoUrlsItem("myUrl");

        return pet;
    }
}