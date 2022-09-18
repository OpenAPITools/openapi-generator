package test_default_unimplemented_server_stubs;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.openapi.example.api.PetApi;
import org.openapi.example.model.Pet;
import org.springframework.web.reactive.function.client.WebClientResponseException;


import static helper.ApiClientFactories.setUpPetApi;
import static helper.TestingHelper.approveException;
import static org.junit.jupiter.api.Assertions.assertThrows;

class PetApiTest {

    PetApi apiInstance;

    @BeforeEach
    void setUp() {
        apiInstance = setUpPetApi(1234);
    }

    @Test
    void addPet() {
        Pet pet = new Pet();
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.addPet(pet).block();
                });

        approveException(exception);
    }

    @Test
    void deletePet() {
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.deletePet(99L, "myApiKey").block();
                });

        approveException(exception);
    }

    @Test
    void getPetById() {
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.getPetById(9L).block();
                });

        approveException(exception);
    }

    @Test
    void updatePet() {
        Pet pet = new Pet();
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.updatePet(pet).block();
                });

        approveException(exception);
    }

}