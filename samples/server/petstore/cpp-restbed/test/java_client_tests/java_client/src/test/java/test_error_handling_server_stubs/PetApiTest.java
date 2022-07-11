package test_error_handling_server_stubs;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.openapi.example.api.PetApi;
import org.openapi.example.model.Pet;
import org.springframework.web.reactive.function.client.WebClientResponseException;

import static helper.ApiClientFactories.setUpPetApi;
import static helper.TestingHelper.*;
import static org.junit.jupiter.api.Assertions.*;

class PetApiTest {

    PetApi apiInstance;

    @BeforeEach
    void setUp() {
        apiInstance = setUpPetApi(1236);
    }

    @Test
    void addPetInvalidEnumValue() {
        Pet pet = new Pet();
        pet.name("PetWithInvalidStatus");
        pet.status(null); // not allowed
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.addPet(pet).block();
                });
        approveException(exception);
    }

    @Test
    void addPetThatThrowsApiException() {
        Pet pet = new Pet();
        pet.name("ThrowsApiException");
        pet.status(Pet.StatusEnum.SOLD);
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.addPet(pet).block();
                });
        approveException(exception);
    }

    @Test
    void addPetThatThrowsStdExceptionDerivedException() {
        Pet pet = new Pet();
        pet.name("ThrowsStdExceptionDerivedException");
        pet.status(Pet.StatusEnum.AVAILABLE);
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.addPet(pet).block();
                });
        approveException(exception);
    }

    @Test
    void addPetThatThrowsInt() {
        Pet pet = new Pet();
        pet.name("ThrowsInt");
        pet.status(Pet.StatusEnum.AVAILABLE);
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.addPet(pet).block();
                });

        approveException(exception);
    }

    @Test
    void addPetThatReturnsStatus200() {
        Pet pet = new Pet();
        pet.name("ReturnsStatus200");
        pet.status(Pet.StatusEnum.AVAILABLE);
        apiInstance.addPet(pet).block();
    }

    @Test
    void addPetThatReturnsStatus405() {
        Pet pet = new Pet();
        pet.name("ReturnsStatus405");
        pet.status(Pet.StatusEnum.AVAILABLE);

        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.addPet(pet).block();
                });

        approveException(exception);
    }

    @Test
    void deletePetThrowsApiException() {
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.deletePet(1342L, "ThrowsApiException").block();
                });
        approveException(exception);
    }

    @Test
    void deletePetThrowsStdExceptionDerivedException() {
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.deletePet(1342L, "ThrowsStdExceptionDerivedException").block();
                });
        approveException(exception);
    }

    @Test
    void deletePetThrowsInt() {
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.deletePet(1342L, "ThrowsInt").block();
                });
        approveException(exception);
    }

    @Test
    void deletePetReturnsStatus200() {
        apiInstance.deletePet(1342L, "ReturnsStatus200").block();
    }

    @Test
    void deletePetReturnsStatus400() {
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.deletePet(1342L, "ReturnsStatus400").block();
                });
        approveException(exception);
    }

    @Test
    void deletePetReturnsStatus300() {
        apiInstance.deletePet(1342L, "ReturnsStatus300").block();
    }

    @Test
    void getPetByIdThrowsApiException() {
        long id = errorRaisingStringToInt("ThrowsApiException");
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.getPetById(id).block();
                });
        approveException(exception);
    }

    @Test
    void getPetByIdThrowsStdExceptionDerivedException() {
        long id = errorRaisingStringToInt("ThrowsStdExceptionDerivedException");
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.getPetById(id).block();
                });
        approveException(exception);
    }

    @Test
    void getPetByIdThrowsInt() {
        long id = errorRaisingStringToInt("ThrowsInt");
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.getPetById(id).block();
                });
        approveException(exception);
    }

    @Test
    void getPetByIdReturnsStatus200() {
        long id = errorRaisingStringToInt("ReturnsStatus200");
       Pet pet = apiInstance.getPetById(id).block();
        approveResponseAsJson(pet);
    }

    @Test
    void getPetByIdReturnsStatus300() {
        long id = errorRaisingStringToInt("ReturnsStatus300");
       Pet pet = apiInstance.getPetById(id).block();
        assertNull(pet);
    }

    @Test
    void getPetByIdReturnsStatus400() {
        long id = errorRaisingStringToInt("ReturnsStatus400");
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.getPetById(id).block();
                });
        approveException(exception);
    }

    @Test
    void getPetByIdReturnsStatus404() {
        long id = errorRaisingStringToInt("ReturnsStatus404");
        WebClientResponseException exception = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.getPetById(id).block();
                });
        approveException(exception);
    }

    @Test
    void updatePetThrowsApiException() {
        Pet pet = new Pet();
        pet.name("ThrowsApiException");
        pet.status(Pet.StatusEnum.SOLD);
        WebClientResponseException exception  = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.updatePet(pet).block();
                });
        approveException(exception);
    }

    @Test
    void updatePetThrowsStdExceptionDerivedException() {
        Pet pet = new Pet();
        pet.name("ThrowsStdExceptionDerivedException");
        pet.status(Pet.StatusEnum.SOLD);
        WebClientResponseException exception  = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.updatePet(pet).block();
                });
        approveException(exception);
    }

    @Test
    void updatePetThrowsInt() {
        Pet pet = new Pet();
        pet.name("ThrowsInt");
        pet.status(Pet.StatusEnum.SOLD);
        WebClientResponseException exception  = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.updatePet(pet).block();
                });
        approveException(exception);
    }


    @Test
    void updatePetReturnsStatus400() {
        Pet pet = new Pet();
        pet.name("ReturnsStatus400");
        pet.status(Pet.StatusEnum.SOLD);
        WebClientResponseException exception  = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.updatePet(pet).block();
                });
        approveException(exception);
    }

    @Test
    void updatePetReturnsStatus405() {
        Pet pet = new Pet();
        pet.name("ReturnsStatus405");
        pet.status(Pet.StatusEnum.SOLD);
        WebClientResponseException exception  = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.updatePet(pet).block();
                });
        approveException(exception);
    }
    @Test
    void updatePetReturnsStatus500() {
        Pet pet = new Pet();
        pet.name("ReturnsStatus500");
        pet.status(Pet.StatusEnum.SOLD);
        WebClientResponseException exception  = assertThrows(
                WebClientResponseException.class, () -> {
                    apiInstance.updatePet(pet).block();
                });
        approveException(exception);
    }
}
