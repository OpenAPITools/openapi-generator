package org.openapitools.client.api;

import org.junit.Before;
import org.junit.Test;
import org.openapitools.client.ApiClient;
import org.openapitools.client.model.Pet;

import java.io.File;
import java.util.List;

/**
 * API tests for PetApi
 */
public class PetApiTest {

    private PetApi api;

    @Before
    public void setup() {
        api = new ApiClient().createService(PetApi.class);
    }

    /**
     * Add a new pet to the store
     *
     *
     */
    @Test
    public void addPetTest() {
        Pet pet = null;
        // api.addPet(pet);

        // TODO: test validations
    }
    /**
     * Deletes a pet
     *
     *
     */
    @Test
    public void deletePetTest() {
        Long petId = null;
        String apiKey = null;
        // api.deletePet(petId, apiKey);

        // TODO: test validations
    }
    /**
     * Finds Pets by status
     *
     * Multiple status values can be provided with comma separated strings
     */
    @Test
    public void findPetsByStatusTest() {
        List<String> status = null;
        // List<Pet> response = api.findPetsByStatus(status);

        // TODO: test validations
    }
    /**
     * Finds Pets by tags
     *
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     */
    @Test
    public void findPetsByTagsTest() {
        List<String> tags = null;
        // List<Pet> response = api.findPetsByTags(tags);

        // TODO: test validations
    }
    /**
     * Find pet by ID
     *
     * Returns a single pet
     */
    @Test
    public void getPetByIdTest() {
        Long petId = null;
        // Pet response = api.getPetById(petId);

        // TODO: test validations
    }
    /**
     * Update an existing pet
     *
     *
     */
    @Test
    public void updatePetTest() {
        Pet pet = null;
        // api.updatePet(pet);

        // TODO: test validations
    }
    /**
     * Updates a pet in the store with form data
     *
     *
     */
    @Test
    public void updatePetWithFormTest() {
        Long petId = null;
        String name = null;
        String status = null;
        // api.updatePetWithForm(petId, name, status);

        // TODO: test validations
    }
    /**
     * uploads an image
     *
     *
     */
    @Test
    public void uploadFileTest() {
        Long petId = null;
        String additionalMetadata = null;
        File file = null;
        // ModelApiResponse response = api.uploadFile(petId, additionalMetadata, file);

        // TODO: test validations
    }
    /**
     * uploads an image (required)
     *
     *
     */
    @Test
    public void uploadFileWithRequiredFileTest() {
        Long petId = null;
        File requiredFile = null;
        String additionalMetadata = null;
        // ModelApiResponse response = api.uploadFileWithRequiredFile(petId, requiredFile, additionalMetadata);

        // TODO: test validations
    }
}
