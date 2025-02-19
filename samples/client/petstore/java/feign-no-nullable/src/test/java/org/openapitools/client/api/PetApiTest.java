package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import java.io.File;
import org.openapitools.client.model.ModelApiResponse;
import org.openapitools.client.model.Pet;
import java.util.Set;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * API tests for PetApi
 */
class PetApiTest {

    private PetApi api;

    @BeforeEach
    public void setup() {
        api = new ApiClient().buildClient(PetApi.class);
    }

    
    /**
     * Add a new pet to the store
     *
     * 
     */
    @Test
    void addPetTest() {
        Pet body = null;
        // api.addPet(body);

        // TODO: test validations
    }

    
    /**
     * Deletes a pet
     *
     * 
     */
    @Test
    void deletePetTest() {
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
    void findPetsByStatusTest() {
        List<String> status = null;
        // List<Pet> response = api.findPetsByStatus(status);

        // TODO: test validations
    }

    /**
     * Finds Pets by status
     *
     * Multiple status values can be provided with comma separated strings
     *
     * This tests the overload of the method that uses a Map for query parameters instead of
     * listing them out individually.
     */
    @Test
    void findPetsByStatusTestQueryMap() {
        PetApi.FindPetsByStatusQueryParams queryParams = new PetApi.FindPetsByStatusQueryParams()
            .status(null);
        // List<Pet> response = api.findPetsByStatus(queryParams);

    // TODO: test validations
    }
    
    /**
     * Finds Pets by tags
     *
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     */
    @Test
    void findPetsByTagsTest() {
        Set<String> tags = null;
        // Set<Pet> response = api.findPetsByTags(tags);

        // TODO: test validations
    }

    /**
     * Finds Pets by tags
     *
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     *
     * This tests the overload of the method that uses a Map for query parameters instead of
     * listing them out individually.
     */
    @Test
    void findPetsByTagsTestQueryMap() {
        PetApi.FindPetsByTagsQueryParams queryParams = new PetApi.FindPetsByTagsQueryParams()
            .tags(null);
        // Set<Pet> response = api.findPetsByTags(queryParams);

    // TODO: test validations
    }
    
    /**
     * Find pet by ID
     *
     * Returns a single pet
     */
    @Test
    void getPetByIdTest() {
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
    void updatePetTest() {
        Pet body = null;
        // api.updatePet(body);

        // TODO: test validations
    }

    
    /**
     * Updates a pet in the store with form data
     *
     * 
     */
    @Test
    void updatePetWithFormTest() {
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
    void uploadFileTest() {
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
    void uploadFileWithRequiredFileTest() {
        Long petId = null;
        File requiredFile = null;
        String additionalMetadata = null;
        // ModelApiResponse response = api.uploadFileWithRequiredFile(petId, requiredFile, additionalMetadata);

        // TODO: test validations
    }

    
}
