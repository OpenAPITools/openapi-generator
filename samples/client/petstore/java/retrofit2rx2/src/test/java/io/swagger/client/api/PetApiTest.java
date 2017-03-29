package io.swagger.client.api;

import io.swagger.client.ApiClient;
import java.io.File;
import io.swagger.client.model.ModelApiResponse;
import io.swagger.client.model.Pet;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
        Pet body = null;
        // Void response = api.addPet(body);

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
        // Void response = api.deletePet(petId, apiKey);

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
        Pet body = null;
        // Void response = api.updatePet(body);

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
        // Void response = api.updatePetWithForm(petId, name, status);

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
    
}
