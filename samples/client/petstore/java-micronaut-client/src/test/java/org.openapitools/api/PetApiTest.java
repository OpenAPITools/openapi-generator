package org.openapitools.api;

import java.io.File;
import org.openapitools.model.ModelApiResponse;
import org.openapitools.model.Pet;
import java.util.Set;
import io.micronaut.test.extensions.junit5.annotation.MicronautTest;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;
import jakarta.inject.Inject;
import reactor.core.publisher.Mono;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


/**
 * API tests for PetApi
 */
@MicronautTest
public class PetApiTest {

    @Inject
    PetApi api;

    
    /**
     * Add a new pet to the store
     */
    @Test
    public void addPetTest() {
        Pet _body = null;
        // api.addPet(_body).block();
        // Mono<Void> asyncResponse = api.addPet(_body);
        // TODO: test validations
    }

    
    /**
     * Deletes a pet
     */
    @Test
    public void deletePetTest() {
        Long petId = null;
        String apiKey = null;
        // api.deletePet(petId, apiKey).block();
        // Mono<Void> asyncResponse = api.deletePet(petId, apiKey);
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
        // List<Pet> response = api.findPetsByStatus(status).block();
        // Mono<List<Pet>> asyncResponse = api.findPetsByStatus(status);
        // TODO: test validations
    }

    
    /**
     * Finds Pets by tags
     *
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     */
    @Test
    public void findPetsByTagsTest() {
        Set<String> tags = null;
        // Set<Pet> response = api.findPetsByTags(tags).block();
        // Mono<Set<Pet>> asyncResponse = api.findPetsByTags(tags);
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
        // Pet response = api.getPetById(petId).block();
        // Mono<Pet> asyncResponse = api.getPetById(petId);
        // TODO: test validations
    }

    
    /**
     * Update an existing pet
     */
    @Test
    public void updatePetTest() {
        Pet _body = null;
        // api.updatePet(_body).block();
        // Mono<Void> asyncResponse = api.updatePet(_body);
        // TODO: test validations
    }

    
    /**
     * Updates a pet in the store with form data
     */
    @Test
    public void updatePetWithFormTest() {
        Long petId = null;
        String name = null;
        String status = null;
        // api.updatePetWithForm(petId, name, status).block();
        // Mono<Void> asyncResponse = api.updatePetWithForm(petId, name, status);
        // TODO: test validations
    }

    
    /**
     * uploads an image
     */
    @Test
    public void uploadFileTest() {
        Long petId = null;
        String additionalMetadata = null;
        File file = null;
        // ModelApiResponse response = api.uploadFile(petId, additionalMetadata, file).block();
        // Mono<ModelApiResponse> asyncResponse = api.uploadFile(petId, additionalMetadata, file);
        // TODO: test validations
    }

    
    /**
     * uploads an image (required)
     */
    @Test
    public void uploadFileWithRequiredFileTest() {
        Long petId = null;
        File requiredFile = null;
        String additionalMetadata = null;
        // ModelApiResponse response = api.uploadFileWithRequiredFile(petId, requiredFile, additionalMetadata).block();
        // Mono<ModelApiResponse> asyncResponse = api.uploadFileWithRequiredFile(petId, requiredFile, additionalMetadata);
        // TODO: test validations
    }

    
}
