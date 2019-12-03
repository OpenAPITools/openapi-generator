package org.openapitools.api;

import org.openapitools.model.ModelApiResponse;
import org.openapitools.model.Pet;
import org.springframework.core.io.Resource;
import io.swagger.annotations.*;
import org.springframework.http.ResponseEntity;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;
import java.util.Map;

/**
 * A delegate to be called by the {@link PetApiController}}.
 * Implement this interface with a {@link org.springframework.stereotype.Service} annotated class.
 */

public interface PetApiDelegate {

    /**
     * POST /pet : Add a new pet to the store
     *
     * @param body Pet object that needs to be added to the store (required)
     * @return the response
     * @see PetApi#addPet
     */
    ResponseEntity<Void> addPet(Pet body);

    /**
     * DELETE /pet/{petId} : Deletes a pet
     *
     * @param petId Pet id to delete (required)
     * @param apiKey  (optional)
     * @return the response
     * @see PetApi#deletePet
     */
    ResponseEntity<Void> deletePet(Long petId,
        String apiKey);

    /**
     * GET /pet/findByStatus : Finds Pets by status
     * Multiple status values can be provided with comma separated strings
     *
     * @param status Status values that need to be considered for filter (required)
     * @return the response
     * @see PetApi#findPetsByStatus
     */
    ResponseEntity<List<Pet>> findPetsByStatus(List<String> status);

    /**
     * GET /pet/findByTags : Finds Pets by tags
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     *
     * @param tags Tags to filter by (required)
     * @return the response
     * @deprecated
     * @see PetApi#findPetsByTags
     */
    ResponseEntity<List<Pet>> findPetsByTags(List<String> tags);

    /**
     * GET /pet/{petId} : Find pet by ID
     * Returns a single pet
     *
     * @param petId ID of pet to return (required)
     * @return the response
     * @see PetApi#getPetById
     */
    ResponseEntity<Pet> getPetById(Long petId);

    /**
     * PUT /pet : Update an existing pet
     *
     * @param body Pet object that needs to be added to the store (required)
     * @return the response
     * @see PetApi#updatePet
     */
    ResponseEntity<Void> updatePet(Pet body);

    /**
     * POST /pet/{petId} : Updates a pet in the store with form data
     *
     * @param petId ID of pet that needs to be updated (required)
     * @param name Updated name of the pet (optional)
     * @param status Updated status of the pet (optional)
     * @return the response
     * @see PetApi#updatePetWithForm
     */
    ResponseEntity<Void> updatePetWithForm(Long petId,
        String name,
        String status);

    /**
     * POST /pet/{petId}/uploadImage : uploads an image
     *
     * @param petId ID of pet to update (required)
     * @param additionalMetadata Additional data to pass to server (optional)
     * @param file file to upload (optional)
     * @return the response
     * @see PetApi#uploadFile
     */
    ResponseEntity<ModelApiResponse> uploadFile(Long petId,
        String additionalMetadata,
        MultipartFile file);

}
