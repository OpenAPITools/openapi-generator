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
     * @see PetApi#addPet
     */
    ResponseEntity<Void> addPet(Pet body);

    /**
     * @see PetApi#deletePet
     */
    ResponseEntity<Void> deletePet(Long petId,
        String apiKey);

    /**
     * @see PetApi#findPetsByStatus
     */
    ResponseEntity<List<Pet>> findPetsByStatus(List<String> status);

    /**
     * @see PetApi#findPetsByTags
     */
    ResponseEntity<List<Pet>> findPetsByTags(List<String> tags);

    /**
     * @see PetApi#getPetById
     */
    ResponseEntity<Pet> getPetById(Long petId);

    /**
     * @see PetApi#updatePet
     */
    ResponseEntity<Void> updatePet(Pet body);

    /**
     * @see PetApi#updatePetWithForm
     */
    ResponseEntity<Void> updatePetWithForm(Long petId,
        String name,
        String status);

    /**
     * @see PetApi#uploadFile
     */
    ResponseEntity<ModelApiResponse> uploadFile(Long petId,
        String additionalMetadata,
        MultipartFile file);

}
