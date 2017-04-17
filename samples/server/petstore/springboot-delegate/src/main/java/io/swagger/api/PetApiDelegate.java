package io.swagger.api;

import io.swagger.model.ModelApiResponse;
import io.swagger.model.Pet;
import org.springframework.core.io.Resource;

import io.swagger.annotations.*;
import org.springframework.http.ResponseEntity;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

/**
 * A delegate to be called by the {@link PetApiController}}.
 * Should be implemented as a controller but without the {@link org.springframework.stereotype.Controller} annotation.
 * Instead, use spring to autowire this class into the {@link PetApiController}.
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
