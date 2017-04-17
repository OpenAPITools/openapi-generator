package io.swagger.api;

import io.swagger.model.ModelApiResponse;
import io.swagger.model.Pet;
import org.springframework.core.io.Resource;

import io.swagger.annotations.*;
import org.springframework.http.HttpStatus;
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
    default ResponseEntity<Void> addPet(Pet body) {
    // do some magic!
    return new ResponseEntity<Void>(HttpStatus.OK);
    }

    /**
     * @see PetApi#deletePet
     */
    default ResponseEntity<Void> deletePet(Long petId,
        String apiKey) {
    // do some magic!
    return new ResponseEntity<Void>(HttpStatus.OK);
    }

    /**
     * @see PetApi#findPetsByStatus
     */
    default ResponseEntity<List<Pet>> findPetsByStatus(List<String> status) {
    // do some magic!
    return new ResponseEntity<List<Pet>>(HttpStatus.OK);
    }

    /**
     * @see PetApi#findPetsByTags
     */
    default ResponseEntity<List<Pet>> findPetsByTags(List<String> tags) {
    // do some magic!
    return new ResponseEntity<List<Pet>>(HttpStatus.OK);
    }

    /**
     * @see PetApi#getPetById
     */
    default ResponseEntity<Pet> getPetById(Long petId) {
    // do some magic!
    return new ResponseEntity<Pet>(HttpStatus.OK);
    }

    /**
     * @see PetApi#updatePet
     */
    default ResponseEntity<Void> updatePet(Pet body) {
    // do some magic!
    return new ResponseEntity<Void>(HttpStatus.OK);
    }

    /**
     * @see PetApi#updatePetWithForm
     */
    default ResponseEntity<Void> updatePetWithForm(Long petId,
        String name,
        String status) {
    // do some magic!
    return new ResponseEntity<Void>(HttpStatus.OK);
    }

    /**
     * @see PetApi#uploadFile
     */
    default ResponseEntity<ModelApiResponse> uploadFile(Long petId,
        String additionalMetadata,
        MultipartFile file) {
    // do some magic!
    return new ResponseEntity<ModelApiResponse>(HttpStatus.OK);
    }

}
