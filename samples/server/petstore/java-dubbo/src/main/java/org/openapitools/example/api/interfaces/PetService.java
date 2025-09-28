package org.openapitools.example.api.interfaces;

import org.openapitools.example.model.ModelApiResponse;
import org.openapitools.example.model.Pet;
import org.openapitools.example.model.*;
import java.util.List;
import java.util.Map;
import java.time.OffsetDateTime;
import java.time.LocalDate;
import java.time.LocalDateTime;
import javax.annotation.Generated;


@Generated(value = "org.openapitools.codegen.languages.JavaDubboServerCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")

public interface PetService {

    /**
     * Add a new pet to the store
     * 
     *
     * @param pet Pet object that needs to be added to the store (required)
     * @return Pet
     */
    Pet addPet(
        Pet pet
    );

    /**
     * Deletes a pet
     * 
     *
     * @param petId Pet id to delete (required)
     * @param apiKey  (optional)
     * @return void
     */
    void deletePet(
        Long petId,
        String apiKey
    );

    /**
     * Finds Pets by status
     * Multiple status values can be provided with comma separated strings
     *
     * @param status Status values that need to be considered for filter (required)
     * @return List<Pet>
     */
    List<Pet> findPetsByStatus(
        List<String> status
    );

    /**
     * Finds Pets by tags
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     *
     * @param tags Tags to filter by (required)
     * @return List<Pet>
     */
    List<Pet> findPetsByTags(
        List<String> tags
    );

    /**
     * Find pet by ID
     * Returns a single pet
     *
     * @param petId ID of pet to return (required)
     * @return Pet
     */
    Pet getPetById(
        Long petId
    );

    /**
     * Update an existing pet
     * 
     *
     * @param pet Pet object that needs to be added to the store (required)
     * @return Pet
     */
    Pet updatePet(
        Pet pet
    );

    /**
     * Updates a pet in the store with form data
     * 
     *
     * @param petId ID of pet that needs to be updated (required)
     * @param name Updated name of the pet (optional)
     * @param status Updated status of the pet (optional)
     * @return void
     */
    void updatePetWithForm(
        Long petId,
        String name,
        String status
    );

    /**
     * uploads an image
     * 
     *
     * @param petId ID of pet to update (required)
     * @param additionalMetadata Additional data to pass to server (optional)
     * @param _file file to upload (optional)
     * @return ModelApiResponse
     */
    ModelApiResponse uploadFile(
        Long petId,
        String additionalMetadata,
        org.springframework.web.multipart.MultipartFile _file
    );
}
