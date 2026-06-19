package org.openapitools.server.api;

import java.io.File;
import io.helidon.webserver.Handler;
import java.util.List;
import org.openapitools.server.model.ModelApiResponse;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.openapitools.server.model.Pet;
import java.util.Set;

import io.helidon.webserver.Routing;
import io.helidon.webserver.ServerRequest;
import io.helidon.webserver.ServerResponse;
import io.helidon.webserver.Service;

public interface PetService extends Service { 

    /**
     * A service registers itself by updating the routing rules.
     * @param rules the routing rules.
     */
    @Override
    default void update(Routing.Rules rules) {
        rules.post("/pet", Handler.create(Pet.class, this::addPet));
        rules.delete("/pet/{petId}", this::deletePet);
        rules.get("/pet/findByStatus", this::findPetsByStatus);
        rules.get("/pet/findByTags", this::findPetsByTags);
        rules.get("/pet/{petId}", this::getPetById);
        rules.put("/pet", Handler.create(Pet.class, this::updatePet));
        rules.post("/pet/{petId}", this::updatePetWithForm);
        rules.post("/pet/{petId}/uploadImage", this::uploadFile);
        rules.post("/fake/{petId}/uploadImageWithRequiredFile", this::uploadFileWithRequiredFile);
    }


    /**
     * POST /pet : Add a new pet to the store.
     * @param request the server request
     * @param response the server response
     * @param pet Pet object that needs to be added to the store 
     */
    void addPet(ServerRequest request, ServerResponse response, Pet pet);

    /**
     * DELETE /pet/{petId} : Deletes a pet.
     * @param request the server request
     * @param response the server response
     */
    void deletePet(ServerRequest request, ServerResponse response);

    /**
     * GET /pet/findByStatus : Finds Pets by status.
     * @param request the server request
     * @param response the server response
     */
    void findPetsByStatus(ServerRequest request, ServerResponse response);

    /**
     * GET /pet/findByTags : Finds Pets by tags.
     * @param request the server request
     * @param response the server response
     */
    void findPetsByTags(ServerRequest request, ServerResponse response);

    /**
     * GET /pet/{petId} : Find pet by ID.
     * @param request the server request
     * @param response the server response
     */
    void getPetById(ServerRequest request, ServerResponse response);

    /**
     * PUT /pet : Update an existing pet.
     * @param request the server request
     * @param response the server response
     * @param pet Pet object that needs to be added to the store 
     */
    void updatePet(ServerRequest request, ServerResponse response, Pet pet);

    /**
     * POST /pet/{petId} : Updates a pet in the store with form data.
     * @param request the server request
     * @param response the server response
     */
    void updatePetWithForm(ServerRequest request, ServerResponse response);

    /**
     * POST /pet/{petId}/uploadImage : uploads an image.
     * @param request the server request
     * @param response the server response
     */
    void uploadFile(ServerRequest request, ServerResponse response);

    /**
     * POST /fake/{petId}/uploadImageWithRequiredFile : uploads an image (required).
     * @param request the server request
     * @param response the server response
     */
    void uploadFileWithRequiredFile(ServerRequest request, ServerResponse response);

}
