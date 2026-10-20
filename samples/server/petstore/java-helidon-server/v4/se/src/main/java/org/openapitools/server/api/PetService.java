package org.openapitools.server.api;

import java.util.stream.Collectors;
import java.io.File;
import io.helidon.http.HeaderNames;
import io.helidon.http.Headers;
import java.util.HexFormat;
import java.util.List;
import java.util.Map;
import org.openapitools.server.model.ModelApiResponse;
import io.helidon.http.media.multipart.MultiPart;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.Optional;
import io.helidon.common.parameters.Parameters;
import org.openapitools.server.model.Pet;
import io.helidon.http.media.multipart.ReadablePart;
import java.util.Set;
import io.helidon.http.Status;
import io.helidon.common.mapper.Value;

import io.helidon.webserver.http.HttpRules;
import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;
import io.helidon.webserver.http.HttpService;

@io.helidon.common.Generated(value = "org.openapitools.codegen.languages.JavaHelidonServerCodegen",
                             trigger = "tag = 'Pet'",
                             version = "stable")
public interface PetService extends HttpService {

    /**
     * A service registers itself by updating the routing rules.
     * @param rules the routing rules.
     */
    @Override
    default void routing(HttpRules rules) {
        rules.post("/pet", this::addPet);
        rules.delete("/pet/{petId}", this::deletePet);
        rules.get("/pet/findByStatus", this::findPetsByStatus);
        rules.get("/pet/findByTags", this::findPetsByTags);
        rules.get("/pet/{petId}", this::getPetById);
        rules.put("/pet", this::updatePet);
        rules.post("/pet/{petId}", this::updatePetWithForm);
        rules.post("/pet/{petId}/uploadImage", this::uploadFile);
        rules.post("/fake/{petId}/uploadImageWithRequiredFile", this::uploadFileWithRequiredFile);
    }


    /**
     * POST /pet : Add a new pet to the store.
     *
     * @param request the server request
     * @param response the server response
     */
    void addPet(ServerRequest request, ServerResponse response);
    /**
     * DELETE /pet/{petId} : Deletes a pet.
     *
     * @param request the server request
     * @param response the server response
     */
    void deletePet(ServerRequest request, ServerResponse response);
    /**
     * GET /pet/findByStatus : Finds Pets by status.
     *
     * @param request the server request
     * @param response the server response
     */
    void findPetsByStatus(ServerRequest request, ServerResponse response);
    /**
     * GET /pet/findByTags : Finds Pets by tags.
     *
     * @param request the server request
     * @param response the server response
     */
    void findPetsByTags(ServerRequest request, ServerResponse response);
    /**
     * GET /pet/{petId} : Find pet by ID.
     *
     * @param request the server request
     * @param response the server response
     */
    void getPetById(ServerRequest request, ServerResponse response);
    /**
     * PUT /pet : Update an existing pet.
     *
     * @param request the server request
     * @param response the server response
     */
    void updatePet(ServerRequest request, ServerResponse response);
    /**
     * POST /pet/{petId} : Updates a pet in the store with form data.
     *
     * @param request the server request
     * @param response the server response
     */
    void updatePetWithForm(ServerRequest request, ServerResponse response);
    /**
     * POST /pet/{petId}/uploadImage : uploads an image.
     *
     * @param request the server request
     * @param response the server response
     */
    void uploadFile(ServerRequest request, ServerResponse response);
    /**
     * POST /fake/{petId}/uploadImageWithRequiredFile : uploads an image (required).
     *
     * @param request the server request
     * @param response the server response
     */
    void uploadFileWithRequiredFile(ServerRequest request, ServerResponse response);
}
