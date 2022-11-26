package org.openapitools.server.api;

import java.io.File;
import io.helidon.webserver.Handler;
import org.openapitools.server.model.ModelApiResponse;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.openapitools.server.model.Pet;
import java.util.Set;
import java.util.logging.Logger;

import io.helidon.webserver.ServerRequest;
import io.helidon.webserver.ServerResponse;

public class PetServiceImpl implements PetService {

    private static final int HTTP_CODE_NOT_IMPLEMENTED = 501;
    private static final Logger LOGGER = Logger.getLogger(PetService.class.getName());
    private static final ObjectMapper MAPPER = JsonProvider.objectMapper();


    public void addPet(ServerRequest request, ServerResponse response, Pet pet) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void deletePet(ServerRequest request, ServerResponse response) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void findPetsByStatus(ServerRequest request, ServerResponse response) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void findPetsByTags(ServerRequest request, ServerResponse response) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void getPetById(ServerRequest request, ServerResponse response) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void updatePet(ServerRequest request, ServerResponse response, Pet pet) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void updatePetWithForm(ServerRequest request, ServerResponse response) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void uploadFile(ServerRequest request, ServerResponse response) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void uploadFileWithRequiredFile(ServerRequest request, ServerResponse response) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

}
