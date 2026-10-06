package org.openapitools.server.api;

import java.util.List;
import java.util.Optional;
import org.openapitools.server.model.Pet;
import io.helidon.http.media.multipart.ReadablePart;
import java.util.Set;
import io.helidon.http.Status;
import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;

public class PetServiceImpl extends PetService {

    @Override
    protected void handleAddPet(ServerRequest request, ServerResponse response, 
                Pet pet) {

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    protected void handleDeletePet(ServerRequest request, ServerResponse response, 
                Long petId, 
                Optional<String> apiKey) {

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    protected void handleFindPetsByStatus(ServerRequest request, ServerResponse response, 
                List<String> status) {

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    protected void handleFindPetsByTags(ServerRequest request, ServerResponse response, 
                Set<String> tags) {

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    protected void handleGetPetById(ServerRequest request, ServerResponse response, 
                Long petId) {

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    protected void handleUpdatePet(ServerRequest request, ServerResponse response, 
                Pet pet) {

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    protected void handleUpdatePetWithForm(ServerRequest request, ServerResponse response, 
                Long petId, 
                Optional<String> name, 
                Optional<String> status) {

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

    @Override
    protected void handleUploadFile(ServerRequest request, ServerResponse response, 
                Long petId, 
                Optional<ReadablePart> additionalMetadata, 
                Optional<ReadablePart> _file) {

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

}
