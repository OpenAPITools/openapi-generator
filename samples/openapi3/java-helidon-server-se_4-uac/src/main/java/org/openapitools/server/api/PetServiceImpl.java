package org.openapitools.server.api;

import java.util.ArrayList;
import java.util.stream.Collectors;
import java.io.File;
import java.util.HashMap;
import io.helidon.http.HeaderNames;
import io.helidon.http.Headers;
import java.util.HexFormat;
import java.io.IOException;
import java.io.InputStream;
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
import java.io.UncheckedIOException;
import io.helidon.common.mapper.Value;
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

    @Override
    protected void handleUploadFileWithRequiredFile(ServerRequest request, ServerResponse response, 
                Long petId, 
                ReadablePart requiredFile, 
                Optional<ReadablePart> additionalMetadata) {

        response.status(Status.NOT_IMPLEMENTED_501).send();
    }

}
