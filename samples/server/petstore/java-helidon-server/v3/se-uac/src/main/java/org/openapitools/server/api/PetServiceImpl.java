package org.openapitools.server.api;

import java.util.ArrayList;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import io.helidon.common.http.DataChunk;
import java.io.File;
import io.helidon.webserver.Handler;
import java.util.HashMap;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.Map;
import org.openapitools.server.model.ModelApiResponse;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.openapitools.server.model.Pet;
import io.helidon.media.multipart.ReadableBodyPart;
import java.util.Set;
import java.io.UncheckedIOException;
import io.helidon.webserver.ServerRequest;
import io.helidon.webserver.ServerResponse;

public class PetServiceImpl extends PetService {

    private static final int HTTP_CODE_NOT_IMPLEMENTED = 501;

    public void handleAddPet(ServerRequest request, ServerResponse response, Pet pet) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void handleDeletePet(ServerRequest request, ServerResponse response, Long petId, String apiKey) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void handleFindPetsByStatus(ServerRequest request, ServerResponse response, List<String> status) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void handleFindPetsByTags(ServerRequest request, ServerResponse response, List<String> tags) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void handleGetPetById(ServerRequest request, ServerResponse response, Long petId) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void handleUpdatePet(ServerRequest request, ServerResponse response, Pet pet) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void handleUpdatePetWithForm(ServerRequest request, ServerResponse response, Long petId, String name, String status) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void handleUploadFile(ServerRequest request, ServerResponse response, Long petId, String additionalMetadata, InputStream _file) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void handleUploadFileWithRequiredFile(ServerRequest request, ServerResponse response, Long petId, InputStream requiredFile, String additionalMetadata) {
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }


    public Void handleError(ServerRequest request, ServerResponse response, Throwable throwable) {
        return response.send(throwable);
    }
}
