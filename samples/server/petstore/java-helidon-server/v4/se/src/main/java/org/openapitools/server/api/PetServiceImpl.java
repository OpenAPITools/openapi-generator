package org.openapitools.server.api;

import java.io.File;
import io.helidon.http.HeaderNames;
import io.helidon.http.Headers;
import java.util.List;
import org.openapitools.server.model.ModelApiResponse;
import io.helidon.http.media.multipart.MultiPart;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.Optional;
import io.helidon.common.parameters.Parameters;
import org.openapitools.server.model.Pet;
import java.util.Set;
import io.helidon.common.mapper.Value;
import java.util.logging.Logger;

import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;
import org.openapitools.server.model.GenericTypes;

public class PetServiceImpl implements PetService {

    private static final int HTTP_CODE_NOT_IMPLEMENTED = 501;
    private static final Logger LOGGER = Logger.getLogger(PetService.class.getName());
    private static final ObjectMapper MAPPER = JsonProvider.objectMapper();


    public void addPet(ServerRequest request, ServerResponse response) {
        Pet pet = request.content().as(Pet.class);


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void deletePet(ServerRequest request, ServerResponse response) {
        Long petId = ValidatorUtils.nonEmpty(request.path()
                .pathParameters()
                .first("petId")
                .map(Long::valueOf));

        Optional<String> apiKey = request.headers()
                .first(HeaderNames.create("api_key"));


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void findPetsByStatus(ServerRequest request, ServerResponse response) {
        List<String> status = ValidatorUtils.nonEmpty(request.query()
                .all("status")
                .stream()
                .toList());


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void findPetsByTags(ServerRequest request, ServerResponse response) {
        Set<String> tags = ValidatorUtils.nonEmpty(request.query()
                .all("tags")
                .stream()
                .toSet());


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void getPetById(ServerRequest request, ServerResponse response) {
        Long petId = ValidatorUtils.nonEmpty(request.path()
                .pathParameters()
                .first("petId")
                .map(Long::valueOf));


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void updatePet(ServerRequest request, ServerResponse response) {
        Pet pet = request.content().as(Pet.class);


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void updatePetWithForm(ServerRequest request, ServerResponse response) {
        Parameters formParams = request.content().as(Parameters.class);
        Long petId = ValidatorUtils.nonEmpty(request.path()
                .pathParameters()
                .first("petId")
                .map(Long::valueOf));

        Optional<String> name = formParams.first("name")
                .asOptional();
        Optional<String> status = formParams.first("status")
                .asOptional();

        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void uploadFile(ServerRequest request, ServerResponse response) {
        MultiPart multiPart = request.content().as(MultiPart.class);
        multiPart.forEachRemaining(part -> {
                // TODO: Insert user-implemented handling of multipart data here.
        });
        Long petId = ValidatorUtils.nonEmpty(request.path()
                .pathParameters()
                .first("petId")
                .map(Long::valueOf));


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void uploadFileWithRequiredFile(ServerRequest request, ServerResponse response) {
        MultiPart multiPart = request.content().as(MultiPart.class);
        multiPart.forEachRemaining(part -> {
                // TODO: Insert user-implemented handling of multipart data here.
        });
        Long petId = ValidatorUtils.nonEmpty(request.path()
                .pathParameters()
                .first("petId")
                .map(Long::valueOf));


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    @Override
    public void afterStop() {
        System.out.println("Service PetService is down. Goodbye!");
    }
}
