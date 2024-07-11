package org.openapitools.server.api;

import java.util.stream.Collectors;
import java.io.File;
import io.helidon.http.HeaderNames;
import io.helidon.http.Headers;
import java.util.HexFormat;
import java.util.List;
import java.util.Map;
import org.openapitools.server.model.ModelApiResponse;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.Optional;
import io.helidon.common.parameters.Parameters;
import org.openapitools.server.model.Pet;
import io.helidon.http.media.multipart.ReadablePart;
import java.util.Set;
import io.helidon.http.Status;
import io.helidon.common.mapper.Value;
import java.util.logging.Logger;import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;

public class PetServiceImpl implements PetService {

    private static final int HTTP_CODE_NOT_IMPLEMENTED = 501;
    private static final Logger LOGGER = Logger.getLogger(PetService.class.getName());
    private static final ObjectMapper MAPPER = JsonProvider.objectMapper();


    @Override
    public void addPet(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    @Override
    public void deletePet(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    @Override
    public void findPetsByStatus(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    @Override
    public void findPetsByTags(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    @Override
    public void getPetById(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    @Override
    public void updatePet(ServerRequest request, ServerResponse response) {
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    @Override
    public void updatePetWithForm(ServerRequest request, ServerResponse response) {
        Parameters formParams = request.content().as(Parameters.class);
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    @Override
    public void uploadFile(ServerRequest request, ServerResponse response) {
        Map<String, ReadablePart> parts = PartsUtils.partsMap(request);
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    @Override
    public void uploadFileWithRequiredFile(ServerRequest request, ServerResponse response) {
        Map<String, ReadablePart> parts = PartsUtils.partsMap(request);
        ValidatorUtils.Validator validator = ValidatorUtils.validator(LOGGER);


        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }


    @Override
    public void afterStop() {
        System.out.println("Service PetService is down. Goodbye!");
    }

}
