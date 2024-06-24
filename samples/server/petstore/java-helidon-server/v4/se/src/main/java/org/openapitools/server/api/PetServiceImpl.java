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
import 
import io.helidon.common.Errors;
import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;
import jakarta.validation.ValidationException;
import org.openapitools.server.model.GenericTypes;

public class PetServiceImpl implements PetService {

    private static final int HTTP_CODE_NOT_IMPLEMENTED = 501;
    private static final System.Logger LOGGER = System.getLogger(PetService.class.getName());
    private static final ObjectMapper MAPPER = JsonProvider.objectMapper();


    public void addPet(ServerRequest request, ServerResponse response) {
        Errors.Collector errorsCollector = Errors.collector();
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        Pet pet = request.content().as(Pet.class);


        Errors errors = errorsCollector.collect();
        errors.log(LOGGER);
        if (errors.hasFatal()) {
            throw new ValidationException("Validation errors: " + errors);
        }
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void deletePet(ServerRequest request, ServerResponse response) {
        Errors.Collector errorsCollector = Errors.collector();
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        Long petId = request.path()
                .pathParameters()
                .first("petId")
                .map(v -> validator.require("petId", v))
                .map(Long::valueOf)
                .orElse(null);

        Optional<String> apiKey = request.headers()
                .first(HeaderNames.create("api_key"))
                .or(Optional::empty);


        Errors errors = errorsCollector.collect();
        errors.log(LOGGER);
        if (errors.hasFatal()) {
            throw new ValidationException("Validation errors: " + errors);
        }
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void findPetsByStatus(ServerRequest request, ServerResponse response) {
        Errors.Collector errorsCollector = Errors.collector();
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        List<String> status = request.query()
                .all("status")
                .stream()
                .map(String::valueOf)
                .map(v -> validator.check("status",
                     v,
                     List.of("available",
                             "pending",
                             "sold")))
                .collect(HCollectors.toRequiredList("status",
                                                    validator);


        Errors errors = errorsCollector.collect();
        errors.log(LOGGER);
        if (errors.hasFatal()) {
            throw new ValidationException("Validation errors: " + errors);
        }
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void findPetsByTags(ServerRequest request, ServerResponse response) {
        Errors.Collector errorsCollector = Errors.collector();
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        Set<String> tags = request.query()
                .all("tags")
                .stream()
                .map(String::valueOf)
                .collect(HCollectors.toRequiredList("tags",
                                                    validator);


        Errors errors = errorsCollector.collect();
        errors.log(LOGGER);
        if (errors.hasFatal()) {
            throw new ValidationException("Validation errors: " + errors);
        }
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void getPetById(ServerRequest request, ServerResponse response) {
        Errors.Collector errorsCollector = Errors.collector();
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        Long petId = request.path()
                .pathParameters()
                .first("petId")
                .map(v -> validator.require("petId", v))
                .map(Long::valueOf)
                .orElse(null);


        Errors errors = errorsCollector.collect();
        errors.log(LOGGER);
        if (errors.hasFatal()) {
            throw new ValidationException("Validation errors: " + errors);
        }
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void updatePet(ServerRequest request, ServerResponse response) {
        Errors.Collector errorsCollector = Errors.collector();
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        Pet pet = request.content().as(Pet.class);


        Errors errors = errorsCollector.collect();
        errors.log(LOGGER);
        if (errors.hasFatal()) {
            throw new ValidationException("Validation errors: " + errors);
        }
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void updatePetWithForm(ServerRequest request, ServerResponse response) {
        Parameters formParams = request.content().as(Parameters.class);

        Errors.Collector errorsCollector = Errors.collector();
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        Long petId = request.path()
                .pathParameters()
                .first("petId")
                .map(v -> validator.require("petId", v))
                .map(Long::valueOf)
                .orElse(null);

        Optional<String> name = formParams
                .first("name")
                .asOptional()
                .or(Optional::empty);

        Optional<String> status = formParams
                .first("status")
                .asOptional()
                .or(Optional::empty);


        Errors errors = errorsCollector.collect();
        errors.log(LOGGER);
        if (errors.hasFatal()) {
            throw new ValidationException("Validation errors: " + errors);
        }
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void uploadFile(ServerRequest request, ServerResponse response) {
        MultiPart multiPart = request.content().as(MultiPart.class);
        multiPart.forEachRemaining(part -> {
                // TODO: Insert user-implemented handling of multipart data here.
        });
        Errors.Collector errorsCollector = Errors.collector();
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        Long petId = request.path()
                .pathParameters()
                .first("petId")
                .map(v -> validator.require("petId", v))
                .map(Long::valueOf)
                .orElse(null);


        Errors errors = errorsCollector.collect();
        errors.log(LOGGER);
        if (errors.hasFatal()) {
            throw new ValidationException("Validation errors: " + errors);
        }
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    public void uploadFileWithRequiredFile(ServerRequest request, ServerResponse response) {
        MultiPart multiPart = request.content().as(MultiPart.class);
        multiPart.forEachRemaining(part -> {
                // TODO: Insert user-implemented handling of multipart data here.
        });
        Errors.Collector errorsCollector = Errors.collector();
        ValidatorUtils.Validator validator = ValidatorUtils.validator();

        Long petId = request.path()
                .pathParameters()
                .first("petId")
                .map(v -> validator.require("petId", v))
                .map(Long::valueOf)
                .orElse(null);


        Errors errors = errorsCollector.collect();
        errors.log(LOGGER);
        if (errors.hasFatal()) {
            throw new ValidationException("Validation errors: " + errors);
        }
        response.status(HTTP_CODE_NOT_IMPLEMENTED).send();
    }

    @Override
    public void afterStop() {
        System.out.println("Service PetService is down. Goodbye!");
    }
}
