package controllers;

import java.io.InputStream;
import apimodels.ModelApiResponse;
import apimodels.Pet;

import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;

import javax.validation.constraints.*;

@SuppressWarnings("RedundantThrows")
public interface PetApiControllerImpInterface {
    default Result addPetHttp(Http.Request request, Pet body) throws Exception {
        addPet(request, body);
        return ok();
    }

    void addPet(Http.Request request, Pet body) throws Exception;

    default Result deletePetHttp(Http.Request request, Long petId, String apiKey) throws Exception {
        deletePet(request, petId, apiKey);
        return ok();
    }

    void deletePet(Http.Request request, Long petId, String apiKey) throws Exception;

    default Result findPetsByStatusHttp(Http.Request request, @NotNull List<String> status) throws Exception {
        List<Pet> obj = findPetsByStatus(request, status);
        if (configuration.getBoolean("useOutputBeanValidation")) {
            for (Pet curItem : obj) {
                OpenAPIUtils.validate(curItem);
            }
        }
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
    }

    List<Pet> findPetsByStatus(Http.Request request, @NotNull List<String> status) throws Exception;

    default Result findPetsByTagsHttp(Http.Request request, @NotNull List<String> tags) throws Exception {
        List<Pet> obj = findPetsByTags(request, tags);
        if (configuration.getBoolean("useOutputBeanValidation")) {
            for (Pet curItem : obj) {
                OpenAPIUtils.validate(curItem);
            }
        }
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
    }

    List<Pet> findPetsByTags(Http.Request request, @NotNull List<String> tags) throws Exception;

    default Result getPetByIdHttp(Http.Request request, Long petId) throws Exception {
        Pet obj = getPetById(request, petId);
        if (configuration.getBoolean("useOutputBeanValidation")) {
                OpenAPIUtils.validate(obj);
        }
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
    }

    Pet getPetById(Http.Request request, Long petId) throws Exception;

    default Result updatePetHttp(Http.Request request, Pet body) throws Exception {
        updatePet(request, body);
        return ok();
    }

    void updatePet(Http.Request request, Pet body) throws Exception;

    default Result updatePetWithFormHttp(Http.Request request, Long petId, String name, String status) throws Exception {
        updatePetWithForm(request, petId, name, status);
        return ok();
    }

    void updatePetWithForm(Http.Request request, Long petId, String name, String status) throws Exception;

    default Result uploadFileHttp(Http.Request request, Long petId, String additionalMetadata, Http.MultipartFormData.FilePart file) throws Exception {
        ModelApiResponse obj = uploadFile(request, petId, additionalMetadata, file);
        if (configuration.getBoolean("useOutputBeanValidation")) {
                OpenAPIUtils.validate(obj);
        }
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
    }

    ModelApiResponse uploadFile(Http.Request request, Long petId, String additionalMetadata, Http.MultipartFormData.FilePart file) throws Exception;

}
