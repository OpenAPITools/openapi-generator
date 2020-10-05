package com.puppies.store.apis;

import java.io.InputStream;
import apimodels.ModelApiResponse;
import apimodels.Pet;

import com.google.inject.Inject;
import com.google.inject.Singleton;
import com.typesafe.config.Config;
import play.mvc.Controller;
import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;
import play.mvc.Result;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.JsonNode;
import openapitools.OpenAPIUtils;
import static play.mvc.Results.ok;

import javax.validation.constraints.*;

@Singleton
@SuppressWarnings("RedundantThrows")
public abstract class PetApiControllerImpInterface {
    @Inject private Config configuration;
    private ObjectMapper mapper = new ObjectMapper();

    Result addPetHttp(Http.Request request, Pet body) throws Exception {
        addPet(request, body);
return ok();

    }

    abstract void addPet(Http.Request request, Pet body) throws Exception;

    Result deletePetHttp(Http.Request request, Long petId, String apiKey) throws Exception {
        deletePet(request, petId, apiKey);
return ok();

    }

    abstract void deletePet(Http.Request request, Long petId, String apiKey) throws Exception;

    Result findPetsByStatusHttp(Http.Request request, @NotNull List<String> status) throws Exception {
        List<Pet> obj = findPetsByStatus(request, status);
    if (configuration.getBoolean("useOutputBeanValidation")) {
        for (Pet curItem : obj) {
            OpenAPIUtils.validate(curItem);
        }
    }
JsonNode result = mapper.valueToTree(obj);
return ok(result);

    }

    abstract List<Pet> findPetsByStatus(Http.Request request, @NotNull List<String> status) throws Exception;

    Result findPetsByTagsHttp(Http.Request request, @NotNull List<String> tags) throws Exception {
        List<Pet> obj = findPetsByTags(request, tags);
    if (configuration.getBoolean("useOutputBeanValidation")) {
        for (Pet curItem : obj) {
            OpenAPIUtils.validate(curItem);
        }
    }
JsonNode result = mapper.valueToTree(obj);
return ok(result);

    }

    abstract List<Pet> findPetsByTags(Http.Request request, @NotNull List<String> tags) throws Exception;

    Result getPetByIdHttp(Http.Request request, Long petId) throws Exception {
        Pet obj = getPetById(request, petId);
    if (configuration.getBoolean("useOutputBeanValidation")) {
            OpenAPIUtils.validate(obj);
    }
JsonNode result = mapper.valueToTree(obj);
return ok(result);

    }

    abstract Pet getPetById(Http.Request request, Long petId) throws Exception;

    Result updatePetHttp(Http.Request request, Pet body) throws Exception {
        updatePet(request, body);
return ok();

    }

    abstract void updatePet(Http.Request request, Pet body) throws Exception;

    Result updatePetWithFormHttp(Http.Request request, Long petId, String name, String status) throws Exception {
        updatePetWithForm(request, petId, name, status);
return ok();

    }

    abstract void updatePetWithForm(Http.Request request, Long petId, String name, String status) throws Exception;

    Result uploadFileHttp(Http.Request request, Long petId, String additionalMetadata, Http.MultipartFormData.FilePart file) throws Exception {
        ModelApiResponse obj = uploadFile(request, petId, additionalMetadata, file);
    if (configuration.getBoolean("useOutputBeanValidation")) {
            OpenAPIUtils.validate(obj);
    }
JsonNode result = mapper.valueToTree(obj);
return ok(result);

    }

    abstract ModelApiResponse uploadFile(Http.Request request, Long petId, String additionalMetadata, Http.MultipartFormData.FilePart file) throws Exception;

}
