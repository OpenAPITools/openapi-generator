package controllers;

import java.io.InputStream;
import apimodels.ModelApiResponse;
import apimodels.Pet;

import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.CompletableFuture;

import javax.validation.constraints.*;

@SuppressWarnings("RedundantThrows")
public interface PetApiControllerImpInterface {
    default CompletionStage<Result> addPetHttp(Http.Request request, Pet body) throws Exception {
        return CompletableFuture.supplyAsync(() -> {
            addPet(request, body);
            return ok();
        });
    }

    void addPet(Http.Request request, Pet body) throws Exception;

    default CompletionStage<Result> deletePetHttp(Http.Request request, Long petId, String apiKey) throws Exception {
        return CompletableFuture.supplyAsync(() -> {
            deletePet(request, petId, apiKey);
            return ok();
        });
    }

    void deletePet(Http.Request request, Long petId, String apiKey) throws Exception;

    default CompletionStage<Result> findPetsByStatusHttp(Http.Request request, @NotNull List<String> status) throws Exception {
        CompletionStage<List<Pet>> stage = findPetsByStatus(request, status).thenApply(obj -> { 
        if (configuration.getBoolean("useOutputBeanValidation")) {
            for (Pet curItem : obj) {
                OpenAPIUtils.validate(curItem);
            }
        }
            return obj;
        });
            stage.thenApply(obj -> {
            JsonNode result = mapper.valueToTree(obj);
            return ok(result);
        });
    }

    CompletionStage<List<Pet>> findPetsByStatus(Http.Request request, @NotNull List<String> status) throws Exception;

    default CompletionStage<Result> findPetsByTagsHttp(Http.Request request, @NotNull List<String> tags) throws Exception {
        CompletionStage<List<Pet>> stage = findPetsByTags(request, tags).thenApply(obj -> { 
        if (configuration.getBoolean("useOutputBeanValidation")) {
            for (Pet curItem : obj) {
                OpenAPIUtils.validate(curItem);
            }
        }
            return obj;
        });
            stage.thenApply(obj -> {
            JsonNode result = mapper.valueToTree(obj);
            return ok(result);
        });
    }

    CompletionStage<List<Pet>> findPetsByTags(Http.Request request, @NotNull List<String> tags) throws Exception;

    default CompletionStage<Result> getPetByIdHttp(Http.Request request, Long petId) throws Exception {
        CompletionStage<Pet> stage = getPetById(request, petId).thenApply(obj -> { 
        if (configuration.getBoolean("useOutputBeanValidation")) {
            OpenAPIUtils.validate(obj);
        }
            return obj;
        });
            stage.thenApply(obj -> {
            JsonNode result = mapper.valueToTree(obj);
            return ok(result);
        });
    }

    CompletionStage<Pet> getPetById(Http.Request request, Long petId) throws Exception;

    default CompletionStage<Result> updatePetHttp(Http.Request request, Pet body) throws Exception {
        return CompletableFuture.supplyAsync(() -> {
            updatePet(request, body);
            return ok();
        });
    }

    void updatePet(Http.Request request, Pet body) throws Exception;

    default CompletionStage<Result> updatePetWithFormHttp(Http.Request request, Long petId, String name, String status) throws Exception {
        return CompletableFuture.supplyAsync(() -> {
            updatePetWithForm(request, petId, name, status);
            return ok();
        });
    }

    void updatePetWithForm(Http.Request request, Long petId, String name, String status) throws Exception;

    default CompletionStage<Result> uploadFileHttp(Http.Request request, Long petId, String additionalMetadata, Http.MultipartFormData.FilePart file) throws Exception {
        CompletionStage<ModelApiResponse> stage = uploadFile(request, petId, additionalMetadata, file).thenApply(obj -> { 
        if (configuration.getBoolean("useOutputBeanValidation")) {
            OpenAPIUtils.validate(obj);
        }
            return obj;
        });
            stage.thenApply(obj -> {
            JsonNode result = mapper.valueToTree(obj);
            return ok(result);
        });
    }

    CompletionStage<ModelApiResponse> uploadFile(Http.Request request, Long petId, String additionalMetadata, Http.MultipartFormData.FilePart file) throws Exception;

}
