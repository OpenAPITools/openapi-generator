package controllers;

import java.io.InputStream;
import apimodels.ModelApiResponse;
import apimodels.Pet;

import com.google.inject.Inject;
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
import play.libs.Files.TemporaryFile;
import java.util.concurrent.CompletionException;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.CompletableFuture;

import javax.validation.constraints.*;

@SuppressWarnings("RedundantThrows")
public abstract class PetApiControllerImpInterface {
    @Inject private Config configuration;
    private ObjectMapper mapper = new ObjectMapper();

    public CompletionStage<Result> addPetHttp(Http.Request request, Pet body) throws Exception {
        CompletableFuture<Result> result = CompletableFuture.supplyAsync(() -> {
        try {
    addPet(request, body);
        } catch (Exception e) {
            throw new CompletionException(e);
        }
        return ok();
    });
    return result;

    }

    public abstract void addPet(Http.Request request, Pet body) throws Exception;

    public CompletionStage<Result> deletePetHttp(Http.Request request, Long petId, String apiKey) throws Exception {
        CompletableFuture<Result> result = CompletableFuture.supplyAsync(() -> {
        try {
    deletePet(request, petId, apiKey);
        } catch (Exception e) {
            throw new CompletionException(e);
        }
        return ok();
    });
    return result;

    }

    public abstract void deletePet(Http.Request request, Long petId, String apiKey) throws Exception;

    public CompletionStage<Result> findPetsByStatusHttp(Http.Request request, @NotNull List<String> status) throws Exception {
        CompletionStage<List<Pet>> stage = findPetsByStatus(request, status).thenApply(obj -> { 
    if (configuration.getBoolean("useOutputBeanValidation")) {
        for (Pet curItem : obj) {
            OpenAPIUtils.validate(curItem);
        }
    }
    return obj;
});
return stage.thenApply(obj -> {
    JsonNode result = mapper.valueToTree(obj);
    return ok(result);
});

    }

    public abstract CompletionStage<List<Pet>> findPetsByStatus(Http.Request request, @NotNull List<String> status) throws Exception;

    public CompletionStage<Result> findPetsByTagsHttp(Http.Request request, @NotNull List<String> tags) throws Exception {
        CompletionStage<List<Pet>> stage = findPetsByTags(request, tags).thenApply(obj -> { 
    if (configuration.getBoolean("useOutputBeanValidation")) {
        for (Pet curItem : obj) {
            OpenAPIUtils.validate(curItem);
        }
    }
    return obj;
});
return stage.thenApply(obj -> {
    JsonNode result = mapper.valueToTree(obj);
    return ok(result);
});

    }

    public abstract CompletionStage<List<Pet>> findPetsByTags(Http.Request request, @NotNull List<String> tags) throws Exception;

    public CompletionStage<Result> getPetByIdHttp(Http.Request request, Long petId) throws Exception {
        CompletionStage<Pet> stage = getPetById(request, petId).thenApply(obj -> { 
    if (configuration.getBoolean("useOutputBeanValidation")) {
        OpenAPIUtils.validate(obj);
    }
    return obj;
});
return stage.thenApply(obj -> {
    JsonNode result = mapper.valueToTree(obj);
    return ok(result);
});

    }

    public abstract CompletionStage<Pet> getPetById(Http.Request request, Long petId) throws Exception;

    public CompletionStage<Result> updatePetHttp(Http.Request request, Pet body) throws Exception {
        CompletableFuture<Result> result = CompletableFuture.supplyAsync(() -> {
        try {
    updatePet(request, body);
        } catch (Exception e) {
            throw new CompletionException(e);
        }
        return ok();
    });
    return result;

    }

    public abstract void updatePet(Http.Request request, Pet body) throws Exception;

    public CompletionStage<Result> updatePetWithFormHttp(Http.Request request, Long petId, String name, String status) throws Exception {
        CompletableFuture<Result> result = CompletableFuture.supplyAsync(() -> {
        try {
    updatePetWithForm(request, petId, name, status);
        } catch (Exception e) {
            throw new CompletionException(e);
        }
        return ok();
    });
    return result;

    }

    public abstract void updatePetWithForm(Http.Request request, Long petId, String name, String status) throws Exception;

    public CompletionStage<Result> uploadFileHttp(Http.Request request, Long petId, String additionalMetadata, Http.MultipartFormData.FilePart<TemporaryFile> file) throws Exception {
        CompletionStage<ModelApiResponse> stage = uploadFile(request, petId, additionalMetadata, file).thenApply(obj -> { 
    if (configuration.getBoolean("useOutputBeanValidation")) {
        OpenAPIUtils.validate(obj);
    }
    return obj;
});
return stage.thenApply(obj -> {
    JsonNode result = mapper.valueToTree(obj);
    return ok(result);
});

    }

    public abstract CompletionStage<ModelApiResponse> uploadFile(Http.Request request, Long petId, String additionalMetadata, Http.MultipartFormData.FilePart<TemporaryFile> file) throws Exception;

}
