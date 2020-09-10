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
    void addPet(Request request, Pet body) throws Exception;

    void deletePet(Request request, Long petId, String apiKey) throws Exception;

    CompletionStage<List<Pet>> findPetsByStatus(Request request, @NotNull List<String> status) throws Exception;

    CompletionStage<List<Pet>> findPetsByTags(Request request, @NotNull List<String> tags) throws Exception;

    CompletionStage<Pet> getPetById(Request request, Long petId) throws Exception;

    void updatePet(Request request, Pet body) throws Exception;

    void updatePetWithForm(Request request, Long petId, String name, String status) throws Exception;

    CompletionStage<ModelApiResponse> uploadFile(Request request, Long petId, String additionalMetadata, Http.MultipartFormData.FilePart file) throws Exception;

}
