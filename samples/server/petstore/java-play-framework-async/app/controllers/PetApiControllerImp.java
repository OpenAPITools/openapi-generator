package controllers;

import java.io.InputStream;
import apimodels.ModelApiResponse;
import apimodels.Pet;

import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.io.FileInputStream;
import play.libs.Files.TemporaryFile;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.CompletableFuture;
import javax.validation.constraints.*;
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaPlayFrameworkCodegen")
public class PetApiControllerImp extends PetApiControllerImpInterface {
    @Override
    public void addPet(Http.Request request, Pet body) throws Exception {
        //Do your magic!!!
    }

    @Override
    public void deletePet(Http.Request request, Long petId, String apiKey) throws Exception {
        //Do your magic!!!
    }

    @Override
    public CompletionStage<List<Pet>> findPetsByStatus(Http.Request request, @NotNull List<String> status) throws Exception {
        //Do your magic!!!
        return CompletableFuture.supplyAsync(() -> {
           return new ArrayList<Pet>();
        });
    }

    @Override
    public CompletionStage<List<Pet>> findPetsByTags(Http.Request request, @NotNull List<String> tags) throws Exception {
        //Do your magic!!!
        return CompletableFuture.supplyAsync(() -> {
           return new ArrayList<Pet>();
        });
    }

    @Override
    public CompletionStage<Pet> getPetById(Http.Request request, Long petId) throws Exception {
        //Do your magic!!!
        return CompletableFuture.supplyAsync(() -> {
           return new Pet();
        });
    }

    @Override
    public void updatePet(Http.Request request, Pet body) throws Exception {
        //Do your magic!!!
    }

    @Override
    public void updatePetWithForm(Http.Request request, Long petId, String name, String status) throws Exception {
        //Do your magic!!!
    }

    @Override
    public CompletionStage<ModelApiResponse> uploadFile(Http.Request request, Long petId, String additionalMetadata, Http.MultipartFormData.FilePart<TemporaryFile> file) throws Exception {
        //Do your magic!!!
        return CompletableFuture.supplyAsync(() -> {
           return new ModelApiResponse();
        });
    }

}
