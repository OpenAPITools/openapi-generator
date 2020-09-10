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
import javax.validation.constraints.*;
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaPlayFrameworkCodegen")
public class PetApiControllerImp implements PetApiControllerImpInterface {
    @Override
    public void addPet(Request request, Pet body) throws Exception {
        //Do your magic!!!
    }

    @Override
    public void deletePet(Request request, Long petId, String apiKey) throws Exception {
        //Do your magic!!!
    }

    @Override
    public CompletionStage<List<Pet>> findPetsByStatus(Request request, @NotNull List<String> status) throws Exception {
        //Do your magic!!!
        return CompletableFuture.supplyAsync(() -> {
           return new ArrayList<Pet>();
        });
    }

    @Override
    public CompletionStage<List<Pet>> findPetsByTags(Request request, @NotNull List<String> tags) throws Exception {
        //Do your magic!!!
        return CompletableFuture.supplyAsync(() -> {
           return new ArrayList<Pet>();
        });
    }

    @Override
    public CompletionStage<Pet> getPetById(Request request, Long petId) throws Exception {
        //Do your magic!!!
        return CompletableFuture.supplyAsync(() -> {
           return new Pet();
        });
    }

    @Override
    public void updatePet(Request request, Pet body) throws Exception {
        //Do your magic!!!
    }

    @Override
    public void updatePetWithForm(Request request, Long petId, String name, String status) throws Exception {
        //Do your magic!!!
    }

    @Override
    public CompletionStage<ModelApiResponse> uploadFile(Request request, Long petId, String additionalMetadata, Http.MultipartFormData.FilePart file) throws Exception {
        //Do your magic!!!
        return CompletableFuture.supplyAsync(() -> {
           return new ModelApiResponse();
        });
    }

}
