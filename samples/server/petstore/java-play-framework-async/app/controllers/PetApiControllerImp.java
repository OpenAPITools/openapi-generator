package controllers;

import java.io.InputStream;
import apimodels.ModelApiResponse;
import apimodels.Pet;

import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;
import java.io.FileInputStream;
import javax.validation.constraints.*;

public class PetApiControllerImp implements PetApiControllerImpInterface {
    @Override
    public void addPet(Pet body) throws Exception {
        //Do your magic!!!
    }

    @Override
    public void deletePet(Long petId, String apiKey) throws Exception {
        //Do your magic!!!
    }

    @Override
    public CompletionStage<List<Pet>> findPetsByStatus( @NotNull List<String> status) throws Exception {
        //Do your magic!!!
        return CompletableFuture.supplyAsync(() -> {
           return new ArrayList<Pet>();
        });
    }

    @Override
    public CompletionStage<List<Pet>> findPetsByTags( @NotNull List<String> tags) throws Exception {
        //Do your magic!!!
        return CompletableFuture.supplyAsync(() -> {
           return new ArrayList<Pet>();
        });
    }

    @Override
    public CompletionStage<Pet> getPetById(Long petId) throws Exception {
        //Do your magic!!!
        return CompletableFuture.supplyAsync(() -> {
           return new Pet();
        });
    }

    @Override
    public void updatePet(Pet body) throws Exception {
        //Do your magic!!!
    }

    @Override
    public void updatePetWithForm(Long petId, String name, String status) throws Exception {
        //Do your magic!!!
    }

    @Override
    public CompletionStage<ModelApiResponse> uploadFile(Long petId, String additionalMetadata, Http.MultipartFormData.FilePart file) throws Exception {
        //Do your magic!!!
        return CompletableFuture.supplyAsync(() -> {
           return new ModelApiResponse();
        });
    }

}
