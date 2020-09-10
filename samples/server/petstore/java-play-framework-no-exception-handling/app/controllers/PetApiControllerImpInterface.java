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
    void addPet(Request request, Pet body) ;

    void deletePet(Request request, Long petId, String apiKey) ;

    List<Pet> findPetsByStatus(Request request, @NotNull List<String> status) ;

    List<Pet> findPetsByTags(Request request, @NotNull List<String> tags) ;

    Pet getPetById(Request request, Long petId) ;

    void updatePet(Request request, Pet body) ;

    void updatePetWithForm(Request request, Long petId, String name, String status) ;

    ModelApiResponse uploadFile(Request request, Long petId, String additionalMetadata, Http.MultipartFormData.FilePart file) ;

}
