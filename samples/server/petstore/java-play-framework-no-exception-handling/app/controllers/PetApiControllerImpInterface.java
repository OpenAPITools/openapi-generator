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
    void addPet(Http.Request request, Pet body) ;

    void deletePet(Http.Request request, Long petId, String apiKey) ;

    List<Pet> findPetsByStatus(Http.Request request, @NotNull List<String> status) ;

    List<Pet> findPetsByTags(Http.Request request, @NotNull List<String> tags) ;

    Pet getPetById(Http.Request request, Long petId) ;

    void updatePet(Http.Request request, Pet body) ;

    void updatePetWithForm(Http.Request request, Long petId, String name, String status) ;

    ModelApiResponse uploadFile(Http.Request request, Long petId, String additionalMetadata, Http.MultipartFormData.FilePart file) ;

}
