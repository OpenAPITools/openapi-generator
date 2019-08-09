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
    void addPet(Pet body) ;

    void deletePet(Long petId, String apiKey) ;

    List<Pet> findPetsByStatus( @NotNull List<String> status) ;

    List<Pet> findPetsByTags( @NotNull List<String> tags) ;

    Pet getPetById(Long petId) ;

    void updatePet(Pet body) ;

    void updatePetWithForm(Long petId, String name, String status) ;

    ModelApiResponse uploadFile(Long petId, String additionalMetadata, Http.MultipartFormData.FilePart file) ;

}
