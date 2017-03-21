package controllers;

import java.io.File;
import apimodels.Pet;

import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;
import javax.validation.constraints.*;

public class PetApiControllerImp {
    void addPet(Pet body) {
        //Do your magic!!!
        
    }

    void deletePet( Long petId, String apiKey) {
        //Do your magic!!!
        
    }

    List<Pet> findPetsByStatus( List<String> status) {
        //Do your magic!!!
        return new ArrayList<Pet>();
    }

    List<Pet> findPetsByTags( List<String> tags) {
        //Do your magic!!!
        return new ArrayList<Pet>();
    }

    Pet getPetById( Long petId) {
        //Do your magic!!!
        return new Pet();
    }

    void updatePet(Pet body) {
        //Do your magic!!!
        
    }

    void updatePetWithForm( String petId, String name, String status) {
        //Do your magic!!!
        
    }

    void uploadFile( Long petId, String additionalMetadata, Http.MultipartFormData.FilePart file) {
        //Do your magic!!!
        
    }

}
