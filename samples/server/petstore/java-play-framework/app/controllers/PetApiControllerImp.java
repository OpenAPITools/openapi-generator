package controllers;

import java.io.File;
import apimodels.Pet;

import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;
import javax.validation.constraints.*;

public class PetApiControllerImp implements PetApiControllerImpInterface {
    public void addPet(Pet body) throws Exception {
        //Do your magic!!!
        
    }

    public void deletePet(Long petId, String apiKey) throws Exception {
        //Do your magic!!!
        
    }

    public List<Pet> findPetsByStatus( List<String> status) throws Exception {
        //Do your magic!!!
        return new ArrayList<Pet>();
    }

    public List<Pet> findPetsByTags( List<String> tags) throws Exception {
        //Do your magic!!!
        return new ArrayList<Pet>();
    }

    public Pet getPetById(Long petId) throws Exception {
        //Do your magic!!!
        return new Pet();
    }

    public void updatePet(Pet body) throws Exception {
        //Do your magic!!!
        
    }

    public void updatePetWithForm(String petId, String name, String status) throws Exception {
        //Do your magic!!!
        
    }

    public void uploadFile(Long petId, String additionalMetadata, Http.MultipartFormData.FilePart file) throws Exception {
        //Do your magic!!!
        
    }

}
