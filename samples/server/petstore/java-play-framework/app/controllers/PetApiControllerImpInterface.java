package controllers;

import java.io.File;
import apimodels.Pet;

import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;

import play.mvc.Http;

public interface PetApiControllerImpInterface {
    void addPet(Pet body);

    void deletePet( Long petId, String apiKey);

    List<Pet> findPetsByStatus( List<String> status);

    List<Pet> findPetsByTags( List<String> tags);

    Pet getPetById( Long petId);

    void updatePet(Pet body);

    void updatePetWithForm( String petId, String name, String status);

    void uploadFile( Long petId, String additionalMetadata, Http.MultipartFormData.FilePart file);

}
