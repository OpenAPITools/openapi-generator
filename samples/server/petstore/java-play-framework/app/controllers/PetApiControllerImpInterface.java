package controllers;

import java.io.File;
import apimodels.Pet;

import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;

import javax.validation.constraints.*;

public interface PetApiControllerImpInterface {
    void addPet(Pet body) throws Exception;

    void deletePet(Long petId, String apiKey) throws Exception;

    List<Pet> findPetsByStatus( List<String> status) throws Exception;

    List<Pet> findPetsByTags( List<String> tags) throws Exception;

    Pet getPetById(Long petId) throws Exception;

    void updatePet(Pet body) throws Exception;

    void updatePetWithForm(String petId, String name, String status) throws Exception;

    void uploadFile(Long petId, String additionalMetadata, Http.MultipartFormData.FilePart file) throws Exception;

}
