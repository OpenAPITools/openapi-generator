package controllers;

import java.io.InputStream;
import apimodels.ModelApiResponse;
import apimodels.Pet;
import java.util.Set;

import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;

import javax.validation.constraints.*;

@SuppressWarnings("RedundantThrows")
public interface PetApiControllerImpInterface {
    void addPet(Pet body) throws Exception;

    void deletePet(Long petId, String apiKey) throws Exception;

    List<Pet> findPetsByStatus( @NotNull List<String> status) throws Exception;

    Set<Pet> findPetsByTags( @NotNull Set<String> tags) throws Exception;

    Pet getPetById(Long petId) throws Exception;

    void updatePet(Pet body) throws Exception;

    void updatePetWithForm(Long petId, String name, String status) throws Exception;

    ModelApiResponse uploadFile(Long petId, String additionalMetadata, Http.MultipartFormData.FilePart file) throws Exception;

    ModelApiResponse uploadFileWithRequiredFile(Long petId, Http.MultipartFormData.FilePart requiredFile, String additionalMetadata) throws Exception;

}
