package com.puppies.store.apis;

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
    void addPet(Http.Request request, Pet body) throws Exception;

    void deletePet(Http.Request request, Long petId, String apiKey) throws Exception;

    List<Pet> findPetsByStatus(Http.Request request, @NotNull List<String> status) throws Exception;

    List<Pet> findPetsByTags(Http.Request request, @NotNull List<String> tags) throws Exception;

    Pet getPetById(Http.Request request, Long petId) throws Exception;

    void updatePet(Http.Request request, Pet body) throws Exception;

    void updatePetWithForm(Http.Request request, Long petId, String name, String status) throws Exception;

    ModelApiResponse uploadFile(Http.Request request, Long petId, String additionalMetadata, Http.MultipartFormData.FilePart file) throws Exception;

}
