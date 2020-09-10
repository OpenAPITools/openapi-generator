package controllers;

import java.io.InputStream;
import apimodels.ModelApiResponse;
import apimodels.Pet;

import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.io.FileInputStream;
import javax.validation.constraints.*;
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaPlayFrameworkCodegen")
public class PetApiControllerImp implements PetApiControllerImpInterface {
    @Override
    public void addPet(Request request, Pet body)  {
        //Do your magic!!!
    }

    @Override
    public void deletePet(Request request, Long petId, String apiKey)  {
        //Do your magic!!!
    }

    @Override
    public List<Pet> findPetsByStatus(Request request, @NotNull List<String> status)  {
        //Do your magic!!!
        return new ArrayList<Pet>();
    }

    @Override
    public List<Pet> findPetsByTags(Request request, @NotNull List<String> tags)  {
        //Do your magic!!!
        return new ArrayList<Pet>();
    }

    @Override
    public Pet getPetById(Request request, Long petId)  {
        //Do your magic!!!
        return new Pet();
    }

    @Override
    public void updatePet(Request request, Pet body)  {
        //Do your magic!!!
    }

    @Override
    public void updatePetWithForm(Request request, Long petId, String name, String status)  {
        //Do your magic!!!
    }

    @Override
    public ModelApiResponse uploadFile(Request request, Long petId, String additionalMetadata, Http.MultipartFormData.FilePart file)  {
        //Do your magic!!!
        return new ModelApiResponse();
    }

}
