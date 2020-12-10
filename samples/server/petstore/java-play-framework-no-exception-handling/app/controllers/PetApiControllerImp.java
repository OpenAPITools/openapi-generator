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
import play.libs.Files.TemporaryFile;
import javax.validation.constraints.*;
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaPlayFrameworkCodegen")
public class PetApiControllerImp extends PetApiControllerImpInterface {
    @Override
    public void addPet(Http.Request request, Pet body)  {
        //Do your magic!!!
    }

    @Override
    public void deletePet(Http.Request request, Long petId, String apiKey)  {
        //Do your magic!!!
    }

    @Override
    public List<Pet> findPetsByStatus(Http.Request request, @NotNull List<String> status)  {
        //Do your magic!!!
        return new ArrayList<Pet>();
    }

    @Override
    public List<Pet> findPetsByTags(Http.Request request, @NotNull List<String> tags)  {
        //Do your magic!!!
        return new ArrayList<Pet>();
    }

    @Override
    public Pet getPetById(Http.Request request, Long petId)  {
        //Do your magic!!!
        return new Pet();
    }

    @Override
    public void updatePet(Http.Request request, Pet body)  {
        //Do your magic!!!
    }

    @Override
    public void updatePetWithForm(Http.Request request, Long petId, String name, String status)  {
        //Do your magic!!!
    }

    @Override
    public ModelApiResponse uploadFile(Http.Request request, Long petId, String additionalMetadata, Http.MultipartFormData.FilePart<TemporaryFile> file)  {
        //Do your magic!!!
        return new ModelApiResponse();
    }

}
