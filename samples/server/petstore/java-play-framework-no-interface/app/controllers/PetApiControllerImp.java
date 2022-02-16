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
public class PetApiControllerImp  {
    
    public void addPet(Http.Request request, Pet body) throws Exception {
        //Do your magic!!!
    }

    
    public void deletePet(Http.Request request, Long petId, String apiKey) throws Exception {
        //Do your magic!!!
    }

    
    public List<Pet> findPetsByStatus(Http.Request request, @NotNull List<String> status) throws Exception {
        //Do your magic!!!
        return new ArrayList<Pet>();
    }

    
    public List<Pet> findPetsByTags(Http.Request request, @NotNull List<String> tags) throws Exception {
        //Do your magic!!!
        return new ArrayList<Pet>();
    }

    
    public Pet getPetById(Http.Request request, Long petId) throws Exception {
        //Do your magic!!!
        return new Pet();
    }

    
    public void updatePet(Http.Request request, Pet body) throws Exception {
        //Do your magic!!!
    }

    
    public void updatePetWithForm(Http.Request request, Long petId, String name, String status) throws Exception {
        //Do your magic!!!
    }

    
    public ModelApiResponse uploadFile(Http.Request request, Long petId, String additionalMetadata, Http.MultipartFormData.FilePart<TemporaryFile> _file) throws Exception {
        //Do your magic!!!
        return new ModelApiResponse();
    }

}
