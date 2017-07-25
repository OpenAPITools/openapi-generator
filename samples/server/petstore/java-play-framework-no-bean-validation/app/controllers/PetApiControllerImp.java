package controllers;

import java.io.InputStream;
import apimodels.ModelApiResponse;
import apimodels.Pet;

import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;
import java.io.FileInputStream;

public class PetApiControllerImp implements PetApiControllerImpInterface {

    private final ObjectMapper mapper;

    @Inject
    private PetApiControllerImp() {
        mapper = new ObjectMapper();
    }

    @Override
    public void addPet(Pet body) throws Exception {
        //Do your magic!!!
        
    }

    @Override
    public void deletePet(Long petId, String apiKey) throws Exception {
        //Do your magic!!!
        
    }

    @Override
    public List<Pet> findPetsByStatus(List<String> status) throws Exception {
        //Do your magic!!!
        String accept = request().getHeader("Accept");
        if (accept != null && accept.contains("application/xml")) {
            return mapper.readValue("", List.class);
        }
        String accept = request().getHeader("Accept");
        if (accept != null && accept.contains("application/json")) {
            return mapper.readValue("", List.class);
        }
        return new ArrayList<Pet>();
    }

    @Override
    public List<Pet> findPetsByTags(List<String> tags) throws Exception {
        //Do your magic!!!
        String accept = request().getHeader("Accept");
        if (accept != null && accept.contains("application/xml")) {
            return mapper.readValue("", List.class);
        }
        String accept = request().getHeader("Accept");
        if (accept != null && accept.contains("application/json")) {
            return mapper.readValue("", List.class);
        }
        return new ArrayList<Pet>();
    }

    @Override
    public Pet getPetById(Long petId) throws Exception {
        //Do your magic!!!
        String accept = request().getHeader("Accept");
        if (accept != null && accept.contains("application/xml")) {
            return mapper.readValue("", Pet.class);
        }
        String accept = request().getHeader("Accept");
        if (accept != null && accept.contains("application/json")) {
            return mapper.readValue("", Pet.class);
        }
        return new Pet();
    }

    @Override
    public void updatePet(Pet body) throws Exception {
        //Do your magic!!!
        
    }

    @Override
    public void updatePetWithForm(Long petId, String name, String status) throws Exception {
        //Do your magic!!!
        
    }

    @Override
    public ModelApiResponse uploadFile(Long petId, String additionalMetadata, Http.MultipartFormData.FilePart file) throws Exception {
        //Do your magic!!!
        String accept = request().getHeader("Accept");
        if (accept != null && accept.contains("application/json")) {
            return mapper.readValue("", ModelApiResponse.class);
        }
        return new ModelApiResponse();
    }

}
