package controllers;

import java.io.InputStream;
import apimodels.ModelApiResponse;
import apimodels.Pet;

import play.mvc.Controller;
import play.mvc.Result;
import play.mvc.Http;
import java.util.List;
import java.util.Map;
import java.util.ArrayList;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.JsonNode;
import com.google.inject.Inject;
import java.io.IOException;
import swagger.SwaggerUtils;
import com.fasterxml.jackson.core.type.TypeReference;

import javax.validation.constraints.*;



public class PetApiController extends Controller {

    private final PetApiControllerImp imp;
    private final ObjectMapper mapper;

    @Inject
    private PetApiController(PetApiControllerImp imp) {
        this.imp = imp;
        mapper = new ObjectMapper();
    }


    
    public Result addPet() throws Exception {
        JsonNode nodebody = request().body().asJson();
        Pet body;

        body = mapper.readValue(nodebody.toString(), Pet.class);
        body.validate();

        imp.addPet(body);
        
        return ok();
        
    }

    
    public Result deletePet(Long petId) throws Exception {
        String valueapiKey = request().getHeader("api_key");
        String apiKey;
        if (valueapiKey != null) {
            apiKey = (String)valueapiKey;
        
        } else {
            apiKey = null;
        }
        imp.deletePet(petId, apiKey);
        
        return ok();
        
    }

    
    public Result findPetsByStatus() throws Exception {
        List<String> statusList = SwaggerUtils.parametersToList("csv", request().queryString().get("status"));
        List<String> status = new ArrayList<String>();
        for (String curParam : statusList) {
            //noinspection UseBulkOperation
            status.add(curParam);
        }
        List<Pet> obj = imp.findPetsByStatus(status);
        for (Pet curItem : obj) {
            curItem.validate();
        }
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
        
        
    }

    
    public Result findPetsByTags() throws Exception {
        List<String> tagsList = SwaggerUtils.parametersToList("csv", request().queryString().get("tags"));
        List<String> tags = new ArrayList<String>();
        for (String curParam : tagsList) {
            //noinspection UseBulkOperation
            tags.add(curParam);
        }
        List<Pet> obj = imp.findPetsByTags(tags);
        for (Pet curItem : obj) {
            curItem.validate();
        }
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
        
        
    }

    
    public Result getPetById(Long petId) throws Exception {
        Pet obj = imp.getPetById(petId);
        obj.validate();
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
        
        
    }

    
    public Result updatePet() throws Exception {
        JsonNode nodebody = request().body().asJson();
        Pet body;

        body = mapper.readValue(nodebody.toString(), Pet.class);
        body.validate();

        imp.updatePet(body);
        
        return ok();
        
    }

    
    public Result updatePetWithForm(Long petId) throws Exception {
        String valuename = (request().body().asMultipartFormData().asFormUrlEncoded().get("name"))[0];
        String name;
        if (valuename != null) {
            name = (String)valuename;
        
        } else {
            name = null;
        }
        String valuestatus = (request().body().asMultipartFormData().asFormUrlEncoded().get("status"))[0];
        String status;
        if (valuestatus != null) {
            status = (String)valuestatus;
        
        } else {
            status = null;
        }
        imp.updatePetWithForm(petId, name, status);
        
        return ok();
        
    }

    
    public Result uploadFile(Long petId) throws Exception {
        String valueadditionalMetadata = (request().body().asMultipartFormData().asFormUrlEncoded().get("additionalMetadata"))[0];
        String additionalMetadata;
        if (valueadditionalMetadata != null) {
            additionalMetadata = (String)valueadditionalMetadata;
        
        } else {
            additionalMetadata = null;
        }
        Http.MultipartFormData.FilePart file = request().body().asMultipartFormData().getFile("file");
                ModelApiResponse obj = imp.uploadFile(petId, additionalMetadata, file);
        obj.validate();
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
        
        
    }
}
