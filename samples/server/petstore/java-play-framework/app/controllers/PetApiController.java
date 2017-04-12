package controllers;

import java.io.InputStream;
import apimodels.Pet;

import play.mvc.Controller;
import play.mvc.Result;
import play.mvc.Http;
import java.util.List;
import java.util.ArrayList;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.JsonNode;
import com.google.inject.Inject;
import java.io.IOException;
import swagger.SwaggerUtils;
import com.fasterxml.jackson.core.type.TypeReference;

import javax.validation.constraints.*;

import swagger.SwaggerUtils.ApiAction;


public class PetApiController extends Controller {

    private PetApiControllerImp imp;
    private ObjectMapper mapper;

    @Inject
    private PetApiController(PetApiControllerImp imp) {
        this.imp = imp;
        mapper = new ObjectMapper();
    }


    @ApiAction
    public Result addPet() throws Exception {
        JsonNode nodebody = request().body().asJson();
        Pet body;
        if (nodebody != null) {
            body = mapper.readValue(nodebody.toString(), Pet.class);
        
        } else {
            body = null;
        }
        imp.addPet(body);
        
        return ok();
    }

    @ApiAction
    public Result deletePet(Long petId) throws Exception {
        String valueapiKey = request().getHeader("api_key");
        String apiKey;
        if (valueapiKey != null) {
            apiKey = (String)valueapiKey;
        
        } else {
            apiKey = "";
        }
        imp.deletePet(petId, apiKey);
        
        return ok();
    }

    @ApiAction
    public Result findPetsByStatus() throws Exception {
        //TODO: Maybe implement this in the future if we can support collection in the body params: see bug in swagger-play: https://github.com/swagger-api/swagger-play/issues/130
        //TODO: Tt seems it is not detected that it's a list based on the collectionFormat field?
        //WIP when both bugs will be fixed
        //List<Pair> statusPair = SwaggerUtils.parameterToPairs("multi", "status", request().getQueryString("status"));
        List<String> status = new ArrayList<String>();
        //for (Pair pair : statusPair) {
        //    status.add(pair.getValue());
        //}
        List<Pet> obj = imp.findPetsByStatus(status);
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
        
    }

    @ApiAction
    public Result findPetsByTags() throws Exception {
        //TODO: Maybe implement this in the future if we can support collection in the body params: see bug in swagger-play: https://github.com/swagger-api/swagger-play/issues/130
        //TODO: Tt seems it is not detected that it's a list based on the collectionFormat field?
        //WIP when both bugs will be fixed
        //List<Pair> tagsPair = SwaggerUtils.parameterToPairs("multi", "tags", request().getQueryString("tags"));
        List<String> tags = new ArrayList<String>();
        //for (Pair pair : tagsPair) {
        //    tags.add(pair.getValue());
        //}
        List<Pet> obj = imp.findPetsByTags(tags);
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
        
    }

    @ApiAction
    public Result getPetById(Long petId) throws Exception {
        Pet obj = imp.getPetById(petId);
        JsonNode result = mapper.valueToTree(obj);
        return ok(result);
        
    }

    @ApiAction
    public Result updatePet() throws Exception {
        JsonNode nodebody = request().body().asJson();
        Pet body;
        if (nodebody != null) {
            body = mapper.readValue(nodebody.toString(), Pet.class);
        
        } else {
            body = null;
        }
        imp.updatePet(body);
        
        return ok();
    }

    @ApiAction
    public Result updatePetWithForm(String petId) throws Exception {
        String valuename = ((String[]) request().body().asMultipartFormData().asFormUrlEncoded().get("name"))[0];
        String name;
        if (valuename != null) {
            name = (String)valuename;
        
        } else {
            name = "";
        }
        String valuestatus = ((String[]) request().body().asMultipartFormData().asFormUrlEncoded().get("status"))[0];
        String status;
        if (valuestatus != null) {
            status = (String)valuestatus;
        
        } else {
            status = "";
        }
        imp.updatePetWithForm(petId, name, status);
        
        return ok();
    }

    @ApiAction
    public Result uploadFile(Long petId) throws Exception {
        String valueadditionalMetadata = ((String[]) request().body().asMultipartFormData().asFormUrlEncoded().get("additionalMetadata"))[0];
        String additionalMetadata;
        if (valueadditionalMetadata != null) {
            additionalMetadata = (String)valueadditionalMetadata;
        
        } else {
            additionalMetadata = "";
        }
        Http.MultipartFormData.FilePart file = request().body().asMultipartFormData().getFile("file");
                imp.uploadFile(petId, additionalMetadata, file);
        
        return ok();
    }
}
